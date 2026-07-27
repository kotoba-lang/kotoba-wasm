(ns kotoba.wasm.core
  ;; See `kotoba.kir`'s ns form for why the whole `:require` clause
  ;; (not just an item inside it) is behind the reader-conditional.
  #?(:clj (:require [kotoba.wasm.typed :as typed]
                    [kotoba.kir.compatibility :as compatibility])
     :cljs (:require [kotoba.wasm.typed :as typed]
                     [kotoba.kir.compatibility :as compatibility]
                     [kotoba.kir.cljs-i64 :as i64])))

;; `uleb` only ever encodes small, non-negative, interpreter-internal counts
;; and indices in this file (section/payload lengths, function/type/import
;; indices) -- never an arbitrary `.kotoba` i64 VALUE -- so it stays plain
;; JS-number-based on both runtimes (`(long n)` was already a no-op cast on
;; :clj for values in this range; dropped for :cljs since cljs has no
;; `long`).
(defn- uleb [n]
  (loop [n #?(:clj (long n) :cljs n) out []]
    (let [b (bit-and n 0x7f) n' (unsigned-bit-shift-right n 7)]
      (if (zero? n') (conj out b) (recur n' (conj out (bit-or b 0x80)))))))

;; `sleb` DOES encode arbitrary `.kotoba` i64 literals (`emit-expr`'s
;; `i64.const` case, below) across the FULL signed 64-bit range, so this is
;; the highest-risk port in this file: cljs's own `bit-shift-right` throws
;; on bigint input ("Cannot mix BigInt and other types" -- confirmed live),
;; and even if it didn't, cljs bitwise ops are JS int32-coerced and would
;; silently truncate any constant outside +-2^31 -- a byte-level corruption
;; of the compiled artifact, not just a value-range check failing loudly
;; like `frontend`'s admission check does. The `:cljs` branch works over
;; bigint throughout via `cljs-i64`, using `i64/ashr` (see its own
;; docstring) in place of `bit-shift-right`.
(defn- sleb [n]
  #?(:clj
     (loop [n (long n) out []]
       (let [b (bit-and n 0x7f) n' (bit-shift-right n 7)
             done (or (and (= n' 0) (zero? (bit-and b 0x40)))
                      (and (= n' -1) (not (zero? (bit-and b 0x40)))))]
         (if done (conj out b) (recur n' (conj out (bit-or b 0x80))))))
     :cljs
     (loop [n (i64/->bigint n) out []]
       ;; cljs.core/bit-and coerces through its collection-oriented SCI
       ;; implementation under nbb and cannot accept JS bigint.  Modulo 128
       ;; yields the identical low seven two's-complement bits, including for
       ;; negative values, without leaving the exact-bigint domain.
       (let [b (js/Number (mod n (js/BigInt 0x80))) n' (i64/ashr n 7)
             done (or (and (= n' i64/zero) (zero? (bit-and b 0x40)))
                      (and (= n' (js/BigInt -1)) (not (zero? (bit-and b 0x40)))))]
         (if done (conj out b) (recur n' (conj out (bit-or b 0x80))))))))

(defn- encode-local-operands [tokens]
  (loop [remaining (seq tokens) encoded []]
    (if-not remaining
      encoded
      (let [token (first remaining)]
        (if (contains? #{::local-get ::local-set ::local-tee} token)
          (let [index (second remaining)]
            (when-not (and (integer? index) (<= 0 index))
              (throw (ex-info "invalid Wasm local index"
                              {:phase :wasm-local-encoding
                               :operation token :index index})))
            (recur (nnext remaining)
                   (into encoded
                         (concat [(case token
                                    ::local-get 0x20
                                    ::local-set 0x21
                                    ::local-tee 0x22)]
                                 (uleb index)))))
          (recur (next remaining) (conj encoded token)))))))

(defn- section [id payload] (into [id] (concat (uleb (count payload)) payload)))
(defn- utf8 [s]
  #?(:clj (mapv #(bit-and (int %) 0xff) (.getBytes ^String s "UTF-8"))
     :cljs (vec (js/Array.from (.encode (js/TextEncoder.) s)))))
(defn- name-bytes [s] (let [bs (utf8 s)] (into (uleb (count bs)) bs)))

(def compatibility-section-name "kotoba.compatibility")

(defn- wasm-runtime [target]
  (case target
    :wasm32-browser-kotoba-v1 :kotoba-browser-host-v1
    :wasm32-wasi-kotoba-v1 :kotoba-wasi-host-v1
    :kotoba-capability-host-v1))

(defn- identity-text [value]
  (if-let [ns (namespace value)] (str ns "/" (name value)) (name value)))

(defn- compatibility-bytes [kir target]
  (let [fields [compatibility/compiler-version
                (identity-text compatibility/language-version)
                (identity-text (:format kir))
                (identity-text target)
                (identity-text (wasm-runtime target))
                (identity-text (cond
                                 (some (fn [{:keys [param-types result]}]
                                         (or (some #{:f32} param-types) (= :f32 result)))
                                       (:functions kir))
                                 :kotoba.typed/mixed-f32-f64-v3
                                 (some (fn [{:keys [param-types result]}]
                                         (or (some #{:f64} param-types) (= :f64 result)))
                                       (:functions kir))
                                 :kotoba.typed/mixed-f64-v2
                                 (= :kotoba.kir/v4 (:format kir)) :kotoba.typed/externref-v1
                                 :else :kotoba.i64/direct-v1))
                (identity-text compatibility/tender-role)
                (identity-text :kotoba.capability-host/v1)]]
    (vec (concat [1] (mapcat name-bytes fields)))))

(defn- local-count [form]
  (if-not (seq? form)
    0
    (let [[op & args] form]
      (if (= op 'let)
        (let [[bindings body] args]
          (+ (quot (count bindings) 2)
             (reduce + (map local-count (take-nth 2 (rest bindings))))
             (local-count body)))
        (reduce + (map local-count args))))))

(declare emit-expr)

(defn- emit-many [forms env ctx]
  (mapcat #(emit-expr % env ctx) forms))

(defn emit-expr [form env {:keys [function-indices intrinsic-indices next-local] :as ctx}]
  (cond
    ;; A literal here may be a bigint (from a `.kotoba` source literal, or
    ;; from `kotoba.kir`'s coercion once it passes through there)
    ;; or a plain number (synthesized directly by `kotoba.compiler.frontend`
    ;; -- e.g. `when`'s trailing `0`); `sleb` above accepts either.
    #?(:clj (integer? form) :cljs (or (i64/bigint-value? form) (integer? form)))
    (into [0x42] (sleb form))                                    ; i64.const
    (symbol? form) [::local-get (get env form)]                         ; local.get
    :else
    (let [[op & args] form]
      (cond
        (= op 'let)
        (let [[bindings body] args]
          (loop [pairs (partition 2 bindings) env env out [] cursor next-local]
            (if-let [[name value] (first pairs)]
              (let [value-code (emit-expr value env (assoc ctx :next-local cursor))]
                (recur (next pairs) (assoc env name cursor)
                       (into out (concat value-code [::local-set cursor])) (inc cursor))) ; local.set
              (into out (emit-expr body env (assoc ctx :next-local cursor))))))

        (= op 'if)
        (let [[test then else] args]
          (concat (emit-expr test env ctx)
                  [0x50 0x45 0x04 0x7e]                         ; i64.eqz;i32.eqz;if i64
                  (emit-expr then env ctx) [0x05]
                  (emit-expr else env ctx) [0x0b]))

        ;; `do`: emit each subexpression in order; drop all but the last value
        ;; from the stack (0x1a = drop). Side effects run once, in order.
        (= op 'do)
        (let [n (count args)]
          (mapcat (fn [i arg]
                    (concat (emit-expr arg env ctx) (when (< i (dec n)) [0x1a])))
                  (range n) args))

        (= op 'cap-call)
        (let [[cap-id value] args]
          (concat [0x42] (sleb cap-id) (emit-expr value env ctx)
                  [0x10 (get intrinsic-indices 'cap-call)]))

        (= op 'typed-cap-call)
        (let [[cap-id _request-type _result-type request] args
              typed-import (get intrinsic-indices [:capability cap-id])]
          (if typed-import
            (concat (emit-expr request env ctx) [0x10 typed-import])
            ;; This path is intentionally unbindable by Component packaging;
            ;; it retains the ordinary core-module fallback for callers that
            ;; emit a typed KIR without a named capability-import table.
            (concat [0x41] (sleb cap-id)
                    (emit-expr request env ctx)
                    [0x10 (get intrinsic-indices 'typed-cap-call)])))

        (contains? '#{pair pair-first pair-second} op)
        (concat (emit-many args env ctx) [0x10 (get intrinsic-indices op)])

        (contains? '#{+ - * quot bit-xor bit-and bit-or} op)
        (let [opcode ({'+ 0x7c '- 0x7d '* 0x7e 'quot 0x7f
                       'bit-and 0x83 'bit-xor 0x85 'bit-or 0x84} op)]
          (if (and (= op '-) (= 1 (count args)))
            (concat [0x42 0] (emit-expr (first args) env ctx) [0x7d])
            (concat (emit-expr (first args) env ctx)
                    (mapcat #(concat (emit-expr % env ctx) [opcode]) (rest args)))))

        (= op 'i32-wrap)
        (concat (emit-expr (first args) env ctx) [0xa7 0xac])

        (= op 'u32-wrap)
        (concat (emit-expr (first args) env ctx) [0xa7 0xad])

        ;; Component adapters may receive a Canonical discriminant as i32
        ;; while reusing the ordinary i64 expression emitter. This internal
        ;; bridge widens that already-on-stack i32 without changing its bits.
        (= op 'i64-extend-i32-u)
        (concat (emit-expr (first args) env ctx) [0xad])

        (contains? '#{i32-wrapping-add i32-wrapping-mul i32-xor} op)
        (concat (emit-expr (first args) env ctx) [0xa7]
                (emit-expr (second args) env ctx) [0xa7]
                [({'i32-wrapping-add 0x6a 'i32-wrapping-mul 0x6c 'i32-xor 0x73} op)
                 0xac])

        ;; ADR-2607254600 D1. Operands are already i64, so unlike the i32
        ;; shifts above there is no wrap/extend around the opcode.
        (contains? '#{i64-shift-left i64-shift-right u64-shift-right} op)
        (concat (emit-expr (first args) env ctx)
                (emit-expr (second args) env ctx)
                [({'i64-shift-left 0x86 'i64-shift-right 0x87 'u64-shift-right 0x88} op)])

        ;; ADR-2607254600 D2. Wasm has no i64.not; xor with all-ones is the
        ;; canonical encoding. `0x42 0x7f` is i64.const -1 (SLEB128).
        (= op 'bit-not)
        (concat (emit-expr (first args) env ctx) [0x42 0x7f 0x85])

        (contains? '#{i32-shift-left i32-shift-right u32-shift-right} op)
        (concat (emit-expr (first args) env ctx) [0xa7]
                (emit-expr (second args) env ctx) [0xa7]
                [({'i32-shift-left 0x74 'i32-shift-right 0x75 'u32-shift-right 0x76} op)
                 (if (= op 'u32-shift-right) 0xad 0xac)])

        (contains? '#{= < > <= >=} op)
        (concat (emit-many args env ctx)
                [({'= 0x51 '< 0x53 '> 0x55 '<= 0x57 '>= 0x59} op)
                 0xad])                                          ; extend i32 result to i64

        :else
        (concat (emit-many args env ctx) [0x10 (get function-indices op)]))))) ; call

(defn- i32-const [value] (into [0x41] (sleb value)))

(defn- typed-function-signatures [functions]
  (into {} (map (fn [function] [(:name function) function]) functions)))

(defn- typed-function-type [{:keys [param-types result]}]
  (concat [0x60] (uleb (count param-types)) (map typed/wasm-type param-types)
          [1 (typed/wasm-type result)]))

(defn- exact-i64 [text]
  #?(:clj (Long/parseLong text) :cljs (js/BigInt text)))

(defn- f64-constant [bits]
  (list 'f64-from-bits (if (string? bits) (exact-i64 bits) bits)))

(defn- f64-horner [z coefficient-bits]
  ;; Preserve Horner's exact operation order without constructing a deeply
  ;; nested expression. The typed emitter lowers `let` bindings iteratively,
  ;; so large fixed polynomials do not depend on the host JavaScript stack.
  (loop [remaining (rest coefficient-bits)
         previous (f64-constant (first coefficient-bits))
         index 0
         bindings []]
    (if-let [bits (first remaining)]
      (let [name (symbol (str "horner-step-" index))]
        (recur (next remaining)
               name
               (inc index)
               (conj bindings name
                     (list 'f64-add (f64-constant bits)
                           (list 'f64-mul z previous)))))
      (list 'let bindings previous))))

(defn- bounded-sin-form [x]
  (let [z (list 'f64-mul x x)
        p (f64-horner z ["4389130328383466826" "-4797767418267846625"
                         "4460272573143870729" "-4730215272828025628"
                         "4523617214285662004" "-4671919876300759014"
                         "4575957461383581969" "-4628199217061079723"])]
    (list 'f64-add x (list 'f64-mul (list 'f64-mul x z) p))))

(defn- bounded-cos-form [x]
  (let [z (list 'f64-mul x x)
        p (f64-horner z ["4407590220077447199" "-4780226356266894179"
                         "4477122120089393304" "-4714566979057836196"
                         "4537941361671905306" "-4659324094485795817"
                         "4586165620538955093" "-4620693217682128896"])]
    (list 'f64-add (f64-constant "4607182418800017408") (list 'f64-mul z p))))

(defn- bounded-exp-form [x]
  (let [p (f64-horner x ["4370323941990432151" "4389130328383466826"
                          "4407590220077447199" "4425604618586929183"
                          "4443145680587881629" "4460272573143870729"
                          "4477122120089393304" "4493156764026750180"
                          "4508805057796939612" "4523617214285662004"
                          "4537941361671905306" "4551452160554016794"
                          "4564047942368979991" "4575957461383581969"
                          "4586165620538955093" "4595172819793696085"
                          "4602678819172646912" "4607182418800017408"])]
    (list 'f64-add (f64-constant "4607182418800017408") (list 'f64-mul x p))))

(defn- bounded-log-form [x]
  (let [one (f64-constant "4607182418800017408")
        y (list 'f64-div (list 'f64-sub x one) (list 'f64-add x one))
        z (list 'f64-mul y y)
        p (f64-horner z ["4587023449039406616" "4587745830934523688"
                          "4588638185040256542" "4589468260265693457"
                          "4590207312512236308" "4591215111030249286"
                          "4592670820000712476" "4594314991293244562"
                          "4596373779694328218" "4599676419421066581"
                          "4607182418800017408"])]
    (list 'f64-mul (list 'f64-mul (f64-constant "4611686018427387904") y) p)))

(defn- bounded-atan-unit-form [value]
  (let [one (f64-constant "4607182418800017408")
        threshold? (list '<= (list 'f64-to-bits value) (exact-i64 "4601133428098527028"))
        t (list 'if threshold?
                value
                (list 'f64-div (list 'f64-sub value one) (list 'f64-add value one)))]
    (list 'let ['t t
                'z (list 'f64-mul 't 't)
                'p (f64-horner 'z ["-4640324292980923366" "4583447231574686416"
                                   "-4639479661842017251" "4584391475231203080"
                                   "-4638562338784276348" "4585130310279789462"
                                   "-4637873616260616344" "4585925428558828667"
                                   "-4636945338076552860" "4587023449039406616"
                                   "-4635626205920252120" "4588638185040256542"
                                   "-4633903776589082351" "4590207312512236308"
                                   "-4632156925824526522" "4592670820000712476"
                                   "-4629057045561531246" "4596373779694328218"
                                   "-4623695617433709227" "4607182418800017408"])
                'a (list 'f64-mul 't 'p)]
          (list 'if threshold?
                'a
                (list 'f64-add (f64-constant "4605249457297304856") 'a)))))

(defn- bounded-atan2-form [y x]
  (let [pi (f64-constant "4614256656552045848")
        half-pi (f64-constant "4609753056924675352")
        negative? (fn [v] (list '< (list 'f64-to-bits v) 0))
        zero? (fn [v] (list '= (list 'bit-and (list 'f64-to-bits v)
                                      (exact-i64 "9223372036854775807")) 0))]
    (list 'if (zero? y)
          (list 'if (negative? x)
                (list 'if (negative? y) (list 'f64-neg pi) pi)
                y)
          (list 'if (zero? x)
                (list 'if (negative? y) (list 'f64-neg half-pi) half-pi)
                (list 'let ['ay (list 'f64-abs y)
                            'ax (list 'f64-abs x)
                            'swap (list '> (list 'f64-to-bits 'ay) (list 'f64-to-bits 'ax))
                            'ratio (list 'if 'swap
                                         (list 'f64-div 'ax 'ay)
                                         (list 'f64-div 'ay 'ax))
                            'base (bounded-atan-unit-form 'ratio)
                            'angle1 (list 'if 'swap
                                          (list 'f64-sub half-pi 'base)
                                          'base)
                            'angle2 (list 'if (negative? x)
                                          (list 'f64-sub pi 'angle1)
                                          'angle1)]
                      (list 'if (negative? y) (list 'f64-neg 'angle2) 'angle2))))))

(defn- wide-exp-form [value]
  (let [half (f64-constant "4602678819172646912")]
    (list 'let ['scaled (list 'f64-mul value (f64-constant "4609176140021203710"))
                'exponent (list 'if (list '< (list 'f64-to-bits 'scaled) 0)
                                (list 'f64-to-i64-truncating (list 'f64-sub 'scaled half))
                                (list 'f64-to-i64-truncating (list 'f64-add 'scaled half)))
                'exponent-f64 (list 'i64-to-f64-checked 'exponent)
                'reduced (list 'f64-sub
                               (list 'f64-sub value
                                     (list 'f64-mul 'exponent-f64
                                           (f64-constant "4604418534313441775")))
                               (list 'f64-mul 'exponent-f64
                                     (f64-constant "4358002977218854975")))
                'scale-bits (list '* (list '+ 'exponent 1023)
                                  (exact-i64 "4503599627370496"))
                'scale (list 'f64-from-bits 'scale-bits)]
          (list 'f64-mul (bounded-exp-form 'reduced) 'scale))))

(defn- wide-log-form [value]
  (let [unit (exact-i64 "4503599627370496")
        mask (exact-i64 "4503599627370495")
        one-bits (exact-i64 "4607182418800017408")
        one-and-half-bits (exact-i64 "4609434218613702656")]
    (list 'let ['bits (list 'f64-to-bits value)
                'exponent0 (list '- (list 'quot 'bits unit) 1023)
                'mantissa0 (list 'f64-from-bits
                                 (list '+ (list 'bit-and 'bits mask) one-bits))
                'high (list '> (list 'f64-to-bits 'mantissa0) one-and-half-bits)
                'mantissa (list 'if 'high
                                (list 'f64-mul 'mantissa0
                                      (f64-constant "4602678819172646912"))
                                'mantissa0)
                'exponent (list 'if 'high (list '+ 'exponent0 1) 'exponent0)
                'exponent-f64 (list 'i64-to-f64-checked 'exponent)
                'kernel (bounded-log-form 'mantissa)]
          (list 'f64-add
                (list 'f64-add 'kernel
                      (list 'f64-mul 'exponent-f64
                            (f64-constant "4604418534313441775")))
                (list 'f64-mul 'exponent-f64
                      (f64-constant "4358002977218854975"))))))

(defn- emit-option-record-capability-project*
  [op args env emit* allocate! intrinsic-indices]
  (let [[cap-id request-values fallback result-size result-alignment
         payload-offset bool-offsets field-offset] args
        field-type
        ({'component-option-record-capability-project-i64 :i64
          'component-option-record-capability-project-f32 :f32
          'component-option-record-capability-project-f64 :f64
          'component-option-record-capability-project-bool :bool}
         op)
        typed-import (get intrinsic-indices [:capability cap-id])
        realloc-index (get intrinsic-indices :component-realloc)
        _ (when-not (and (vector? request-values) (seq request-values))
            (throw
             (ex-info "component record request must have flat values"
                      {:phase :wasm-component-record-capability-lowering})))
        _ (when-not typed-import
            (throw
             (ex-info "component aggregate capability requires a named import"
                      {:phase :wasm-component-record-capability-lowering
                       :capability cap-id})))
        _ (when-not realloc-index
            (throw
             (ex-info "component aggregate capability requires canonical realloc"
                      {:phase :wasm-component-record-capability-lowering})))
        _ (when-not
           (and (integer? result-size) (pos? result-size)
                (integer? result-alignment)
                (contains? #{1 2 4 8} result-alignment)
                (integer? payload-offset) (<= 0 payload-offset)
                (vector? bool-offsets)
                (every? #(and (integer? %) (<= 0 %) (< % result-size))
                        bool-offsets)
                (integer? field-offset) (<= 0 field-offset)
                (contains? #{:i64 :f32 :f64 :bool} field-type))
            (throw
             (ex-info "component record result layout is invalid"
                      {:phase :wasm-component-record-capability-lowering
                       :result-size result-size
                       :result-alignment result-alignment
                       :payload-offset payload-offset
                       :bool-offsets bool-offsets
                       :field-offset field-offset
                       :field-type field-type})))
        field-width ({:i64 8 :f64 8 :f32 4 :bool 1} field-type)
        _ (when (> (+ field-offset field-width) result-size)
            (throw
             (ex-info "component record projected field exceeds result area"
                      {:phase :wasm-component-record-capability-lowering
                       :field-offset field-offset :field-width field-width
                       :result-size result-size})))
        result-local (allocate! 0x7f)
        result-end-local (allocate! 0x7f)
        disc-local (allocate! 0x7f)
        load-field
        (case field-type
          :i64 (concat [::local-get result-local 0x29 0x03]
                       (uleb field-offset))
          :f64 (concat [::local-get result-local 0x2b 0x03]
                       (uleb field-offset))
          :f32 (concat [::local-get result-local 0x2a 0x02]
                       (uleb field-offset))
          :bool (concat [::local-get result-local 0x2d 0x00]
                        (uleb field-offset)))]
    (concat
     (i32-const 0) (i32-const 0)
     (i32-const result-alignment) (i32-const result-size)
     [0x10 realloc-index ::local-set result-local]
     (i32-const 1)
     (mapcat #(emit* % env) request-values)
     [::local-get result-local 0x10 typed-import]
     [::local-get result-local 0x41] (sleb (dec result-alignment))
     [0x71 0x45 0x04 0x40 0x05 0x00 0x0b
      ::local-get result-local 0x41] (sleb result-size)
     [0x6a ::local-set result-end-local
      ::local-get result-end-local ::local-get result-local
      0x49 0x04 0x40 0x00 0x0b
      ::local-get result-end-local 0x3f 0x00
      0x41 16 0x74 0x4b 0x04 0x40 0x00 0x0b
      ::local-get result-local 0x2d 0x00 0x00
      ::local-set disc-local
      ::local-get disc-local 0x41 1 0x4b
      0x04 0x40 0x00 0x0b
      ::local-get disc-local
      0x04] [({:i64 0x7e :f64 0x7c :f32 0x7d :bool 0x7f} field-type)]
     (mapcat (fn [offset]
               (concat [::local-get result-local 0x2d 0x00]
                       (uleb offset)
                       [0x41 1 0x4b 0x04 0x40 0x00 0x0b]))
             bool-offsets)
     load-field
     (when (= :bool field-type)
       [0x22 disc-local 0x41 1 0x4b
        0x04 0x40 0x00 0x0b
        ::local-get disc-local])
     [0x05]
     (emit* fallback env)
     [0x0b])))

(def ^:private aggregate-capability-ops
  '#{component-option-list-capability-count
     component-result-list-capability-count
     component-option-record-capability-project-i64
     component-option-record-capability-project-f32
     component-option-record-capability-project-f64
     component-option-record-capability-project-bool})

(defn- emit-aggregate-capability*
  [op args env emit* allocate! intrinsic-indices option-list result-list]
  (case op
    component-option-list-capability-count (option-list args env)
    component-result-list-capability-count (result-list args env)
    (emit-option-record-capability-project*
     op args env emit* allocate! intrinsic-indices)))

(defn- parse-recursive-item-plan
  "Parse the closed prefix encoding used by component item-validation kind 6.
  Nodes are: 0 scalar/empty, 1 bool, 2 string(max-bytes),
  3 record(field-count, offset+node...), 4 union(case-count,payload-offset,
  node...), and 5 list(max-items,stride,alignment,node)."
  [words]
  (let [seen (atom 0)]
    (letfn [(parse-node [index depth]
              (let [kind (get words index)]
                (when (and (integer? kind)
                           (< depth 32)
                           (<= (swap! seen inc) 1024))
                  (case kind
                    0 [{:kind :scalar} (inc index)]
                    1 [{:kind :bool} (inc index)]
                    2 (let [maximum (get words (inc index))]
                        (when (and (integer? maximum)
                                   (<= 0 maximum 0x7fffffff))
                          [{:kind :string :maximum maximum} (+ index 2)]))
                    3 (let [field-count (get words (inc index))]
                        (when (and (integer? field-count)
                                   (<= 1 field-count 256))
                          (loop [remaining field-count
                                 cursor (+ index 2)
                                 fields []]
                            (if (zero? remaining)
                              [{:kind :record :fields fields} cursor]
                              (let [offset (get words cursor)
                                    parsed (when (and (integer? offset)
                                                      (<= 0 offset 0x7fffffff))
                                             (parse-node (inc cursor)
                                                         (inc depth)))]
                                (when parsed
                                  (recur (dec remaining) (second parsed)
                                         (conj fields
                                               {:offset offset
                                                :node (first parsed)}))))))))
                    4 (let [case-count (get words (inc index))
                            payload-offset (get words (+ index 2))]
                        (when (and (integer? case-count)
                                   (<= 1 case-count 256)
                                   (integer? payload-offset)
                                   (<= 0 payload-offset 0x7fffffff))
                          (loop [remaining case-count
                                 cursor (+ index 3)
                                 cases []]
                            (if (zero? remaining)
                              [{:kind :union
                                :payload-offset payload-offset
                                :cases cases}
                               cursor]
                              (when-let [parsed
                                         (parse-node cursor (inc depth))]
                                (recur (dec remaining) (second parsed)
                                       (conj cases (first parsed))))))))
                    5 (let [maximum (get words (inc index))
                            stride (get words (+ index 2))
                            alignment (get words (+ index 3))]
                        (when (and (integer? maximum)
                                   (<= 0 maximum 0x7fffffff)
                                   (integer? stride)
                                   (<= 1 stride 0x7fffffff)
                                   (<= (* maximum stride) 0xffffffff)
                                   (integer? alignment)
                                   (pos? alignment)
                                   (zero? (bit-and alignment (dec alignment)))
                                   (<= alignment 0x40000000))
                          (when-let [parsed
                                     (parse-node (+ index 4) (inc depth))]
                            [{:kind :list
                              :maximum maximum
                              :stride stride
                              :alignment alignment
                              :item (first parsed)}
                             (second parsed)])))
                    nil))))]
      (when-let [[node cursor] (parse-node 0 0)]
        (when (= cursor (count words)) node)))))

(defn- recursive-item-plan-fits?
  "Prove every fixed offset/load in a recursive plan stays within its parent
  Canonical layout. Runtime pointer ranges are checked separately."
  [node available]
  (case (:kind node)
    :scalar true
    :bool (<= 1 available)
    :string (<= 8 available)
    :record
    (every? (fn [{:keys [offset node]}]
              (and (<= offset available)
                   (recursive-item-plan-fits? node (- available offset))))
            (:fields node))
    :union
    (and (<= 1 available)
         (<= (:payload-offset node) available)
         (every? #(recursive-item-plan-fits?
                   % (- available (:payload-offset node)))
                 (:cases node)))
    :list
    (and (<= 8 available)
         (recursive-item-plan-fits? (:item node) (:stride node)))
    false))

(defn- emit-valid-utf8
  "Emit a strict scalar-value UTF-8 scan for one already range-checked byte
  slice. Rejects stray/truncated continuation bytes, overlong encodings,
  UTF-16 surrogates, and values above U+10FFFF."
  [pointer-local length-local allocate!]
  (let [index-local (allocate! 0x7f)
        next-local (allocate! 0x7f)
        lead-local (allocate! 0x7f)
        second-local (allocate! 0x7f)
        width-local (allocate! 0x7f)
        load-byte
        (fn [offset]
          (concat
           [::local-get pointer-local
            ::local-get index-local
            0x6a]
           (when (pos? offset)
             (concat (i32-const offset) [0x6a]))
           [0x2d 0x00 0x00]))
        in-range
        (fn [local minimum maximum]
          (concat
           [::local-get local 0x41] (sleb minimum)
           [0x4f
            ::local-get local 0x41] (sleb maximum)
           [0x4d 0x71]))
        set-width-when
        (fn [minimum maximum width]
          (concat
           (in-range lead-local minimum maximum)
           [0x04 0x40]
           (i32-const width)
           [::local-set width-local 0x0b]))
        continuation
        (fn [offset]
          (concat
           [::local-get width-local 0x41] (sleb offset)
           [0x4b 0x04 0x40]
           (load-byte offset)
           (when (= offset 1) [::local-tee second-local])
           [0x41] (sleb 0x80)
           [0x4f]
           (load-byte offset)
           [0x41] (sleb 0xbf)
           [0x4d 0x71
            0x45 0x04 0x40 0x00 0x0b
            0x0b]))
        special-second
        (fn [lead comparison boundary]
          (concat
           [::local-get lead-local 0x41] (sleb lead)
           [0x46 0x04 0x40
            ::local-get second-local
            0x41] (sleb boundary)
           [comparison
            0x45 0x04 0x40 0x00 0x0b
            0x0b]))]
    (concat
     (i32-const 0) [::local-set index-local]
     [0x02 0x40
      0x03 0x40
      ::local-get index-local
      ::local-get length-local
      0x4f
      0x0d 0x01]
     (load-byte 0)
     [::local-set lead-local
      ::local-get lead-local
      0x41] (sleb 0x80)
     [0x49
      0x04 0x40]
     (i32-const 1)
     [::local-set width-local
      0x05]
     (i32-const 0)
     [::local-set width-local]
     (set-width-when 0xc2 0xdf 2)
     (set-width-when 0xe0 0xef 3)
     (set-width-when 0xf0 0xf4 4)
     [::local-get width-local
      0x45 0x04 0x40 0x00 0x0b
      0x0b
      ::local-get index-local
      ::local-get width-local
      0x6a
      ::local-set next-local
      ::local-get next-local
      ::local-get index-local
      0x49 0x04 0x40 0x00 0x0b
      ::local-get next-local
      ::local-get length-local
      0x4b 0x04 0x40 0x00 0x0b]
     (continuation 1)
     (continuation 2)
     (continuation 3)
     ;; Boundary rules exclude overlong triples/quads, surrogate code points,
     ;; and scalar values above U+10FFFF.
     (special-second 0xe0 0x4f 0xa0)
     (special-second 0xed 0x4d 0x9f)
     (special-second 0xf0 0x4f 0x90)
     (special-second 0xf4 0x4d 0x8f)
     [::local-get next-local
      ::local-set index-local
      0x0c 0x00
      0x0b 0x0b])))

(defn- emit-typed-function-body
  [function function-indices intrinsic-indices descriptor-indices literal-indices signatures
  {:keys [component-canonical-scalars? component-unchecked-bool-params]}]
  (let [locals (volatile! [])
        param-count (count (:params function))
        unchecked-bool-param-indices
        (if (map? component-unchecked-bool-params)
          (get component-unchecked-bool-params (:name function) #{})
          (or component-unchecked-bool-params #{}))
        wasm-type (fn [type]
                    (if (and component-canonical-scalars? (= :bool type))
                      0x7f
                      (typed/wasm-type type)))
        reference-type? (fn [type]
                          (and (not (and component-canonical-scalars? (= :bool type)))
                               (typed/reference-type? type)))
        allocate! (fn [wasm-type]
                    (let [index (+ param-count (count @locals))]
                      (vswap! locals conj wasm-type)
                      index))
        descriptor-id (fn [type]
                        (or (get descriptor-indices type)
                            (throw (ex-info "typed Wasm descriptor is not sealed"
                                            {:phase :wasm-typed-lowering :descriptor type}))))
        scalar-suffix (fn [type]
                        (case type :i64 "i64" :f64 "f64" :f32 "f32" "ref"))
        env (into {} (map-indexed (fn [index [name type]]
                                    [name {:index index :type type}])
                                  (map vector (:params function) (:param-types function))))]
    (letfn [(emit-builder [type tag item-forms item-types env]
              (let [initial (concat (i32-const (descriptor-id type)) (i32-const tag)
                                    [0x10 (get intrinsic-indices 'typed-new)])
                    pushed (reduce (fn [code [item item-type]]
                                     (concat code (emit* item env)
                                             [0x10 (get intrinsic-indices
                                                        (symbol (str "typed-push-"
                                                                     (scalar-suffix item-type))))]))
                                   initial (map vector item-forms item-types))]
                (concat (i32-const (descriptor-id type)) pushed
                        [0x10 (get intrinsic-indices 'typed-seal)])))
            (emit-get [type value-form index item-type env]
              (concat (i32-const (descriptor-id type)) (emit* value-form env)
                      (i32-const index)
                      [0x10 (get intrinsic-indices
                                 (symbol (str "typed-get-" (scalar-suffix item-type))))]))
            (emit-bool [code]
              (if component-canonical-scalars?
                code
                (concat code [0x10 (get intrinsic-indices 'typed-bool)])))
            (emit-equal [type left right env]
              (concat (i32-const (descriptor-id type))
                      (emit* left env) (emit* right env)
                      [0x10 (get intrinsic-indices 'typed-equal) 0xad]))
            (emit-test [form env]
              (let [type (typed/infer-type
                          form
                          (into {} (map (fn [[key item]] [key (:type item)]) env))
                          signatures)]
                (case type
                  :i64 (concat (emit* form env) [0x50 0x45])
                  :bool (if component-canonical-scalars?
                          (emit* form env)
                          (concat (i32-const (descriptor-id :bool))
                                  (emit* form env)
                                  [0x10 (get intrinsic-indices 'typed-tag)]))
                  (throw (ex-info "typed Wasm condition must be bool or i64"
                                  {:phase :wasm-typed-lowering
                                   :type type :form form})))))
            (emit-component-list-validation
              [pointer count max-items stride alignment env]
              (when-not (and (integer? max-items)
                             (<= 0 max-items 0x7fffffff)
                             (integer? stride)
                             (<= 1 stride 0x7fffffff)
                             (<= (* max-items stride) 0xffffffff)
                             (integer? alignment)
                             (pos? alignment)
                             (zero? (bit-and alignment (dec alignment)))
                             (<= alignment 0x40000000))
                (throw
                 (ex-info "component list layout is invalid"
                          {:phase :wasm-component-list-lowering
                           :max-items max-items
                           :stride stride
                           :alignment alignment})))
              (let [pointer-local (allocate! 0x7f)
                    count-local (allocate! 0x7f)
                    bytes-local (allocate! 0x7f)
                    end-local (allocate! 0x7f)]
                {:pointer-local pointer-local
                 :count-local count-local
                 :code
                 (concat
                  (emit* pointer env) [::local-set pointer-local]
                  (emit* count env) [::local-set count-local]
                  ;; Canonical list pointers are aligned even when the
                  ;; selected list is empty.
                  [::local-get pointer-local 0x41] (sleb (dec alignment))
                  [0x71 0x45 0x04 0x40 0x05 0x00 0x0b]
                  ;; The item count is unsigned and descriptor-bounded.
                  [::local-get count-local 0x41] (sleb max-items)
                  [0x4b 0x04 0x40 0x00 0x0b]
                  ;; bytes = count * stride. Reject unsigned multiply
                  ;; overflow independently of the configured bound.
                  [::local-get count-local 0x41] (sleb stride)
                  [0x6c ::local-set bytes-local
                   ::local-get count-local 0x45 0x04 0x40 0x05
                   ::local-get bytes-local 0x41] (sleb stride)
                  [0x6e ::local-get count-local 0x47
                   0x04 0x40 0x00 0x0b 0x0b]
                  ;; end = ptr + bytes, rejecting unsigned wrap and a range
                  ;; beyond this module's actual linear memory.
                  [::local-get pointer-local ::local-get bytes-local
                   0x6a ::local-set end-local
                   ::local-get end-local ::local-get pointer-local
                   0x49 0x04 0x40 0x00 0x0b
                   ::local-get end-local 0x3f 0x00
                   0x41 16 0x74 0x4b 0x04 0x40 0x00 0x0b])}))
            (emit-component-list-item-validation
              [list-validation stride item-kind item-validation-args]
              (when (and item-kind (not= 0 item-kind))
                (let [max-total-bytes (first item-validation-args)
                      bool-count (first item-validation-args)
                      bool-offsets (vec (rest item-validation-args))
                      nested-total (first item-validation-args)
                      nested-depth (second item-validation-args)
                      nested-layout-words (vec (drop 2 item-validation-args))
                      nested-layouts
                      (when (and (integer? nested-depth)
                                 (= (* 2 nested-depth)
                                    (count nested-layout-words)))
                        (mapv vec (partition 2 nested-layout-words)))
                      union-case-count (first item-validation-args)
                      union-payload-offset (second item-validation-args)
                      union-total (nth item-validation-args 2 nil)
                      union-case-words (vec (drop 3 item-validation-args))
                      union-cases
                      (when (and (integer? union-case-count)
                                 (= (* 2 union-case-count)
                                    (count union-case-words)))
                        (mapv vec (partition 2 union-case-words)))
                      recursive-byte-total (first item-validation-args)
                      recursive-item-total (second item-validation-args)
                      recursive-plan
                      (when (and (integer? recursive-byte-total)
                                 (<= 0 recursive-byte-total 0x7fffffff)
                                 (integer? recursive-item-total)
                                 (<= 0 recursive-item-total 0x7fffffff))
                        (parse-recursive-item-plan
                         (vec (drop 2 item-validation-args))))]
                 (when-not
                  (case item-kind
                    1 (and (= 1 (count item-validation-args))
                           (integer? max-total-bytes)
                           (<= 0 max-total-bytes 0x7fffffff)
                           (<= 8 stride))
                    2 (and (= [0] item-validation-args)
                           (<= 1 stride))
                    3 (and (integer? bool-count)
                           (= bool-count (count bool-offsets))
                           (pos? bool-count)
                           (= bool-count (count (distinct bool-offsets)))
                           (every? #(and (integer? %)
                                         (<= 0 %)
                                         (< % stride))
                                   bool-offsets))
                    4 (and (integer? nested-total)
                           (<= 0 nested-total 0x7fffffff)
                           (integer? nested-depth)
                           (<= 1 nested-depth 32)
                           nested-layouts
                           (every?
                            (fn [[item-stride item-alignment]]
                              (and (integer? item-stride)
                                   (<= 1 item-stride 0x7fffffff)
                                   (<= (* nested-total item-stride)
                                       0xffffffff)
                                   (integer? item-alignment)
                                   (pos? item-alignment)
                                   (zero? (bit-and item-alignment
                                                   (dec item-alignment)))
                                   (<= item-alignment 0x40000000)))
                            nested-layouts))
                    5 (and (integer? union-case-count)
                           (<= 1 union-case-count 256)
                           (integer? union-payload-offset)
                           (<= 0 union-payload-offset)
                           (< union-payload-offset stride)
                           (integer? union-total)
                           (<= 0 union-total 0x7fffffff)
                           union-cases
                           (every?
                            (fn [[case-kind case-max]]
                              (and (integer? case-kind)
                                   (<= 0 case-kind 2)
                                   (integer? case-max)
                                   (case case-kind
                                     0 (zero? case-max)
                                     1 (and (<= 0 case-max union-total)
                                            (<= (+ union-payload-offset 8)
                                                stride))
                                     2 (and (zero? case-max)
                                            (< union-payload-offset stride))
                                     false)))
                            union-cases))
                    6 (and recursive-plan
                           (recursive-item-plan-fits? recursive-plan stride))
                    false)
                  (throw
                   (ex-info
                    "component list item validation plan is invalid"
                    {:phase :wasm-component-list-item-lowering
                     :item-kind item-kind
                     :item-validation-args item-validation-args
                     :stride stride})))
                (let [index-local (allocate! 0x7f)
                      item-local (allocate! 0x7f)
                      pointer-local (allocate! 0x7f)
                      length-local (allocate! 0x7f)
                      total-local (allocate! 0x7f)
                      next-total-local (allocate! 0x7f)
                      end-local (allocate! 0x7f)
                      max-total max-total-bytes]
                  (case item-kind
                   1
                   (concat
                   ;; Canonical list<string>/list<keyword> items are
                   ;; standard32 (pointer,length) records at offsets 0/4.
                   ;; Visit every item even when the branch only observes the
                   ;; outer count.
                   (i32-const 0) [::local-set index-local]
                   (i32-const 0) [::local-set total-local]
                   [0x02 0x40
                    0x03 0x40
                    ::local-get index-local
                    ::local-get (:count-local list-validation)
                    0x4f
                    0x0d 0x01
                    ::local-get (:pointer-local list-validation)
                    ::local-get index-local
                    0x41] (sleb stride)
                   [0x6c 0x6a ::local-set item-local
                    ::local-get item-local
                    0x28 0x02 0x00
                    ::local-set pointer-local
                    ::local-get item-local
                    0x28 0x02 0x04
                    ::local-set length-local
                    ::local-get total-local
                    ::local-get length-local
                    0x6a ::local-set next-total-local
                    ::local-get next-total-local
                    ::local-get total-local
                    0x49
                    0x04 0x40 0x00 0x0b
                    ::local-get next-total-local
                    0x41] (sleb max-total)
                   [0x4b
                    0x04 0x40 0x00 0x0b
                    ::local-get next-total-local
                    ::local-set total-local
                    ::local-get pointer-local
                    ::local-get length-local
                    0x6a ::local-set end-local
                    ::local-get end-local
                    ::local-get pointer-local
                    0x49
                    0x04 0x40 0x00 0x0b
                    ::local-get end-local
                    0x3f 0x00
                    0x41 16 0x74
                    0x4b
                    0x04 0x40 0x00 0x0b]
                   (emit-valid-utf8 pointer-local length-local allocate!)
                   [
                    ::local-get index-local
                    0x41 0x01 0x6a
                    ::local-set index-local
                    0x0c 0x00
                    0x0b 0x0b])

                   2
                   (concat
                    ;; Canonical bool items are one byte. Validate every
                    ;; active item even when the caller observes only count.
                    (i32-const 0) [::local-set index-local]
                    [0x02 0x40
                     0x03 0x40
                     ::local-get index-local
                     ::local-get (:count-local list-validation)
                     0x4f
                     0x0d 0x01
                     ::local-get (:pointer-local list-validation)
                     ::local-get index-local
                     0x41] (sleb stride)
                    [0x6c 0x6a ::local-set item-local
                     ::local-get item-local
                     0x2d 0x00 0x00
                     0x41 0x01
                     0x4b
                     0x04 0x40 0x00 0x0b
                     ::local-get index-local
                     0x41 0x01 0x6a
                     ::local-set index-local
                     0x0c 0x00
                     0x0b 0x0b])

                   3
                   (concat
                    ;; A finite inline record needs no traversal for numeric
                    ;; leaves, but every bool field must be canonical even
                    ;; when source code observes only the outer list count.
                    (i32-const 0) [::local-set index-local]
                    [0x02 0x40
                     0x03 0x40
                     ::local-get index-local
                     ::local-get (:count-local list-validation)
                     0x4f
                     0x0d 0x01
                     ::local-get (:pointer-local list-validation)
                     ::local-get index-local
                     0x41] (sleb stride)
                    [0x6c 0x6a ::local-set item-local]
                    (mapcat
                     (fn [offset]
                       (concat
                        [::local-get item-local
                         0x2d 0x00]
                        (uleb offset)
                        [0x41 0x01
                         0x4b
                         0x04 0x40 0x00 0x0b]))
                     bool-offsets)
                    [::local-get index-local
                     0x41 0x01 0x6a
                     ::local-set index-local
                     0x0c 0x00
                     0x0b 0x0b])

                   4
                   (let [total-local (allocate! 0x7f)]
                     (letfn [(nested-node-code
                               [pointer count level]
                               (let [[item-stride item-alignment]
                                     (nth nested-layouts level)
                                     validated
                                     (emit-component-list-validation
                                      pointer count nested-total
                                      item-stride item-alignment env)
                                     next-total-local (allocate! 0x7f)
                                     nested?
                                     (< (inc level) nested-depth)
                                     traversal
                                     (when nested?
                                       (let [nested-index-local (allocate! 0x7f)
                                             nested-item-local (allocate! 0x7f)
                                             nested-pointer-local (allocate! 0x7f)
                                             nested-count-local (allocate! 0x7f)]
                                         (concat
                                          (i32-const 0)
                                          [::local-set nested-index-local
                                           0x02 0x40
                                           0x03 0x40
                                           ::local-get nested-index-local
                                           ::local-get (:count-local validated)
                                           0x4f
                                           0x0d 0x01
                                           ::local-get (:pointer-local validated)
                                           ::local-get nested-index-local
                                           0x41]
                                          (sleb item-stride)
                                          [0x6c 0x6a
                                           ::local-set nested-item-local
                                           ::local-get nested-item-local
                                           0x28 0x02 0x00
                                           ::local-set nested-pointer-local
                                           ::local-get nested-item-local
                                           0x28 0x02 0x04
                                           ::local-set nested-count-local]
                                          (nested-node-code
                                           {:wasm-local nested-pointer-local}
                                           {:wasm-local nested-count-local}
                                           (inc level))
                                          [::local-get nested-index-local
                                           0x41 0x01 0x6a
                                           ::local-set nested-index-local
                                           0x0c 0x00
                                           0x0b 0x0b])))]
                                 (concat
                                  (:code validated)
                                  [::local-get total-local
                                   ::local-get (:count-local validated)
                                   0x6a
                                   ::local-set next-total-local
                                   ::local-get next-total-local
                                   ::local-get total-local
                                   0x49
                                   0x04 0x40 0x00 0x0b
                                   ::local-get next-total-local
                                   0x41]
                                  (sleb nested-total)
                                  [0x4b
                                   0x04 0x40 0x00 0x0b
                                   ::local-get next-total-local
                                   ::local-set total-local]
                                  traversal)))]
                       (let [outer-index-local (allocate! 0x7f)
                             outer-item-local (allocate! 0x7f)
                             nested-pointer-local (allocate! 0x7f)
                             nested-count-local (allocate! 0x7f)]
                         (concat
                          [::local-get (:count-local list-validation)
                           ::local-set total-local]
                          (i32-const 0)
                          [::local-set outer-index-local
                           0x02 0x40
                           0x03 0x40
                           ::local-get outer-index-local
                           ::local-get (:count-local list-validation)
                           0x4f
                           0x0d 0x01
                           ::local-get (:pointer-local list-validation)
                           ::local-get outer-index-local
                           0x41]
                          (sleb stride)
                          [0x6c 0x6a
                           ::local-set outer-item-local
                           ::local-get outer-item-local
                           0x28 0x02 0x00
                           ::local-set nested-pointer-local
                           ::local-get outer-item-local
                           0x28 0x02 0x04
                           ::local-set nested-count-local]
                          (nested-node-code
                           {:wasm-local nested-pointer-local}
                           {:wasm-local nested-count-local}
                           0)
                          [::local-get outer-index-local
                           0x41 0x01 0x6a
                           ::local-set outer-index-local
                           0x0c 0x00
                           0x0b 0x0b]))))

                   5
                   (let [discriminant-local (allocate! 0x7f)]
                     (concat
                      ;; Validate the discriminant before selecting a case.
                      ;; Only the selected case may inspect the joined payload:
                      ;; inactive pointer-shaped bytes are intentionally ignored.
                      (i32-const 0) [::local-set index-local]
                      (i32-const 0) [::local-set total-local]
                      [0x02 0x40
                       0x03 0x40
                       ::local-get index-local
                       ::local-get (:count-local list-validation)
                       0x4f
                       0x0d 0x01
                       ::local-get (:pointer-local list-validation)
                       ::local-get index-local
                       0x41] (sleb stride)
                      [0x6c 0x6a ::local-set item-local
                       ::local-get item-local
                       0x2d 0x00 0x00
                       ::local-set discriminant-local
                       ::local-get discriminant-local
                       0x41] (sleb union-case-count)
                      [0x4f 0x04 0x40 0x00 0x0b]
                      (mapcat
                       (fn [case-index [case-kind case-max]]
                         (concat
                          [::local-get discriminant-local
                           0x41] (sleb case-index)
                          [0x46 0x04 0x40]
                          (case case-kind
                            0 []
                            1 (concat
                               [::local-get item-local
                                0x28 0x02] (uleb union-payload-offset)
                               [::local-set pointer-local
                                ::local-get item-local
                                0x28 0x02] (uleb (+ union-payload-offset 4))
                               [::local-set length-local
                                ::local-get length-local
                                0x41] (sleb case-max)
                               [0x4b 0x04 0x40 0x00 0x0b
                                ::local-get total-local
                                ::local-get length-local
                                0x6a ::local-set next-total-local
                                ::local-get next-total-local
                                ::local-get total-local
                                0x49 0x04 0x40 0x00 0x0b
                                ::local-get next-total-local
                                0x41] (sleb union-total)
                               [0x4b 0x04 0x40 0x00 0x0b
                                ::local-get next-total-local
                                ::local-set total-local
                                ::local-get pointer-local
                                ::local-get length-local
                                0x6a ::local-set end-local
                                ::local-get end-local
                                ::local-get pointer-local
                                0x49 0x04 0x40 0x00 0x0b
                                ::local-get end-local
                                0x3f 0x00
                                0x41 16 0x74
                                0x4b 0x04 0x40 0x00 0x0b]
                               (emit-valid-utf8
                                pointer-local length-local allocate!))
                            2 (concat
                               [::local-get item-local
                                0x2d 0x00] (uleb union-payload-offset)
                               [0x41 0x01
                                0x4b 0x04 0x40 0x00 0x0b]))
                          [0x0b]))
                       (range union-case-count)
                       union-cases)
                      [::local-get index-local
                       0x41 0x01 0x6a
                       ::local-set index-local
                       0x0c 0x00
                       0x0b 0x0b]))

                   6
                   (let [item-total-local (allocate! 0x7f)]
                     (letfn [(address-code [base-local offset]
                               (concat
                                [::local-get base-local]
                                (when (pos? offset)
                                  (concat (i32-const offset) [0x6a]))))
                             (node-code [node base-local offset]
                               (case (:kind node)
                                 :scalar []
                                 :bool
                                 (concat
                                  (address-code base-local offset)
                                  [0x2d 0x00 0x00
                                   0x41 0x01
                                   0x4b 0x04 0x40 0x00 0x0b])
                                 :string
                                 (concat
                                  (address-code base-local offset)
                                  [0x28 0x02 0x00
                                   ::local-set pointer-local]
                                  (address-code base-local (+ offset 4))
                                  [0x28 0x02 0x00
                                   ::local-set length-local
                                   ::local-get length-local
                                   0x41] (sleb (:maximum node))
                                  [0x4b 0x04 0x40 0x00 0x0b
                                   ::local-get total-local
                                   ::local-get length-local
                                   0x6a ::local-set next-total-local
                                   ::local-get next-total-local
                                   ::local-get total-local
                                   0x49 0x04 0x40 0x00 0x0b
                                   ::local-get next-total-local
                                   0x41] (sleb recursive-byte-total)
                                  [0x4b 0x04 0x40 0x00 0x0b
                                   ::local-get next-total-local
                                   ::local-set total-local
                                   ::local-get pointer-local
                                   ::local-get length-local
                                   0x6a ::local-set end-local
                                   ::local-get end-local
                                   ::local-get pointer-local
                                   0x49 0x04 0x40 0x00 0x0b
                                   ::local-get end-local
                                   0x3f 0x00
                                   0x41 16 0x74
                                   0x4b 0x04 0x40 0x00 0x0b]
                                  (emit-valid-utf8
                                   pointer-local length-local allocate!))
                                 :record
                                 (mapcat
                                  (fn [{field-offset :offset child :node}]
                                    (node-code child base-local
                                               (+ offset field-offset)))
                                  (:fields node))
                                 :union
                                 (let [disc-local (allocate! 0x7f)]
                                   (concat
                                    (address-code base-local offset)
                                    [0x2d 0x00 0x00
                                     ::local-set disc-local
                                     ::local-get disc-local
                                     0x41] (sleb (count (:cases node)))
                                    [0x4f 0x04 0x40 0x00 0x0b]
                                    (mapcat
                                     (fn [case-index child]
                                       (concat
                                        [::local-get disc-local
                                         0x41] (sleb case-index)
                                        [0x46 0x04 0x40]
                                        (node-code
                                         child base-local
                                         (+ offset (:payload-offset node)))
                                        [0x0b]))
                                     (range (count (:cases node)))
                                     (:cases node))))
                                 :list
                                 (let [nested-pointer-local (allocate! 0x7f)
                                       nested-count-local (allocate! 0x7f)
                                       validated
                                       (emit-component-list-validation
                                        {:wasm-local nested-pointer-local}
                                        {:wasm-local nested-count-local}
                                        (:maximum node)
                                        (:stride node)
                                        (:alignment node)
                                        env)
                                       nested-next-total-local
                                       (allocate! 0x7f)
                                       child (:item node)
                                       traversal
                                       (when-not (= :scalar (:kind child))
                                         (let [nested-index-local
                                               (allocate! 0x7f)
                                               nested-item-local
                                               (allocate! 0x7f)]
                                           (concat
                                            (i32-const 0)
                                            [::local-set nested-index-local
                                             0x02 0x40
                                             0x03 0x40
                                             ::local-get nested-index-local
                                             ::local-get
                                             (:count-local validated)
                                             0x4f
                                             0x0d 0x01
                                             ::local-get
                                             (:pointer-local validated)
                                             ::local-get nested-index-local
                                             0x41]
                                            (sleb (:stride node))
                                            [0x6c 0x6a
                                             ::local-set nested-item-local]
                                            (node-code child nested-item-local 0)
                                            [::local-get nested-index-local
                                             0x41 0x01 0x6a
                                             ::local-set nested-index-local
                                             0x0c 0x00
                                             0x0b 0x0b])))]
                                   (concat
                                    (address-code base-local offset)
                                    [0x28 0x02 0x00
                                     ::local-set nested-pointer-local]
                                    (address-code base-local (+ offset 4))
                                    [0x28 0x02 0x00
                                     ::local-set nested-count-local]
                                    (:code validated)
                                    [::local-get item-total-local
                                     ::local-get (:count-local validated)
                                     0x6a
                                     ::local-set nested-next-total-local
                                     ::local-get nested-next-total-local
                                     ::local-get item-total-local
                                     0x49 0x04 0x40 0x00 0x0b
                                     ::local-get nested-next-total-local
                                     0x41] (sleb recursive-item-total)
                                    [0x4b 0x04 0x40 0x00 0x0b
                                     ::local-get nested-next-total-local
                                     ::local-set item-total-local]
                                    traversal))
                                 []))]
                       (concat
                        (i32-const 0) [::local-set total-local]
                        [::local-get (:count-local list-validation)
                         ::local-set item-total-local
                         ::local-get item-total-local
                         0x41] (sleb recursive-item-total)
                        [0x4b 0x04 0x40 0x00 0x0b]
                        (i32-const 0) [::local-set index-local]
                        [0x02 0x40
                         0x03 0x40
                         ::local-get index-local
                         ::local-get (:count-local list-validation)
                         0x4f
                         0x0d 0x01
                         ::local-get (:pointer-local list-validation)
                         ::local-get index-local
                         0x41] (sleb stride)
                        [0x6c 0x6a ::local-set item-local]
                        (node-code recursive-plan item-local 0)
                        [::local-get index-local
                         0x41 0x01 0x6a
                         ::local-set index-local
                         0x0c 0x00
                         0x0b 0x0b]))))))))
            (emit-option-list-capability-count [args env]
              (let [[cap-id pointer count fallback max-items stride alignment
                     result-size payload-offset requested-result-alignment
                     item-kind & item-validation-args] args
                    result-alignment (or requested-result-alignment alignment)
                    typed-import (get intrinsic-indices [:capability cap-id])
                    realloc-index (get intrinsic-indices :component-realloc)
                    _ (when-not typed-import
                        (throw
                         (ex-info
                          "component aggregate capability requires a named import"
                          {:phase :wasm-component-aggregate-capability-lowering
                           :capability cap-id})))
                    _ (when-not realloc-index
                        (throw
                         (ex-info
                          "component aggregate capability requires canonical realloc"
                          {:phase :wasm-component-aggregate-capability-lowering})))
                    _ (when-not
                       (and (integer? result-size) (pos? result-size)
                            (<= result-size 0x7fffffff)
                            (integer? payload-offset) (<= 0 payload-offset)
                            (<= (+ payload-offset 8) result-size)
                            (integer? result-alignment)
                            (contains? #{1 2 4 8} result-alignment))
                        (throw
                         (ex-info
                          "component aggregate capability result layout is invalid"
                          {:phase :wasm-component-aggregate-capability-lowering
                           :result-size result-size
                           :payload-offset payload-offset})))
                    request (emit-component-list-validation
                             pointer count max-items stride alignment env)
                    request-items
                    (emit-component-list-item-validation
                     request stride item-kind item-validation-args)
                    result-local (allocate! 0x7f)
                    result-end-local (allocate! 0x7f)
                    disc-local (allocate! 0x7f)
                    result-pointer-local (allocate! 0x7f)
                    result-count-local (allocate! 0x7f)
                    result-list
                    (emit-component-list-validation
                     {:wasm-local result-pointer-local}
                     {:wasm-local result-count-local}
                     max-items stride alignment env)
                    result-items
                    (emit-component-list-item-validation
                     result-list stride item-kind item-validation-args)]
                (concat
                 (:code request)
                 request-items
                 (i32-const 0) (i32-const 0)
                 (i32-const result-alignment) (i32-const result-size)
                 [0x10 realloc-index ::local-set result-local]
                 (i32-const 1)
                 [::local-get (:pointer-local request)
                  ::local-get (:count-local request)
                  ::local-get result-local
                  0x10 typed-import]
                 [::local-get result-local 0x41] (sleb (dec result-alignment))
                 [0x71 0x45 0x04 0x40 0x05 0x00 0x0b
                  ::local-get result-local 0x41] (sleb result-size)
                 [0x6a ::local-set result-end-local
                  ::local-get result-end-local ::local-get result-local
                  0x49 0x04 0x40 0x00 0x0b
                  ::local-get result-end-local 0x3f 0x00
                  0x41 16 0x74 0x4b 0x04 0x40 0x00 0x0b
                  ::local-get result-local 0x2d 0x00 0x00
                  ::local-set disc-local
                  ::local-get disc-local 0x41 1 0x4b
                  0x04 0x40 0x00 0x0b
                  ::local-get disc-local
                  0x04 0x7e
                  ::local-get result-local 0x28 0x02]
                 (uleb payload-offset)
                 [::local-set result-pointer-local
                  ::local-get result-local 0x28 0x02]
                 (uleb (+ payload-offset 4))
                 [::local-set result-count-local]
                 (:code result-list)
                 result-items
                 [::local-get (:count-local result-list) 0xad
                  0x05]
                 (emit* fallback env)
                 [0x0b])))
            (emit-result-list-capability-count [args env]
              (let [[cap-id request-disc pointer count max-items stride alignment
                     result-size payload-offset requested-result-alignment
                     item-kind & item-validation-args] args
                    result-alignment (or requested-result-alignment alignment)
                    typed-import (get intrinsic-indices [:capability cap-id])
                    realloc-index (get intrinsic-indices :component-realloc)
                    _ (when-not (contains? #{0 1} request-disc)
                        (throw
                         (ex-info
                          "component result capability request case is invalid"
                          {:phase :wasm-component-aggregate-capability-lowering
                           :request-disc request-disc})))
                    _ (when-not typed-import
                        (throw
                         (ex-info
                          "component aggregate capability requires a named import"
                          {:phase :wasm-component-aggregate-capability-lowering
                           :capability cap-id})))
                    _ (when-not realloc-index
                        (throw
                         (ex-info
                          "component aggregate capability requires canonical realloc"
                          {:phase :wasm-component-aggregate-capability-lowering})))
                    _ (when-not
                       (and (integer? result-size) (pos? result-size)
                            (<= result-size 0x7fffffff)
                            (integer? payload-offset) (<= 0 payload-offset)
                            (<= (+ payload-offset 8) result-size)
                            (integer? result-alignment)
                            (contains? #{1 2 4 8} result-alignment))
                        (throw
                         (ex-info
                          "component aggregate capability result layout is invalid"
                          {:phase :wasm-component-aggregate-capability-lowering
                           :result-size result-size
                           :payload-offset payload-offset})))
                    request (emit-component-list-validation
                             pointer count max-items stride alignment env)
                    request-items
                    (emit-component-list-item-validation
                     request stride item-kind item-validation-args)
                    result-local (allocate! 0x7f)
                    result-end-local (allocate! 0x7f)
                    disc-local (allocate! 0x7f)
                    result-pointer-local (allocate! 0x7f)
                    result-count-local (allocate! 0x7f)
                    result-list
                    (emit-component-list-validation
                     {:wasm-local result-pointer-local}
                     {:wasm-local result-count-local}
                     max-items stride alignment env)
                    result-items
                    (emit-component-list-item-validation
                     result-list stride item-kind item-validation-args)]
                (concat
                 (:code request)
                 request-items
                 (i32-const 0) (i32-const 0)
                 (i32-const result-alignment) (i32-const result-size)
                 [0x10 realloc-index ::local-set result-local]
                 (i32-const request-disc)
                 [::local-get (:pointer-local request)
                  ::local-get (:count-local request)
                  ::local-get result-local
                  0x10 typed-import]
                 [::local-get result-local 0x41] (sleb (dec result-alignment))
                 [0x71 0x45 0x04 0x40 0x05 0x00 0x0b
                  ::local-get result-local 0x41] (sleb result-size)
                 [0x6a ::local-set result-end-local
                  ::local-get result-end-local ::local-get result-local
                  0x49 0x04 0x40 0x00 0x0b
                  ::local-get result-end-local 0x3f 0x00
                  0x41 16 0x74 0x4b 0x04 0x40 0x00 0x0b
                  ::local-get result-local 0x2d 0x00 0x00
                  ::local-set disc-local
                  ::local-get disc-local 0x41 1 0x4b
                  0x04 0x40 0x00 0x0b
                  ::local-get result-local 0x28 0x02]
                 (uleb payload-offset)
                 [::local-set result-pointer-local
                  ::local-get result-local 0x28 0x02]
                 (uleb (+ payload-offset 4))
                 [::local-set result-count-local]
                 (:code result-list)
                 result-items
                 [::local-get (:count-local result-list) 0xad])))
            (emit-component-list-get [op args env]
              (let [[pointer count index fallback
                     max-items stride alignment] args
                    {:keys [code pointer-local count-local]}
                    (emit-component-list-validation
                     pointer count max-items stride alignment env)
                    index-local (allocate! 0x7e)
                    i64-result? (= op 'component-list-get-i64)
                    load-op (if i64-result? 0x29 0x2b)
                    result-type (if i64-result? 0x7e 0x7c)]
                (concat
                 code
                 (emit* index env) [::local-set index-local]
                 ;; Unsigned comparison makes a negative i64 index take the
                 ;; fallback branch without addressing memory.
                 [::local-get index-local
                  ::local-get count-local 0xad
                  0x54 0x04 result-type
                  ::local-get pointer-local
                  ::local-get index-local 0xa7
                  0x41] (sleb stride)
                 [0x6c 0x6a load-op 0x03 0x00 0x05]
                 (emit* fallback env)
                 [0x0b])))
            (emit-component-string-byte-length [args env]
              (let [[pointer length max-bytes] args
                    pointer-local (allocate! 0x7f)
                    length-local (allocate! 0x7f)
                    end-local (allocate! 0x7f)]
                (when-not (and (integer? max-bytes)
                               (<= 0 max-bytes 0x7fffffff))
                  (throw
                   (ex-info "component string byte bound is invalid"
                            {:phase :wasm-component-scalar-lowering
                             :max-bytes max-bytes})))
                (concat
                 (emit* pointer env) [::local-set pointer-local]
                 (emit* length env) [::local-set length-local]
                 [::local-get length-local 0x41] (sleb max-bytes)
                 [0x4b 0x04 0x40 0x00 0x0b
                  ::local-get pointer-local ::local-get length-local
                  0x6a ::local-set end-local
                  ::local-get end-local ::local-get pointer-local
                  0x49 0x04 0x40 0x00 0x0b
                  ::local-get end-local 0x3f 0x00
                  0x41 16 0x74 0x4b 0x04 0x40 0x00 0x0b
                  ::local-get length-local 0xad])))
            (emit-assoc [type value index replacement replacement-type env]
              (concat (i32-const (descriptor-id type)) (emit* value env)
                      (i32-const index) (emit* replacement env)
                      [0x10 (get intrinsic-indices
                                 (symbol (str "typed-assoc-"
                                              (scalar-suffix replacement-type))))]))
            (emit-match [type value-form branches env]
              (let [value-local (allocate! 0x6f)
                    tag-local (allocate! 0x7f)
                    result-type (typed/infer-type
                                 (nth (first branches) 2)
                                 (assoc (into {} (map (fn [[key item]] [key (:type item)]) env))
                                        (second (first branches))
                                        (second (first (nth type 2))))
                                 signatures)
                    setup (concat (emit* value-form env) [::local-set value-local]
                                  (i32-const (descriptor-id type)) [::local-get value-local]
                                  [0x10 (get intrinsic-indices 'typed-tag) ::local-set tag-local])
                    emit-branches
                    (fn emit-branches [index remaining]
                      (let [[_ binder body] (first remaining)
                            payload-type (second (nth (nth type 2) index))
                            binder-local (allocate! (wasm-type payload-type))
                            branch-env (assoc env binder {:index binder-local :type payload-type})
                            body-code (concat (emit-get type {:wasm-local value-local} index payload-type env)
                                              [::local-set binder-local] (emit* body branch-env))]
                        (if (= 1 (count remaining))
                          body-code
                          (concat [::local-get tag-local] (i32-const index) [0x46 0x04
                                  (wasm-type result-type)]
                                  body-code [0x05]
                                  (emit-branches (inc index) (rest remaining)) [0x0b]))))]
                (concat setup (emit-branches 0 branches))))
            (emit* [form env]
              (cond
                (and (map? form) (contains? form :wasm-local)) [::local-get (:wasm-local form)]
                #?(:clj (integer? form)
                   :cljs (or (i64/bigint-value? form) (integer? form)))
                (into [0x42] (sleb form))
                (and component-canonical-scalars? (boolean? form))
                (i32-const (if form 1 0))
                (or (string? form) (keyword? form) (boolean? form))
                (let [literal [(cond (string? form) :string (keyword? form) :keyword :else :bool)
                               (if (keyword? form) (str form) form)]]
                  (concat (i32-const (get literal-indices literal))
                          [0x10 (get intrinsic-indices 'typed-literal)]))
                (symbol? form) [::local-get (:index (get env form))]
                :else
                (let [[op & args] form]
                  (cond
                    (= op 'let)
                    (let [[bindings body] args]
                      (loop [remaining (partition 2 bindings) current-env env code []]
                        (if-let [[name value] (first remaining)]
                          (let [type (typed/infer-type value
                                                       (into {} (map (fn [[key item]]
                                                                      [key (:type item)]) current-env))
                                                       signatures)
                                value-code (emit* value current-env)
                                local (allocate! (wasm-type type))]
                            (recur (next remaining)
                                   (assoc current-env name {:index local :type type})
                                   (concat code value-code [::local-set local])))
                          (concat code (emit* body current-env)))))
                    (= op 'if)
                    (let [[test then else] args
                          result-type (typed/infer-type
                                       then
                                       (into {} (map (fn [[key item]] [key (:type item)]) env))
                                       signatures)]
                      (concat (emit-test test env)
                              [0x04 (wasm-type result-type)]
                              (emit* then env) [0x05] (emit* else env) [0x0b]))
                    (= op 'typed-cap-call)
                    (let [[cap-id _ _ request] args
                          typed-import (get intrinsic-indices [:capability cap-id])]
                      (if typed-import
                        ;; A typed import takes the request directly; the
                        ;; capability id is carried by the import identity, not
                        ;; passed as an operand.
                        (concat (emit* request env) [0x10 typed-import])
                        (concat (i32-const cap-id) (emit* request env)
                                [0x10 (get intrinsic-indices 'typed-cap-call)])))
                    (= op 'i64-extend-i32-u)
                    (concat (emit* (first args) env) [0xad])
                    (= op 'component-unreachable)
                    [0x00]
                    (= op 'component-assert-bool)
                    (let [value (first args)]
                      (concat (emit* value env) [0x41 1 0x4b 0x04 0x40 0x00 0x0b]
                              (emit* value env)))
                    (= op 'component-i64-to-i32)
                    (concat (emit* (first args) env) [0xa7])
                    (= op 'component-i32-to-f32)
                    (concat (emit* (first args) env) [0xbe])
                    (= op 'component-i64-to-f32)
                    (concat (emit* (first args) env) [0xa7 0xbe])
                    (= op 'component-i64-to-f64)
                    (concat (emit* (first args) env) [0xbf])
                    (= op 'component-string-byte-length)
                    (emit-component-string-byte-length args env)
                    (= op 'component-list-count)
                    (let [[pointer count max-items stride alignment] args
                          {:keys [code count-local]}
                          (emit-component-list-validation
                           pointer count max-items stride alignment env)]
                      (concat code [::local-get count-local 0xad]))
                    (contains? '#{component-list-at-i64
                                  component-list-at-f64} op)
                    (let [[pointer count index max-items stride alignment] args
                          {:keys [code pointer-local count-local]}
                          (emit-component-list-validation
                           pointer count max-items stride alignment env)
                          index-local (allocate! 0x7e)
                          load-op (if (= op 'component-list-at-i64) 0x29 0x2b)]
                      (concat
                       code
                       (emit* index env) [::local-set index-local]
                       ;; One unsigned comparison rejects both negative i64
                       ;; indices and indices at/past the selected count.
                       [::local-get index-local
                        ::local-get count-local 0xad
                        0x5a 0x04 0x40 0x00 0x0b
                        ::local-get pointer-local
                        ::local-get index-local 0xa7
                        0x41] (sleb stride)
                       [0x6c 0x6a load-op 0x03 0x00]))
                    (contains? '#{component-list-get-i64
                                  component-list-get-f64} op)
                    (emit-component-list-get op args env)
                    (= op 'f64-to-bits)
                    (let [value-local (allocate! 0x7c)]
                      (concat (emit* (first args) env) [::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x62 0x04 0x7e]
                              [0x42] (sleb 9221120237041090560)
                              [0x05 ::local-get value-local 0xbd 0x0b]))
                    (= op 'f64-from-bits)
                    (let [value-local (allocate! 0x7c)]
                      (concat (emit* (first args) env) [0xbf ::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x62 0x04 0x7c]
                              [0x42] (sleb 9221120237041090560) [0xbf]
                              [0x05 ::local-get value-local 0x0b]))
                    (= op 'i64-to-f64-rounded)
                    (concat (emit* (first args) env) [0xb9])
                    (= op 'i64-to-f64-checked)
                    (let [source-local (allocate! 0x7e)
                          result-local (allocate! 0x7c)]
                      (concat (emit* (first args) env) [::local-set source-local]
                              [::local-get source-local 0xb9 ::local-set result-local]
                              ;; The truncation itself traps if rounding crossed
                              ;; the signed-i64 boundary.  Otherwise require an
                              ;; exact round-trip before returning the f64.
                              [::local-get result-local 0xb0 ::local-get source-local 0x52
                               0x04 0x40 0x00 0x0b ::local-get result-local]))
                    (= op 'f64-to-i64-truncating)
                    (concat (emit* (first args) env) [0xb0])
                    (= op 'f64-to-i64-checked)
                    (let [source-local (allocate! 0x7c)
                          result-local (allocate! 0x7e)]
                      (concat (emit* (first args) env) [::local-set source-local]
                              [::local-get source-local 0xb0 ::local-set result-local]
                              [::local-get result-local 0xb9 ::local-get source-local 0x62
                               0x04 0x40 0x00 0x0b ::local-get result-local]))
                    (contains? '#{f64-add f64-sub f64-mul f64-div f64-min f64-max} op)
                    (concat (emit* (first args) env) (emit* (second args) env)
                            [({'f64-add 0xa0 'f64-sub 0xa1 'f64-mul 0xa2 'f64-div 0xa3
                               'f64-min 0xa4 'f64-max 0xa5} op)])
                    (= op 'f64-neg)
                    (concat (emit* (first args) env) [0x9a])
                    (= op 'f64-abs)
                    (concat (emit* (first args) env) [0x99])
                    (= op 'f64-sqrt)
                    (concat (emit* (first args) env) [0x9f])
                    (contains? '#{f64-sin-quarter-turn f64-cos-quarter-turn} op)
                    (let [value-local (allocate! 0x7c)
                          value {:wasm-local value-local}
                          domain-check (concat
                                        [::local-get value-local ::local-get value-local 0x62
                                         ::local-get value-local 0x99]
                                        (emit* (f64-constant "4605249457297304856") env)
                                        [0x64 0x72 0x04 0x40 0x00 0x0b])]
                      (concat (emit* (first args) env) [::local-set value-local]
                              domain-check
                              (if (= op 'f64-sin-quarter-turn)
                                (concat [::local-get value-local]
                                        (emit* (f64-constant 0) env) [0x61 0x04 0x7c]
                                        [::local-get value-local 0x05]
                                        (emit* (bounded-sin-form value) env) [0x0b])
                                (emit* (bounded-cos-form value) env))))
                    (contains? '#{f64-sin-bounded f64-cos-bounded} op)
                    (let [value-local (allocate! 0x7c)
                          scaled-local (allocate! 0x7c)
                          nearest-local (allocate! 0x7c)
                          quadrant-local (allocate! 0x7e)
                          reduced-local (allocate! 0x7c)
                          reduced {:wasm-local reduced-local}
                          sin-code (concat [::local-get reduced-local]
                                           (emit* (f64-constant 0) env) [0x61 0x04 0x7c]
                                           [::local-get reduced-local 0x05]
                                           (emit* (bounded-sin-form reduced) env) [0x0b])
                          cos-code (emit* (bounded-cos-form reduced) env)
                          neg-sin (concat sin-code [0x9a])
                          neg-cos (concat cos-code [0x9a])
                          branch-code
                          (if (= op 'f64-sin-bounded)
                            [sin-code cos-code neg-sin neg-cos]
                            [cos-code neg-sin neg-cos sin-code])
                          choose (fn choose [quadrant]
                                   (if (= quadrant 3)
                                     (nth branch-code quadrant)
                                     (concat [::local-get quadrant-local 0x42] (sleb quadrant) [0x51 0x04 0x7c]
                                             (nth branch-code quadrant) [0x05]
                                             (choose (inc quadrant)) [0x0b])))]
                      (concat
                       (emit* (first args) env) [::local-set value-local]
                       ;; Reject NaN, infinities, and values outside ±8192π.
                       [::local-get value-local ::local-get value-local 0x62
                        ::local-get value-local 0x99]
                       (emit* (f64-constant "4672803451707862296") env)
                       [0x64 0x72 0x04 0x40 0x00 0x0b]
                       ;; scaled=x*(2/π), then nearest integer with ties away from zero.
                       [::local-get value-local]
                       (emit* (f64-constant "4603909380684499075") env)
                       [0xa2 ::local-set scaled-local ::local-get scaled-local]
                       (emit* (f64-constant "0") env) [0x66 0x04 0x7c]
                       [::local-get scaled-local]
                       (emit* (f64-constant "4602678819172646912") env) [0xa0 0x9c 0x05]
                       [::local-get scaled-local]
                       (emit* (f64-constant "-4620693217682128896") env) [0xa0 0x9b 0x0b]
                       [::local-set nearest-local]
                       ;; r=(x-n*pi/2_hi)-n*pi/2_lo.
                       [::local-get value-local ::local-get nearest-local]
                       (emit* (f64-constant "4609753056924675352") env) [0xa2 0xa1]
                       [::local-get nearest-local]
                       (emit* (f64-constant "4364452196894661639") env) [0xa2 0xa1 ::local-set reduced-local]
                       [::local-get nearest-local 0xb0 0x42] (sleb 3) [0x83 ::local-set quadrant-local]
                       (choose 0)))
                    (= op 'f64-exp-near-zero)
                    (let [value-local (allocate! 0x7c)
                          value {:wasm-local value-local}]
                      (concat (emit* (first args) env) [::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x62
                               ::local-get value-local 0x99]
                              (emit* (f64-constant "4602678819172646912") env)
                              [0x64 0x72 0x04 0x40 0x00 0x0b]
                              (emit* (bounded-exp-form value) env)))
                    (= op 'f64-log-near-one)
                    (let [value-local (allocate! 0x7c)
                          value {:wasm-local value-local}]
                      (concat (emit* (first args) env) [::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x62
                               ::local-get value-local]
                              (emit* (f64-constant "4604930618986332160") env)
                              [0x63 0x72 ::local-get value-local]
                              (emit* (f64-constant "4609434218613702656") env)
                              [0x64 0x72 0x04 0x40 0x00 0x0b]
                              (emit* (bounded-log-form value) env)))
                    (= op 'f64-atan2-bounded)
                    (let [y-local (allocate! 0x7c)
                          x-local (allocate! 0x7c)
                          y {:wasm-local y-local}
                          x {:wasm-local x-local}
                          finite-check (fn [local]
                                         (concat [::local-get local ::local-get local 0x62
                                                  ::local-get local 0x99]
                                                 (emit* (f64-constant "9218868437227405311") env)
                                                 [0x64 0x72]))]
                      (concat (emit* (first args) env) [::local-set y-local]
                              (emit* (second args) env) [::local-set x-local]
                              (finite-check y-local) (finite-check x-local)
                              [0x72 0x04 0x40 0x00 0x0b]
                              (emit* (bounded-atan2-form y x) env)))
                    (= op 'f64-exp-bounded)
                    (let [value-local (allocate! 0x7c)
                          value {:wasm-local value-local}]
                      (concat (emit* (first args) env) [::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x62
                               ::local-get value-local 0x99]
                              (emit* (f64-constant "4644950930959776239") env)
                              [0x64 0x72 0x04 0x40 0x00 0x0b]
                              (emit* (wide-exp-form value) env)))
                    (= op 'f64-log-bounded)
                    (let [value-local (allocate! 0x7c)
                          value {:wasm-local value-local}]
                      (concat (emit* (first args) env) [::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x62
                               ::local-get value-local]
                              (emit* (f64-constant "2301339409586323456") env)
                              [0x63 0x72 ::local-get value-local]
                              (emit* (f64-constant "6913025428013711360") env)
                              [0x64 0x72 0x04 0x40 0x00 0x0b]
                              (emit* (wide-log-form value) env)))
                    (contains? '#{f64-eq f64-lt f64-le f64-gt f64-ge} op)
                    (emit-bool
                     (concat (emit* (first args) env) (emit* (second args) env)
                             [({'f64-eq 0x61 'f64-lt 0x63 'f64-gt 0x64
                                'f64-le 0x65 'f64-ge 0x66} op)]))
                    (= op 'f64-unordered)
                    (let [left-local (allocate! 0x7c)
                          right-local (allocate! 0x7c)]
                      (emit-bool
                       (concat (emit* (first args) env) [::local-set left-local]
                               (emit* (second args) env) [::local-set right-local]
                               [::local-get left-local ::local-get left-local 0x62
                                ::local-get right-local ::local-get right-local 0x62 0x72])))
                    (= op 'f32-to-bits)
                    (let [value-local (allocate! 0x7d)]
                      (concat (emit* (first args) env) [::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x5c 0x04 0x7e]
                              [0x42] (sleb 2143289344)
                              [0x05 ::local-get value-local 0xbc 0xac 0x0b]))
                    (= op 'f32-from-bits)
                    (let [bits-local (allocate! 0x7e)
                          value-local (allocate! 0x7d)]
                      (concat (emit* (first args) env) [::local-set bits-local]
                              [::local-get bits-local 0x42] (sleb -2147483648) [0x53]
                              [::local-get bits-local 0x42] (sleb 2147483647) [0x55 0x72]
                              [0x04 0x40 0x00 0x0b]
                              [::local-get bits-local 0xa7 0xbe ::local-set value-local]
                              [::local-get value-local ::local-get value-local 0x5c 0x04 0x7d]
                              [0x42] (sleb 2143289344) [0xa7 0xbe]
                              [0x05 ::local-get value-local 0x0b]))
                    (= op 'f64-to-f32-rounded)
                    (concat (emit* (first args) env) [0xb6])
                    (= op 'f32-to-f64-exact)
                    (concat (emit* (first args) env) [0xbb])
                    (= op 'i64-to-f32-rounded)
                    (concat (emit* (first args) env) [0xb4])
                    (= op 'i64-to-f32-checked)
                    (let [source-local (allocate! 0x7e)
                          result-local (allocate! 0x7d)]
                      (concat (emit* (first args) env) [::local-set source-local]
                              [::local-get source-local 0xb4 ::local-set result-local]
                              [::local-get result-local 0xae ::local-get source-local 0x52
                               0x04 0x40 0x00 0x0b ::local-get result-local]))
                    (= op 'f32-to-i64-truncating)
                    (concat (emit* (first args) env) [0xae])
                    (= op 'f32-to-i64-checked)
                    (let [source-local (allocate! 0x7d)
                          result-local (allocate! 0x7e)]
                      (concat (emit* (first args) env) [::local-set source-local]
                              [::local-get source-local 0xae ::local-set result-local]
                              [::local-get result-local 0xb4 ::local-get source-local 0x5c
                               0x04 0x40 0x00 0x0b ::local-get result-local]))
                    (contains? '#{f32-add f32-sub f32-mul f32-div f32-min f32-max} op)
                    (concat (emit* (first args) env) (emit* (second args) env)
                            [({'f32-add 0x92 'f32-sub 0x93 'f32-mul 0x94 'f32-div 0x95
                               'f32-min 0x96 'f32-max 0x97} op)])
                    (= op 'f32-neg)
                    (concat (emit* (first args) env) [0x8c])
                    (= op 'f32-abs)
                    (concat (emit* (first args) env) [0x8b])
                    (= op 'f32-sqrt)
                    (concat (emit* (first args) env) [0x91])
                    (contains? '#{f32-eq f32-lt f32-le f32-gt f32-ge} op)
                    (emit-bool
                     (concat (emit* (first args) env) (emit* (second args) env)
                             [({'f32-eq 0x5b 'f32-lt 0x5d 'f32-gt 0x5e
                                'f32-le 0x5f 'f32-ge 0x60} op)]))
                    (= op 'f32-unordered)
                    (let [left-local (allocate! 0x7d)
                          right-local (allocate! 0x7d)]
                      (emit-bool
                       (concat (emit* (first args) env) [::local-set left-local]
                               (emit* (second args) env) [::local-set right-local]
                               [::local-get left-local ::local-get left-local 0x5c
                                ::local-get right-local ::local-get right-local 0x5c 0x72])))
                    ;; Keep capability-only dispatch after the scalar numeric
                    ;; families. CLJS emits `cond` as nested JavaScript
                    ;; branches; putting this comparatively large predicate
                    ;; before the deeply expanded f32/f64 kernels consumes
                    ;; enough stack to fail on macOS runners with a smaller
                    ;; Node stack.
                    (contains? aggregate-capability-ops op)
                    (emit-aggregate-capability*
                     op args env emit* allocate! intrinsic-indices
                     emit-option-list-capability-count
                     emit-result-list-capability-count)
                    (contains? '#{+ - * quot bit-xor bit-and bit-or} op)
                    (let [opcode ({'+ 0x7c '- 0x7d '* 0x7e 'quot 0x7f
                                   'bit-and 0x83 'bit-xor 0x85 'bit-or 0x84} op)]
                      (if (and (= op '-) (= 1 (count args)))
                        (concat [0x42 0] (emit* (first args) env) [0x7d])
                        (concat (emit* (first args) env)
                                (mapcat #(concat (emit* % env) [opcode]) (rest args)))))
                    (= op 'i32-wrap)
                    (concat (emit* (first args) env) [0xa7 0xac])
                    (= op 'u32-wrap)
                    (concat (emit* (first args) env) [0xa7 0xad])
                    (contains? '#{i32-wrapping-add i32-wrapping-mul i32-xor} op)
                    (concat (emit* (first args) env) [0xa7]
                            (emit* (second args) env) [0xa7]
                            [({'i32-wrapping-add 0x6a 'i32-wrapping-mul 0x6c 'i32-xor 0x73} op)
                             0xac])
                    ;; ADR-2607254600 D1. Operands are already i64, so unlike
                    ;; the i32 shifts below there is no wrap/extend.
                    (contains? '#{i64-shift-left i64-shift-right u64-shift-right} op)
                    (concat (emit* (first args) env)
                            (emit* (second args) env)
                            [({'i64-shift-left 0x86 'i64-shift-right 0x87
                               'u64-shift-right 0x88} op)])

                    ;; ADR-2607254600 D2. No i64.not in wasm; xor all-ones.
                    ;; `0x42 0x7f` is i64.const -1 (SLEB128).
                    (= op 'bit-not)
                    (concat (emit* (first args) env) [0x42 0x7f 0x85])

                    (contains? '#{i32-shift-left i32-shift-right u32-shift-right} op)
                    (concat (emit* (first args) env) [0xa7]
                            (emit* (second args) env) [0xa7]
                            [({'i32-shift-left 0x74 'i32-shift-right 0x75 'u32-shift-right 0x76} op)
                             (if (= op 'u32-shift-right) 0xad 0xac)])
                    (= op 'string-byte-length)
                    (concat (i32-const (descriptor-id :string)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-count)])
                    (= op 'string-concat)
                    (concat (i32-const (descriptor-id :string))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-string-concat)])
                    (= op 'string-substring)
                    (concat (i32-const (descriptor-id :string))
                            (emit* (first args) env) (emit* (second args) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'typed-string-substring)])
                    (= op 'string-replace-all)
                    (concat (i32-const (descriptor-id :string))
                            (emit* (first args) env) (emit* (second args) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'typed-string-replace-all)])
                    (= op 'string-contains?)
                    (concat (i32-const (descriptor-id :string))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-string-contains) 0xad])
                    (= op 'string-code-point-at)
                    (concat (i32-const (descriptor-id :string))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-string-code-point-at) 0xad])
                    (= op 'string-fold-case)
                    (concat (i32-const (descriptor-id :string)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-string-fold-case)])
                    (= op 'keyword-name)
                    (concat (i32-const (descriptor-id :keyword)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-keyword-name)])
                    (= op 'keyword-from-string)
                    (concat (i32-const (descriptor-id :keyword)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-keyword-from-string)])
                    (= op 'symbol)
                    (concat (i32-const (descriptor-id :symbol)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-symbol-from-string)])
                    (= op 'vector-new)
                    (emit-builder :vector-i64 -1 args (repeat (count args) :i64) env)
                    (= op 'vector-count)
                    (let [value-type
                          (typed/infer-type
                           (first args)
                           (into {} (map (fn [[key item]] [key (:type item)]) env))
                           signatures)
                          countable?
                          (or (= :vector-i64 value-type)
                              (and (vector? value-type)
                                   (= 2 (count value-type))
                                   (= :list (first value-type))))]
                      (when-not countable?
                        (throw
                         (ex-info "vector-count requires a canonical list"
                                  {:phase :wasm-typed-lowering
                                   :type value-type})))
                      (concat (i32-const (descriptor-id value-type))
                              (emit* (first args) env)
                              [0x10 (get intrinsic-indices 'typed-count)]))
                    (= op 'vector-at)
                    (concat (i32-const (descriptor-id :vector-i64))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-vector-at-i64)])
                    (= op 'vector-get)
                    (let [[value index fallback] args
                          value-local (allocate! 0x6f)
                          index-local (allocate! 0x7e)]
                      (concat (emit* value env) [::local-set value-local]
                              (emit* index env) [::local-set index-local]
                              [::local-get index-local 0x42 0 0x59 ::local-get index-local]
                              (i32-const (descriptor-id :vector-i64)) [::local-get value-local]
                              [0x10 (get intrinsic-indices 'typed-count) 0x54 0x71 0x04 0x7e]
                              (i32-const (descriptor-id :vector-i64))
                              [::local-get value-local ::local-get index-local
                               0x10 (get intrinsic-indices 'typed-vector-at-i64) 0x05]
                              (emit* fallback env) [0x0b]))
                    (= op 'vector-drop)
                    (concat (i32-const (descriptor-id :vector-i64))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-vector-drop)])
                    (= op 'vector-assoc)
                    (concat (i32-const (descriptor-id :vector-i64))
                            (emit* (first args) env) (emit* (second args) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'typed-vector-assoc-i64)])
                    (= op 'vector-conj)
                    (concat (i32-const (descriptor-id :vector-i64))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-vector-conj-i64)])
                    (= op 'vector-f64-new)
                    (emit-builder :vector-f64 -1 args (repeat (count args) :f64) env)
                    (= op 'vector-f64-count)
                    (concat (i32-const (descriptor-id :vector-f64)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-count)])
                    (= op 'string-index-new)
                    (concat (i32-const (descriptor-id :string-index))
                            [0x10 (get intrinsic-indices 'typed-string-index-new)])
                    (= op 'string-index-count)
                    (concat (i32-const (descriptor-id :string-index)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-count)])
                    (= op 'string-index-contains)
                    (emit-bool
                     (concat (i32-const (descriptor-id :string-index))
                             (emit* (first args) env) (emit* (second args) env)
                             [0x10 (get intrinsic-indices 'typed-string-index-contains)]))
                    (= op 'string-index-get)
                    (concat (i32-const (descriptor-id :string-index))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-string-index-get)])
                    (= op 'string-index-assoc)
                    (concat (i32-const (descriptor-id :string-index))
                            (emit* (first args) env) (emit* (second args) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'typed-string-index-assoc)])
                    (= op 'disjoint-set-i64-new)
                    (concat (i32-const (descriptor-id :disjoint-set-i64))
                            (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-disjoint-set-i64-new)])
                    (= op 'disjoint-set-i64-count)
                    (concat (i32-const (descriptor-id :disjoint-set-i64))
                            (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-count)])
                    (= op 'disjoint-set-i64-union)
                    (concat (i32-const (descriptor-id :disjoint-set-i64))
                            (emit* (first args) env) (emit* (second args) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'typed-disjoint-set-i64-union)])
                    (= op 'document-null)
                    (concat (i32-const (descriptor-id :document))
                            [0x10 (get intrinsic-indices 'typed-document-null)])
                    (contains? '#{document-bool document-i64 document-f64
                                  document-string document-keyword} op)
                    (concat (i32-const (descriptor-id :document))
                            (emit* (first args) env)
                            [0x10 (get intrinsic-indices
                                       ({'document-bool 'typed-document-bool
                                         'document-i64 'typed-document-i64
                                         'document-f64 'typed-document-f64
                                         'document-string 'typed-document-string
                                         'document-keyword 'typed-document-keyword} op))])
                    (= op 'document-vector)
                    (emit-builder :document -1 args (repeat (count args) :document) env)
                    (= op 'document-map)
                    (emit-builder :document -2 args
                                  (map-indexed (fn [index _]
                                                 (if (even? index) :keyword :document)) args) env)
                    (= op 'document-count)
                    (concat (i32-const (descriptor-id :document)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-count)])
                    (= op 'document-kind)
                    (concat (i32-const (descriptor-id :document)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-document-kind)])
                    (contains? '#{document-vector-at document-map-entry-at document-vector-assoc
                                  document-vector-conj document-vector-drop
                                  document-vector-remove} op)
                    (concat (i32-const (descriptor-id :document))
                            (mapcat #(emit* % env) args)
                            [0x10 (get intrinsic-indices
                                       ({'document-vector-at 'typed-document-vector-at
                                         'document-map-entry-at 'typed-document-map-entry-at
                                         'document-vector-assoc 'typed-document-vector-assoc
                                         'document-vector-conj 'typed-document-vector-conj
                                         'document-vector-drop 'typed-document-vector-drop
                                         'document-vector-remove 'typed-document-vector-remove} op))])
                    (= op 'document-contains)
                    (emit-bool
                     (concat (i32-const (descriptor-id :document))
                             (emit* (first args) env) (emit* (second args) env)
                             [0x10 (get intrinsic-indices 'typed-document-contains)]))
                    (= op 'document-equal?)
                    (emit-bool
                     (concat (i32-const (descriptor-id :document))
                             (emit* (first args) env) (emit* (second args) env)
                             [0x10 (get intrinsic-indices 'typed-equal)]))
                    (= op 'document-sha256)
                    (concat (i32-const (descriptor-id :document)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-document-sha256)])
                    (= op 'document-print)
                    (concat (i32-const (descriptor-id :document)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-document-print)])
                    (= op 'document-read)
                    (concat (i32-const (descriptor-id :document)) (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'typed-document-read)])
                    (contains? '#{document-get document-assoc document-dissoc
                                  document-merge document-string-value document-bool-value
                                  document-keyword-value document-i64-value document-f64-value} op)
                    (concat (i32-const (descriptor-id :document))
                            (mapcat #(emit* % env) args)
                            [0x10 (get intrinsic-indices
                                       ({'document-get 'typed-document-get
                                         'document-assoc 'typed-document-assoc
                                         'document-dissoc 'typed-document-dissoc
                                         'document-merge 'typed-document-merge
                                         'document-string-value 'typed-document-string-value
                                         'document-keyword-value 'typed-document-keyword-value
                                         'document-bool-value 'typed-document-bool-value
                                         'document-i64-value 'typed-document-i64-value
                                         'document-f64-value 'typed-document-f64-value} op))])
                    (= op 'vector-f64-at)
                    (concat (i32-const (descriptor-id :vector-f64))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-vector-at-f64)])
                    (= op 'vector-f64-get)
                    (let [[value index fallback] args
                          value-local (allocate! 0x6f)
                          index-local (allocate! 0x7e)]
                      (concat (emit* value env) [::local-set value-local]
                              (emit* index env) [::local-set index-local]
                              [::local-get index-local 0x42 0 0x59 ::local-get index-local]
                              (i32-const (descriptor-id :vector-f64)) [::local-get value-local]
                              [0x10 (get intrinsic-indices 'typed-count) 0x54 0x71 0x04 0x7c]
                              (i32-const (descriptor-id :vector-f64))
                              [::local-get value-local ::local-get index-local
                               0x10 (get intrinsic-indices 'typed-vector-at-f64) 0x05]
                              (emit* fallback env) [0x0b]))
                    (= op 'vector-f64-drop)
                    (concat (i32-const (descriptor-id :vector-f64))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-vector-drop)])
                    (= op 'vector-f64-assoc)
                    (concat (i32-const (descriptor-id :vector-f64))
                            (emit* (first args) env) (emit* (second args) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'typed-vector-assoc-f64)])
                    (= op 'vector-f64-conj)
                    (concat (i32-const (descriptor-id :vector-f64))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-vector-conj-f64)])
                    (= op 'string=?)
                    (concat (i32-const (descriptor-id :string))
                            (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'typed-equal) 0xad])
                    (= op 'bool-not)
                    (emit-bool (concat (emit-test (first args) env) [0x45]))
                    (contains? '#{option-some option-none} op)
                    (let [payload (when (= op 'option-some) args)]
                      (emit-builder :option-i64
                                    (if (= op 'option-some) 1 0)
                                    payload
                                    (if payload [:i64] [])
                                    env))
                    (contains? '#{result-ok result-err} op)
                    (emit-builder :result-i64
                                  (if (= op 'result-ok) 1 0)
                                  args [:i64] env)
                    (contains? '#{option-some? result-ok?} op)
                    (let [type (if (= op 'option-some?)
                                 :option-i64 :result-i64)]
                      (emit-bool
                       (concat (i32-const (descriptor-id type))
                               (emit* (first args) env)
                               [0x10 (get intrinsic-indices 'typed-tag)])))
                    (contains? '#{option-value result-value result-error} op)
                    (let [[value fallback] args
                          type (if (= op 'option-value)
                                 :option-i64 :result-i64)
                          wanted (if (= op 'result-error) 0 1)
                          value-local (allocate! 0x6f)]
                      (concat (emit* value env) [::local-set value-local]
                              (i32-const (descriptor-id type))
                              [::local-get value-local
                               0x10 (get intrinsic-indices 'typed-tag)]
                              (i32-const wanted) [0x46 0x04 0x7e]
                              (emit-get type {:wasm-local value-local} 0 :i64 env)
                              [0x05] (emit* fallback env) [0x0b]))
                    (contains? '#{= < > <= >=} op)
                    (let [operand-type (typed/infer-type
                                        (first args)
                                        (into {} (map (fn [[key item]] [key (:type item)]) env))
                                        signatures)]
                      (if (= operand-type :i64)
                        (concat (emit* (first args) env) (emit* (second args) env)
                                [({'= 0x51 '< 0x53 '> 0x55 '<= 0x57 '>= 0x59} op)
                                 0xad])
                        (if (= op '=)
                          (emit-equal operand-type (first args) (second args) env)
                          (throw (ex-info "typed Wasm ordering requires i64 operands"
                                          {:phase :wasm-typed-lowering
                                           :operation op :type operand-type})))))
                    (contains? '#{option-some-of option-none-of} op)
                    (let [[type & payload] args]
                      (emit-builder type (if (= op 'option-some-of) 1 0) payload
                                    (if (seq payload) [(second type)] []) env))
                    (contains? '#{result-ok-of result-err-of} op)
                    (let [[type payload] args
                          tag (if (= op 'result-ok-of) 1 0)]
                      (emit-builder type tag [payload]
                                    [(if (= tag 1) (second type) (nth type 2))] env))
                    (= op 'variant-new)
                    (let [[type tag payload] args
                          index (first (keep-indexed #(when (= tag (first %2)) %1)
                                                     (nth type 2)))
                          payload-type (second (nth (nth type 2) index))]
                      (emit-builder type index [payload] [payload-type] env))
                    (= op 'hetero-vector-new)
                    (let [[type & items] args]
                      (emit-builder type -1 items (second type) env))
                    (= op 'typed-set-new)
                    (let [[type & items] args]
                      (emit-builder type -1 items (repeat (count items) (second type)) env))
                    (= op 'typed-map-new)
                    (let [[type & items] args]
                      (emit-builder type -1 items
                                    (take (count items) (cycle [(second type) (nth type 2)])) env))
                    (= op 'record-new)
                    (let [[type & items] args]
                      (emit-builder type -1 items (map second (nth type 2)) env))
                    (= op 'option-match)
                    (let [[type value none-body binder some-body] args
                          value-local (allocate! 0x6f)
                          binder-type (second type)
                          binder-local (allocate! (wasm-type binder-type))
                          result-type (typed/infer-type none-body
                                                       (into {} (map (fn [[key item]] [key (:type item)]) env))
                                                       signatures)]
                      (concat (emit* value env) [::local-set value-local]
                              (i32-const (descriptor-id type)) [::local-get value-local]
                              [0x10 (get intrinsic-indices 'typed-tag) 0x04
                               (wasm-type result-type)]
                              (emit-get type {:wasm-local value-local} 0 binder-type env)
                              [::local-set binder-local]
                              (emit* some-body (assoc env binder {:index binder-local :type binder-type}))
                              [0x05] (emit* none-body env) [0x0b]))
                    (= op 'result-match-of)
                    (let [[type value ok-name ok-body err-name err-body] args
                          value-local (allocate! 0x6f)
                          ok-type (second type)
                          err-type (nth type 2)
                          ok-local (allocate! (wasm-type ok-type))
                          err-local (allocate! (wasm-type err-type))
                          result-type (typed/infer-type ok-body
                                                       (assoc (into {} (map (fn [[key item]] [key (:type item)]) env))
                                                              ok-name ok-type)
                                                       signatures)]
                      (concat (emit* value env) [::local-set value-local]
                              (i32-const (descriptor-id type)) [::local-get value-local]
                              [0x10 (get intrinsic-indices 'typed-tag) 0x04
                               (wasm-type result-type)]
                              (emit-get type {:wasm-local value-local} 0 ok-type env)
                              [::local-set ok-local]
                              (emit* ok-body (assoc env ok-name {:index ok-local :type ok-type}))
                              [0x05]
                              (emit-get type {:wasm-local value-local} 0 err-type env)
                              [::local-set err-local]
                              (emit* err-body (assoc env err-name {:index err-local :type err-type}))
                              [0x0b]))
                    (= op 'variant-match) (emit-match (first args) (second args) (nth args 2) env)
                    (= op 'hetero-vector-at)
                    (let [[type value index] args
                          item-type (nth (second type) index)]
                      (emit-get type value index item-type env))
                    (= op 'record-get)
                    (let [[type value field] args
                          index (first (keep-indexed #(when (= field (first %2)) %1) (nth type 2)))
                          item-type (second (nth (nth type 2) index))]
                      (emit-get type value index item-type env))
                    (contains? '#{option-some?-of result-ok?-of} op)
                    (let [[type value] args]
                      (emit-bool (concat (i32-const (descriptor-id type)) (emit* value env)
                                         [0x10 (get intrinsic-indices 'typed-tag)])))
                    (contains? '#{option-value-of result-value-of result-error-of} op)
                    (let [[type value fallback] args
                          wanted (if (= op 'result-error-of) 0 1)
                          payload-type (case op
                                         option-value-of (second type)
                                         result-value-of (second type)
                                         result-error-of (nth type 2))
                          value-local (allocate! 0x6f)]
                      (concat (emit* value env) [::local-set value-local]
                              (i32-const (descriptor-id type)) [::local-get value-local]
                              [0x10 (get intrinsic-indices 'typed-tag)]
                              (i32-const wanted) [0x46 0x04 (wasm-type payload-type)]
                              (emit-get type {:wasm-local value-local} 0 payload-type env)
                              [0x05] (emit* fallback env) [0x0b]))
                    (= op 'hetero-vector-assoc)
                    (let [[type value index replacement] args]
                      (emit-assoc type value index replacement (nth (second type) index) env))
                    (= op 'record-assoc)
                    (let [[type value field replacement] args
                          index (first (keep-indexed #(when (= field (first %2)) %1) (nth type 2)))
                          replacement-type (second (nth (nth type 2) index))]
                      (emit-assoc type value index replacement replacement-type env))
                    (contains? '#{hetero-vector-equal typed-set-equal typed-map-equal record-equal} op)
                    (let [[type left right] args] (emit-equal type left right env))
                    (contains? '#{typed-set-contains typed-set-conj typed-set-disj} op)
                    (let [[type value item] args
                          item-type (second type)
                          operation ({'typed-set-contains 0 'typed-set-conj 1 'typed-set-disj 2} op)
                          contains? (= op 'typed-set-contains)
                          code (concat (i32-const (descriptor-id type)) (emit* value env)
                                       (when-not contains? (i32-const operation)) (emit* item env)
                                       [0x10 (get intrinsic-indices
                                                  (if contains?
                                                    (if (= item-type :i64)
                                                      'typed-set-contains-i64 'typed-set-contains-ref)
                                                    (if (= item-type :i64)
                                                      'typed-set-op-i64 'typed-set-op-ref)))])]
                      (if (= op 'typed-set-contains) (emit-bool code) code))
                    (contains? '#{hetero-vector-count typed-set-count} op)
                    (let [[type value] args]
                      (concat (i32-const (descriptor-id type)) (emit* value env)
                              [0x10 (get intrinsic-indices 'typed-count)]))
                    (= op 'typed-map-count)
                    (let [[type value] args]
                      (concat (i32-const (descriptor-id type)) (emit* value env)
                              [0x10 (get intrinsic-indices 'typed-count)]))
                    (contains? '#{typed-map-contains typed-map-get typed-map-dissoc} op)
                    (let [[type value key] args
                          key-type (second type)
                          prefix (case op
                                   typed-map-contains "typed-map-contains-"
                                   typed-map-get "typed-map-get-"
                                   typed-map-dissoc "typed-map-dissoc-")
                          intrinsic (symbol (str prefix (if (= key-type :i64) "i64" "ref")))
                          code (concat (i32-const (descriptor-id type)) (emit* value env)
                                       (emit* key env) [0x10 (get intrinsic-indices intrinsic)])]
                      (if (= op 'typed-map-contains) (emit-bool code) code))
                    (= op 'typed-map-entry-at)
                    (let [[type value index] args]
                      (concat (i32-const (descriptor-id type)) (emit* value env)
                              (emit* index env)
                              [0x10 (get intrinsic-indices 'typed-map-entry-at)]))
                    (= op 'typed-map-assoc)
                    (let [[type value key item] args
                          key-code (if (= (second type) :i64) "i" "r")
                          item-code (if (= (nth type 2) :i64) "i" "r")
                          intrinsic (symbol (str "typed-map-assoc-" key-code item-code))]
                      (concat (i32-const (descriptor-id type)) (emit* value env)
                              (emit* key env) (emit* item env)
                              [0x10 (get intrinsic-indices intrinsic)]))
                    (= op 'xml-path-count)
                    (concat (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'xml-path-count)])
                    (= op 'xml-name-count)
                    (concat (emit* (first args) env) (emit* (second args) env)
                            [0x10 (get intrinsic-indices 'xml-name-count)])
                    (= op 'xml-name-text)
                    (concat (emit* (nth args 0) env) (emit* (nth args 1) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'xml-name-text)])
                    (= op 'xml-path-text)
                    (concat (emit* (nth args 0) env) (emit* (nth args 1) env)
                            (emit* (nth args 2) env)
                            [0x10 (get intrinsic-indices 'xml-path-text)])
                    (= op 'xml-path-attr)
                    (concat (emit* (nth args 0) env) (emit* (nth args 1) env)
                            (emit* (nth args 2) env) (emit* (nth args 3) env)
                            [0x10 (get intrinsic-indices 'xml-path-attr)])
                    (= op 'decimal-f64-parse)
                    (concat (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'decimal-f64-parse)])
                    (= op 'decimal-f64x3-parse)
                    (concat (emit* (first args) env)
                            [0x10 (get intrinsic-indices 'decimal-f64x3-parse)])
                    :else
                    (if-let [function-index (get function-indices op)]
                      (concat (mapcat #(emit* % env) args) [0x10 function-index])
                      (throw (ex-info "typed Wasm operation is not qualified"
                                      {:phase :wasm-typed-lowering
                                       :operation op :form form})))))))]
      ;; `prefix` and `body-code` are lazy seqs whose realization is what runs
      ;; `allocate!`'s side effects into `@locals`. Force them with `doall`
      ;; BEFORE reading `@locals` for the locals declaration below -- otherwise
      ;; the declared count is taken from a partially-realized `@locals` and
      ;; undercounts scratch locals (e.g. a body needing 6 f64 temporaries
      ;; declared only 5, producing `invalid local index: 5` at instantiation).
      (let [prefix (doall
                    (mapcat (fn [[index type]]
                              (cond
                                (and component-canonical-scalars?
                                     (= :bool type)
                                     (not (contains? unchecked-bool-param-indices index)))
                                [::local-get index 0x41 1 0x4b 0x04 0x40 0x00 0x0b]
                                (reference-type? type)
                                (concat (i32-const (descriptor-id type)) [::local-get index]
                                        [0x10 (get intrinsic-indices 'typed-assert-ref)
                                         ::local-set index])))
                            (map-indexed vector (:param-types function))))
            body-code (doall (emit* (:body function) env))
            body-code (doall
                       (if (reference-type? (:result function))
                         (concat (i32-const (descriptor-id (:result function))) body-code
                                 [0x10 (get intrinsic-indices 'typed-assert-ref)])
                         body-code))
            declarations (if (empty? @locals) [0]
                           (concat (uleb (count @locals))
                                   (mapcat (fn [type] [1 type]) @locals)))
            charge [0x23 0 0x50 0x04 0x40 0x00 0x0b
                    0x23 0 0x42 1 0x7d 0x24 0]
            instructions (encode-local-operands (concat prefix charge body-code))
            body (concat declarations instructions [0x0b])]
        (concat (uleb (count body)) body)))))

(defn- function-type [{:keys [params]}]
  (concat [0x60] (uleb (count params)) (repeat (count params) 0x7e) [1 0x7e]))

(defn- function-body [function function-indices intrinsic-indices]
  (let [param-env (zipmap (:params function) (range))
        locals (local-count (:body function))
        declarations (if (zero? locals) [0] (concat [1] (uleb locals) [0x7e]))
        ;; Every call consumes one unit from a module-private monotonic fuel
        ;; global. It is never exported and cannot be replenished by guest code.
        charge [0x23 0 0x50 0x04 0x40 0x00 0x0b ; global.get;eqz;if;unreachable;end
                0x23 0 0x42 1 0x7d 0x24 0]       ; global.get;const 1;sub;global.set
        instructions (encode-local-operands
                      (concat charge (emit-expr (:body function) param-env
                                      {:function-indices function-indices
                                       :intrinsic-indices intrinsic-indices
                                       :next-local (count (:params function))})))
        body (concat declarations instructions [0x0b])]
    (concat (uleb (count body)) body)))

(defn- uses-operation? [functions operations]
  (boolean
   (some (fn [function]
           (some #(and (seq? %) (contains? operations (first %)))
                 (tree-seq coll? seq (:body function))))
         functions)))

(def default-fuel
  "Historical fixed call budget. Every caller that supplies no `:fuel` gets
  exactly this, so core-wasm behaviour is unchanged by fuel parameterization."
  512)

(def max-fuel
  "Upper bound on a declared fuel budget. The charge is a single i64
  `global.get`/`sub`, so the representable ceiling is i64; this bound keeps a
  declared budget inside a value the SLEB128 encoder and every host that
  reports remaining fuel can carry without ambiguity."
  (dec (bit-shift-left 1 62)))

(defn- fuel-budget! [fuel]
  (cond
    (nil? fuel) default-fuel
    (not (integer? fuel))
    (throw (ex-info "fuel budget must be an integer"
                    {:phase :wasm-emit :fuel fuel}))
    (not (pos? fuel))
    (throw (ex-info "fuel budget must be positive"
                    {:phase :wasm-emit :fuel fuel}))
    (> fuel max-fuel)
    (throw (ex-info "fuel budget exceeds the representable ceiling"
                    {:phase :wasm-emit :fuel fuel :max max-fuel}))
    :else fuel))

;; --- component linear memory -------------------------------------------------
;; ADR 0076 section 4a. A component cannot import `kotoba:typed`/`kotoba:heap`,
;; so anything that is not a bare scalar has to live in the module's own linear
;; memory -- which is why all sixteen hand-written WAT shapes build their data
;; with i32.store/i64.store rather than calling the host. Until now the emitted
;; component declared a ZERO-page memory and a `cm32p2_realloc` whose whole body
;; was `i32.const 0`: correct for a scalar-only signature, useless for anything
;; else, and worse than useless if the Canonical ABI ever called it (address 0
;; in a 0-page memory traps).
;;
;; This is the binary port of `component-core/bounded-bump-realloc-wat`, with
;; the same properties: alignment-respecting, capacity-trapping, and
;; old-content-preserving on grow. The Canonical ABI's own string-copy
;; machinery calls realloc an unpredictable number of times before a module's
;; own body runs, so the allocator has to compose with those calls rather than
;; assume it owns every allocation.

(def ^:private wasm-page-bytes 65536)

(def component-memory-pages
  "Pages of linear memory a component module declares.

  ADR 0077 decision 3 sizes this from the language's own bound rather than a
  round number: `value/vector-item-limit` is 16384 items, so one maximum
  `:vector-i64` is 128 KiB of live data -- two pages exactly. A named
  capability can keep its lowered request live while lifting an equally large
  result into the same module, so the arena covers two maximum vectors. The
  extra page is headroom for result records and the Canonical ABI's other
  allocations.

  Derived here rather than written as a literal so it cannot drift from the
  item limit it exists to cover."
  (+ 1 (* 2 (quot (+ (* 16384 8) (dec wasm-page-bytes)) wasm-page-bytes))))

(defn- component-memory-budget! [pages]
  (let [pages (or pages 16)]
    (when-not (and (integer? pages)
                   (<= component-memory-pages pages 65536))
      (throw (ex-info "component memory budget must cover the language arena"
                      {:phase :wasm-emit :memory-pages pages
                       :minimum component-memory-pages :maximum 65536})))
    pages))

(def component-arena-base
  "First address the bump allocator hands out. Not 0: the Canonical ABI uses a
  null `old-ptr` to mean `fresh allocation`, so address 0 must never be a live
  pointer."
  8)

(def component-arena-capacity
  "The bump allocator's ceiling. Kept in lockstep with the declared memory: a
  capacity above it would let a write run off the memory, and one below it would
  waste a declared page. `component-heap-test` asserts the two agree, because
  they are edited in different places and nothing else would notice them
  drifting apart."
  (* component-memory-pages wasm-page-bytes))

(defn- component-realloc-body
  "cm32p2_realloc: (old-ptr, old-size, align, new-size) -> ptr.
  Params 0..3; locals 4=ptr, 5=end, 6=copy-size."
  []
  (let [body (concat
              ;; three i32 locals
              [1 3 0x7f]
              ;; new-size == 0 -> return 0
              [0x20 3 0x45 0x04 0x40 0x41 0x00 0x0f 0x0b]
              ;; align must be non-zero, <= 8, and a power of two
              [0x20 2 0x45 0x04 0x40 0x00 0x0b]
              [0x20 2 0x41 0x08 0x4b 0x04 0x40 0x00 0x0b]
              [0x20 2 0x20 2 0x41 0x01 0x6b 0x71 0x04 0x40 0x00 0x0b]
              ;; ptr = (next + align - 1) & -align
              [0x23 1 0x20 2 0x41 0x01 0x6b 0x6a
               0x41 0x00 0x20 2 0x6b 0x71 0x22 4]
              ;; end = ptr + new-size; trap on wrap
              [0x20 3 0x6a 0x22 5 0x20 4 0x49 0x04 0x40 0x00 0x0b]
              ;; trap past capacity
              (concat [0x20 5 0x41] (sleb component-arena-capacity)
                      [0x4b 0x04 0x40 0x00 0x0b])
              ;; next = end
              [0x20 5 0x24 1]
              ;; if old-ptr != 0, copy min(old-size, new-size) bytes
              [0x20 0 0x45 0x04 0x40 0x05
               0x20 1 0x20 3 0x49 0x04 0x7f 0x20 1 0x05 0x20 3 0x0b 0x21 6
               0x20 4 0x20 0 0x20 6 0xfc 0x0a 0x00 0x00
               0x0b]
              ;; return ptr
              [0x20 4 0x0b])]
    (concat (uleb (count body)) body)))

(defn emit
  ([kir target] (emit kir target {}))
  ([kir target {:keys [component-standard32? fuel memory-pages capability-imports
                       core-param-types component-canonical-scalars?]
                :as opts}]
  (let [fuel-initial (fuel-budget! fuel)
        memory-maximum (component-memory-budget! memory-pages)
        functions (:functions kir)
        typed? (= :kotoba.kir/v4 (:format kir))
        emitted-wasm-type (fn [type]
                            (if (and component-canonical-scalars? (= :bool type))
                              0x7f
                              (typed/wasm-type type)))
        has-cap? (uses-operation? functions '#{cap-call})
        has-typed-cap? (uses-operation? functions '#{typed-cap-call})
        _named-capability
        (when (and component-canonical-scalars?
                   has-typed-cap?
                   (empty? capability-imports))
          (throw
           (ex-info
            "canonical scalar Component capability requires a named import"
            {:phase :wasm-component-scalar-lowering})))
        _ (when (and component-canonical-scalars?
                     (typed/requires-host-runtime? kir {:native-bool? true}))
            (throw (ex-info "canonical scalar Component adapter requires a host value"
                            {:phase :wasm-component-scalar-lowering})))
        exported-names (set (or (:exports kir) (map :name functions)))
        exported-functions (filterv #(contains? exported-names (:name %)) functions)
        ;; Effects describe authority requirements, but imports must be
        ;; derived from executable operations. Component normalization keeps
        ;; the original :cap/call effect while replacing the generic operation
        ;; with a named typed-cap-call; deriving this from effects would then
        ;; reintroduce an ambient, unbindable generic import.
        heap-ops (let [found (volatile! #{})]
                   (letfn [(walk [form]
                             (cond
                               (seq? form)
                               (do
                                 (when (contains? '#{pair pair-first pair-second} (first form))
                                   (vswap! found conj (first form)))
                                 (doseq [arg (rest form)] (walk arg)))
                               (coll? form) (doseq [item form] (walk item))))]
                     (doseq [function functions] (walk (:body function)))
                     @found))
        has-xml? (uses-operation? functions
                                  '#{xml-path-count xml-name-count xml-name-text
                                     xml-path-text xml-path-attr})
        has-decimal? (uses-operation? functions '#{decimal-f64-parse})
        has-decimal-x3? (uses-operation? functions '#{decimal-f64x3-parse})
        has-string-index? (uses-operation? functions
                                            '#{string-index-new string-index-count
                                               string-index-contains string-index-get
                                               string-index-assoc})
        has-string-concat? (uses-operation? functions '#{string-concat})
        has-string-substring? (uses-operation? functions '#{string-substring})
        has-string-replace? (uses-operation? functions '#{string-replace-all})
        has-string-contains? (uses-operation? functions '#{string-contains?})
        has-string-code-point-at? (uses-operation? functions '#{string-code-point-at})
        has-string-fold-case? (uses-operation? functions '#{string-fold-case})
        has-keyword-name? (uses-operation? functions '#{keyword-name})
        has-disjoint-set? (uses-operation? functions
                                            '#{disjoint-set-i64-new disjoint-set-i64-count
                                               disjoint-set-i64-union})
        has-document? (uses-operation? functions
                                       '#{document-null document-bool document-i64 document-f64
                                          document-string document-keyword document-vector document-map
                                          document-count document-kind document-vector-at document-map-entry-at document-vector-assoc
                                          document-vector-conj document-vector-drop document-vector-remove
                                          document-equal? document-sha256 document-print document-read document-contains document-get document-assoc
                                          document-dissoc document-merge document-string-value
                                          document-keyword-value document-bool-value
                                          document-i64-value document-f64-value})
        has-keyword-from-string? (uses-operation? functions '#{keyword-from-string})
        has-symbol-from-string? (uses-operation? functions '#{symbol})
        typed-imports (when (and typed?
                                 (not component-canonical-scalars?)
                                 (typed/requires-host-runtime? kir))
                        (vec (concat
                         [['typed-literal "kotoba:typed" "literal" [0x60 1 0x7f 1 0x6f]]
                         ['typed-new "kotoba:typed" "new" [0x60 2 0x7f 0x7f 1 0x6f]]
                         ['typed-push-i64 "kotoba:typed" "push-i64" [0x60 2 0x6f 0x7e 1 0x6f]]
                         ['typed-push-f64 "kotoba:typed" "push-f64" [0x60 2 0x6f 0x7c 1 0x6f]]
                         ['typed-push-f32 "kotoba:typed" "push-f32" [0x60 2 0x6f 0x7d 1 0x6f]]
                         ['typed-push-ref "kotoba:typed" "push-ref" [0x60 2 0x6f 0x6f 1 0x6f]]
                         ['typed-seal "kotoba:typed" "seal" [0x60 2 0x7f 0x6f 1 0x6f]]
                         ['typed-assert-ref "kotoba:typed" "assert-ref" [0x60 2 0x7f 0x6f 1 0x6f]]
                         ['typed-tag "kotoba:typed" "tag" [0x60 2 0x7f 0x6f 1 0x7f]]
                         ['typed-get-i64 "kotoba:typed" "get-i64" [0x60 3 0x7f 0x6f 0x7f 1 0x7e]]
                         ['typed-get-f64 "kotoba:typed" "get-f64" [0x60 3 0x7f 0x6f 0x7f 1 0x7c]]
                         ['typed-get-f32 "kotoba:typed" "get-f32" [0x60 3 0x7f 0x6f 0x7f 1 0x7d]]
                         ['typed-get-ref "kotoba:typed" "get-ref" [0x60 3 0x7f 0x6f 0x7f 1 0x6f]]
                         ['typed-count "kotoba:typed" "count" [0x60 2 0x7f 0x6f 1 0x7e]]
                         ['typed-bool "kotoba:typed" "bool" [0x60 1 0x7f 1 0x6f]]
                         ['typed-equal "kotoba:typed" "equal" [0x60 3 0x7f 0x6f 0x6f 1 0x7f]]]
                         (when has-string-concat?
                           [['typed-string-concat "kotoba:typed" "string-concat"
                             [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]])
                         (when has-string-substring?
                           [['typed-string-substring "kotoba:typed" "string-substring"
                             [0x60 4 0x7f 0x6f 0x7e 0x7e 1 0x6f]]])
                         (when has-string-replace?
                           [['typed-string-replace-all "kotoba:typed" "string-replace-all"
                             [0x60 4 0x7f 0x6f 0x6f 0x6f 1 0x6f]]])
                         (when has-string-contains?
                           [['typed-string-contains "kotoba:typed" "string-contains"
                             [0x60 3 0x7f 0x6f 0x6f 1 0x7f]]])
                         (when has-string-code-point-at?
                           [['typed-string-code-point-at "kotoba:typed" "string-code-point-at"
                             [0x60 3 0x7f 0x6f 0x7e 1 0x7f]]])
                         (when has-string-fold-case?
                           [['typed-string-fold-case "kotoba:typed" "string-fold-case"
                             [0x60 2 0x7f 0x6f 1 0x6f]]])
                         (when has-keyword-name?
                           [['typed-keyword-name "kotoba:typed" "keyword-name"
                             [0x60 2 0x7f 0x6f 1 0x6f]]])
                         [
                         ['typed-assoc-i64 "kotoba:typed" "assoc-i64" [0x60 4 0x7f 0x6f 0x7f 0x7e 1 0x6f]]
                         ['typed-assoc-f64 "kotoba:typed" "assoc-f64" [0x60 4 0x7f 0x6f 0x7f 0x7c 1 0x6f]]
                         ['typed-assoc-f32 "kotoba:typed" "assoc-f32" [0x60 4 0x7f 0x6f 0x7f 0x7d 1 0x6f]]
                         ['typed-assoc-ref "kotoba:typed" "assoc-ref" [0x60 4 0x7f 0x6f 0x7f 0x6f 1 0x6f]]
                         ['typed-vector-drop "kotoba:typed" "vector-drop" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                         ['typed-vector-at-i64 "kotoba:typed" "vector-at-i64" [0x60 3 0x7f 0x6f 0x7e 1 0x7e]]
                         ['typed-vector-assoc-i64 "kotoba:typed" "vector-assoc-i64" [0x60 4 0x7f 0x6f 0x7e 0x7e 1 0x6f]]
                         ['typed-vector-conj-i64 "kotoba:typed" "vector-conj-i64" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                         ['typed-vector-at-f64 "kotoba:typed" "vector-at-f64" [0x60 3 0x7f 0x6f 0x7e 1 0x7c]]
                         ['typed-vector-assoc-f64 "kotoba:typed" "vector-assoc-f64" [0x60 4 0x7f 0x6f 0x7e 0x7c 1 0x6f]]
                         ['typed-vector-conj-f64 "kotoba:typed" "vector-conj-f64" [0x60 3 0x7f 0x6f 0x7c 1 0x6f]]
                         ['typed-set-op-i64 "kotoba:typed" "set-op-i64" [0x60 4 0x7f 0x6f 0x7f 0x7e 1 0x6f]]
                         ['typed-set-op-ref "kotoba:typed" "set-op-ref" [0x60 4 0x7f 0x6f 0x7f 0x6f 1 0x6f]]
                         ['typed-set-contains-i64 "kotoba:typed" "set-contains-i64" [0x60 3 0x7f 0x6f 0x7e 1 0x7f]]
                         ['typed-set-contains-ref "kotoba:typed" "set-contains-ref" [0x60 3 0x7f 0x6f 0x6f 1 0x7f]]
                         ['typed-map-contains-i64 "kotoba:typed" "map-contains-i64" [0x60 3 0x7f 0x6f 0x7e 1 0x7f]]
                         ['typed-map-contains-ref "kotoba:typed" "map-contains-ref" [0x60 3 0x7f 0x6f 0x6f 1 0x7f]]
                         ['typed-map-get-i64 "kotoba:typed" "map-get-i64" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                         ['typed-map-get-ref "kotoba:typed" "map-get-ref" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]
                         ['typed-map-entry-at "kotoba:typed" "map-entry-at" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                         ['typed-map-assoc-ii "kotoba:typed" "map-assoc-ii" [0x60 4 0x7f 0x6f 0x7e 0x7e 1 0x6f]]
                         ['typed-map-assoc-ir "kotoba:typed" "map-assoc-ir" [0x60 4 0x7f 0x6f 0x7e 0x6f 1 0x6f]]
                         ['typed-map-assoc-ri "kotoba:typed" "map-assoc-ri" [0x60 4 0x7f 0x6f 0x6f 0x7e 1 0x6f]]
                         ['typed-map-assoc-rr "kotoba:typed" "map-assoc-rr" [0x60 4 0x7f 0x6f 0x6f 0x6f 1 0x6f]]
                         ['typed-map-dissoc-i64 "kotoba:typed" "map-dissoc-i64" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                         ['typed-map-dissoc-ref "kotoba:typed" "map-dissoc-ref" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]]
                         (when has-string-index?
                           [['typed-string-index-new "kotoba:typed" "string-index-new" [0x60 1 0x7f 1 0x6f]]
                            ['typed-string-index-contains "kotoba:typed" "string-index-contains" [0x60 3 0x7f 0x6f 0x6f 1 0x7f]]
                            ['typed-string-index-get "kotoba:typed" "string-index-get" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]
                            ['typed-string-index-assoc "kotoba:typed" "string-index-assoc" [0x60 4 0x7f 0x6f 0x6f 0x7e 1 0x6f]]])
                         (when has-disjoint-set?
                           [['typed-disjoint-set-i64-new "kotoba:typed" "disjoint-set-i64-new" [0x60 2 0x7f 0x7e 1 0x6f]]
                            ['typed-disjoint-set-i64-union "kotoba:typed" "disjoint-set-i64-union" [0x60 4 0x7f 0x6f 0x7e 0x7e 1 0x6f]]])
                         (when has-document?
                           [['typed-document-null "kotoba:typed" "document-null" [0x60 1 0x7f 1 0x6f]]
                            ['typed-document-bool "kotoba:typed" "document-bool" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-i64 "kotoba:typed" "document-i64" [0x60 2 0x7f 0x7e 1 0x6f]]
                            ['typed-document-f64 "kotoba:typed" "document-f64" [0x60 2 0x7f 0x7c 1 0x6f]]
                            ['typed-document-string "kotoba:typed" "document-string" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-keyword "kotoba:typed" "document-keyword" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-kind "kotoba:typed" "document-kind" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-sha256 "kotoba:typed" "document-sha256" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-print "kotoba:typed" "document-print" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-read "kotoba:typed" "document-read" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-vector-at "kotoba:typed" "document-vector-at" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                            ['typed-document-map-entry-at "kotoba:typed" "document-map-entry-at" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                            ['typed-document-vector-assoc "kotoba:typed" "document-vector-assoc" [0x60 4 0x7f 0x6f 0x7e 0x6f 1 0x6f]]
                            ['typed-document-vector-conj "kotoba:typed" "document-vector-conj" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]
                            ['typed-document-vector-drop "kotoba:typed" "document-vector-drop" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                            ['typed-document-vector-remove "kotoba:typed" "document-vector-remove" [0x60 3 0x7f 0x6f 0x7e 1 0x6f]]
                            ['typed-document-contains "kotoba:typed" "document-contains" [0x60 3 0x7f 0x6f 0x6f 1 0x7f]]
                            ['typed-document-get "kotoba:typed" "document-get" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]
                            ['typed-document-assoc "kotoba:typed" "document-assoc" [0x60 4 0x7f 0x6f 0x6f 0x6f 1 0x6f]]
                            ['typed-document-dissoc "kotoba:typed" "document-dissoc" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]
                            ['typed-document-merge "kotoba:typed" "document-merge" [0x60 3 0x7f 0x6f 0x6f 1 0x6f]]
                            ['typed-document-string-value "kotoba:typed" "document-string-value" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-keyword-value "kotoba:typed" "document-keyword-value" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-bool-value "kotoba:typed" "document-bool-value" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-i64-value "kotoba:typed" "document-i64-value" [0x60 2 0x7f 0x6f 1 0x6f]]
                            ['typed-document-f64-value "kotoba:typed" "document-f64-value" [0x60 2 0x7f 0x6f 1 0x6f]]])
                         (when has-keyword-from-string?
                           [['typed-keyword-from-string "kotoba:typed" "keyword-from-string"
                             [0x60 2 0x7f 0x6f 1 0x6f]]])
                         (when has-symbol-from-string?
                           [['typed-symbol-from-string "kotoba:typed" "symbol-from-string"
                             [0x60 2 0x7f 0x6f 1 0x6f]]])
                         (when has-xml?
                           [['xml-path-count "kotoba:typed" "xml-path-count" [0x60 2 0x6f 0x6f 1 0x7e]]
                            ['xml-name-count "kotoba:typed" "xml-name-count" [0x60 2 0x6f 0x6f 1 0x7e]]
                            ['xml-name-text "kotoba:typed" "xml-name-text" [0x60 3 0x6f 0x6f 0x7e 1 0x6f]]
                            ['xml-path-text "kotoba:typed" "xml-path-text" [0x60 3 0x6f 0x6f 0x7e 1 0x6f]]
                            ['xml-path-attr "kotoba:typed" "xml-path-attr" [0x60 4 0x6f 0x6f 0x7e 0x6f 1 0x6f]]])
                         (when has-decimal?
                           [['decimal-f64-parse "kotoba:typed" "decimal-f64-parse" [0x60 1 0x6f 1 0x6f]]])
                         (when has-decimal-x3?
                           [['decimal-f64x3-parse "kotoba:typed" "decimal-f64x3-parse" [0x60 1 0x6f 1 0x6f]]]))))
        imports (vec (concat typed-imports
                      (if (seq capability-imports)
                        (mapv (fn [{:keys [id module field type]}]
                                [[:capability id] module field type])
                              capability-imports)
                        (when has-typed-cap?
                          [['typed-cap-call "kotoba:typed" "cap-call"
                            [0x60 2 0x7f 0x6f 1 0x6f]]]))
                      (when has-cap? [['cap-call "kotoba:cap" "call"
                                       [0x60 2 0x7e 0x7e 1 0x7e]]])
                      (when (seq heap-ops)
                        [['pair "kotoba:heap" "pair" [0x60 2 0x7e 0x7e 1 0x7e]]
                         ['pair-first "kotoba:heap" "pair-first" [0x60 1 0x7e 1 0x7e]]
                         ['pair-second "kotoba:heap" "pair-second" [0x60 1 0x7e 1 0x7e]]])))
        shift (count imports)
        intrinsic-indices (into {} (map-indexed (fn [index [op]] [op index]) imports))
        indices (into {} (map-indexed (fn [i f] [(:name f) (+ i shift)]) functions))
        component-type-count (if component-standard32?
                               (+ (count exported-functions) 2)
                               0)
        component-type-base (+ (count functions) shift)
        post-type-indices (range component-type-base
                                 (+ component-type-base (count exported-functions)))
        realloc-type-index (+ component-type-base (count exported-functions))
        initialize-type-index (inc realloc-type-index)
        component-types (when component-standard32?
                          (concat
                           (mapcat (fn [{:keys [result]}]
                                     [0x60 1 (emitted-wasm-type result) 0])
                                   exported-functions)
                           [0x60 4 0x7f 0x7f 0x7f 0x7f 1 0x7f]
                           [0x60 0 0]))
        function-types
        (mapcat (fn [function]
                  (if-let [params (get core-param-types (:name function))]
                    (concat [0x60] (uleb (count params)) params
                            [1 (emitted-wasm-type (:result function))])
                    (if typed?
                      (if component-canonical-scalars?
                        (concat [0x60] (uleb (count (:param-types function)))
                                (map emitted-wasm-type (:param-types function))
                                [1 (emitted-wasm-type (:result function))])
                        (typed-function-type function))
                      (function-type function))))
                functions)
        types (concat (uleb (+ (count functions) shift component-type-count))
                      (mapcat #(nth % 3) imports)
                      function-types
                      component-types)
        import-sec (when (seq imports)
                     (concat (uleb shift)
                             (mapcat (fn [[_ module field _] index]
                                       (concat (name-bytes module) (name-bytes field)
                                               [0] (uleb index)))
                                     imports (range))))
        component-function-count (if component-standard32?
                                   (+ (count exported-functions) 2)
                                   0)
        function-sec (concat
                      (uleb (+ (count functions) component-function-count))
                      (mapcat uleb (range shift (+ shift (count functions))))
                      (when component-standard32?
                        (mapcat uleb (concat post-type-indices
                                             [realloc-type-index initialize-type-index]))))
        ;; (global (mut i64) (i64.const FUEL)); the module-private, guest-
        ;; unreplenishable call budget. It defaults to `default-fuel` (512),
        ;; which is low enough to trap before the host call stack becomes the
        ;; limiting resource -- the historical fixed value, and still what
        ;; every core-wasm caller gets when it passes no `:fuel`.
        ;;
        ;; ADR-2607252500 makes Wasm Components the primary application
        ;; artifact, and kototama's component-platform contract requires
        ;; `:fuel` as a DECLARED per-component budget
        ;; (`:required-budgets [:fuel :memory-pages]`, validated by
        ;; `kototama.component-platform/validate-world!` as any positive
        ;; integer). So the value is a caller-supplied budget here rather than
        ;; a constant baked into codegen; the enforcement mechanism (charge
        ;; per call, trap at zero, no guest replenishment) is unchanged.
        global-sec (vec (concat
                         (if component-standard32? [2] [1])
                         [0x7e 1 0x42] (sleb fuel-initial) [0x0b]
                         ;; global 1: the bump pointer. Fuel stays global 0 so
                         ;; every function prologue's `global.get 0` is
                         ;; unchanged.
                         (when component-standard32?
                           (concat [0x7f 1 0x41]
                                   (sleb component-arena-base) [0x0b]))))
        ;; Pure functions are exported with their source names. This makes
        ;; runtime parameters observable and testable without host authority.
        component-function-base (+ shift (count functions))
        realloc-function-index (+ component-function-base (count exported-functions))
        initialize-function-index (inc realloc-function-index)
        export-sec (if component-standard32?
                     (concat
                      (uleb (+ (* 2 (count exported-functions)) 3))
                      (mapcat (fn [function post-index]
                                (concat
                                 (name-bytes (str "cm32p2||" (name (:name function))))
                                 [0] (uleb (get indices (:name function)))
                                 (name-bytes (str "cm32p2||" (name (:name function)) "_post"))
                                 [0] (uleb post-index)))
                              exported-functions
                              (range component-function-base realloc-function-index))
                      (name-bytes "cm32p2_memory") [2] (uleb 0)
                      (name-bytes "cm32p2_realloc") [0] (uleb realloc-function-index)
                      (name-bytes "cm32p2_initialize") [0] (uleb initialize-function-index))
                     (concat (uleb (count exported-functions))
                             (mapcat (fn [function]
                                       (concat (name-bytes (name (:name function))) [0]
                                               (uleb (get indices (:name function)))))
                                     exported-functions)))
        descriptor-indices (when typed? (typed/descriptor-indices kir))
        literal-indices (when typed? (typed/literal-indices kir))
        signatures (when typed? (typed-function-signatures functions))
        code-sec (concat
                  (uleb (+ (count functions) component-function-count))
                  (mapcat #(if typed?
                             (emit-typed-function-body
                                                       % indices
                                                       (assoc intrinsic-indices
                                                              :component-realloc
                                                              realloc-function-index)
                                                       descriptor-indices literal-indices signatures
                                                       opts)
                             (function-body % indices intrinsic-indices))
                          functions)
                  (when component-standard32?
                    (concat
                     (mapcat (fn [_] [2 0 0x0b]) exported-functions)
                     (component-realloc-body)
                     [2 0 0x0b])))
        target-sec (concat (name-bytes "kotoba.target")
                           (utf8 (name target)))
        typed-sec (when (= :kotoba.kir/v4 (:format kir))
                    (concat (name-bytes typed/custom-section-name)
                            (typed/metadata-bytes kir)))
        compatibility-sec (concat (name-bytes compatibility-section-name)
                                  (compatibility-bytes kir target))]
    (let [bytes (concat [0 0x61 0x73 0x6d 1 0 0 0] (section 0 target-sec)
                        (section 0 compatibility-sec)
                        (when typed-sec (section 0 typed-sec))
                        (section 1 types) (when (seq imports) (section 2 import-sec))
                        (section 3 function-sec)
                        (when component-standard32?
                          (section 5 [1 1 component-memory-pages memory-maximum]))
                        (section 6 global-sec)
                        (section 7 export-sec) (section 10 code-sec))]
      #?(:clj (byte-array (map unchecked-byte bytes))
         :cljs (js/Uint8Array.from (clj->js (map #(bit-and % 0xff) bytes))))))))

(defn emit-component-core
  "Emit a standard32-named core module for Component Model Canonical lifting.

  `opts` accepts `:fuel` and `:memory-pages`; both are compiled into the core
  module so independent Component engines enforce the admitted ceilings."
  ([kir target] (emit-component-core kir target {}))
  ([kir target opts] (emit kir target (assoc opts :component-standard32? true))))

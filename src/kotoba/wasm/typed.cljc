(ns kotoba.wasm.typed
  "Wasm-specific typed metadata: the custom section, its ABI versions, and the
  wasm type mapping.

  The structural descriptor language and its byte encoding used to live here
  and now live in `kotoba.kir.descriptor` (T0), because the native capability
  boundary needs the same encoding and there must be exactly one of it. The
  names are re-exported below so every caller of this namespace is unchanged;
  what moved is where they are defined, not what they mean."
  #?(:clj  (:require [kotoba.kir.descriptor :as descriptor])
     :cljs (:require [kotoba.kir.descriptor :as descriptor]
                     [kotoba.kir.cljs-i64 :as i64])))

;; Re-exported from kotoba.kir.descriptor. Vars rather than aliases so that
;; `kotoba.wasm.typed/encode-descriptor` keeps resolving for existing callers
;; in this repo and in the compiler.
(def primitive-tags descriptor/primitive-tags)
(def scalar-adt-aliases descriptor/scalar-adt-aliases)
(def boolean-result-ops descriptor/boolean-result-ops)
(def descriptor? descriptor/descriptor?)
(def uleb descriptor/uleb)
(def utf8 descriptor/utf8)
(def text-bytes descriptor/text-bytes)
(def keyword-text descriptor/keyword-text)
(def encode-descriptor descriptor/encode-descriptor)
(def descriptor-table descriptor/descriptor-table)
(def descriptor-indices descriptor/descriptor-indices)
(def capability-contracts descriptor/capability-contracts)

(def abi-version 8)
(def schema-abi-version 9)
(def compact-graph-abi-version 10)
(def document-abi-version 11)
(def symbol-abi-version 12)
(def list-abi-version 13)
(def bytes-abi-version 14)
(def custom-section-name "kotoba.typed")


(declare literal-table reference-type?)

(defn requires-host-runtime?
  ([kir] (requires-host-runtime? kir {}))
  ([kir {:keys [native-bool?]}]
   ;; i64, f32, and f64 are native Wasm scalars. Canonical Component adapters
   ;; may additionally opt into bool=i32; ordinary KIR v4 deliberately keeps
   ;; bool as a sealed externref.
   (let [native-types (cond-> #{:i64 :f32 :f64} native-bool? (conj :bool))
         signature-types (mapcat (fn [{:keys [param-types result]}]
                                   (conj (vec param-types) result))
                                 (:functions kir))
         ;; `descriptor-table` walks the sealed KIR map and therefore also
         ;; observes the map's own keyword-valued metadata as `:keyword`.
         ;; Actual guest keyword literals are independently present in the
         ;; body-only literal table, so discard only that metadata artefact.
         body-descriptors (disj (set (descriptor-table kir)) :keyword)
         literals (if native-bool?
                    (remove #(= :bool (first %)) (literal-table kir))
                    (literal-table kir))]
     (or (some #(not (contains? native-types %)) signature-types)
         (some #(not (contains? native-types %)) body-descriptors)
         (seq literals)))))

(defn- literal-walk [form found]
  (cond
    ;; `typed-cap-call`'s two TYPE positions are syntax, not values: they are
    ;; the sealed capability contract `capability-contracts` above reads, and
    ;; the guest never computes with them. Every other bare keyword stays a
    ;; literal (a scalar type keyword such as `:bool` is also a perfectly good
    ;; executable keyword value), so this is a positional exception, not a
    ;; blanket "keywords in call position are syntax" rule.
    ;;
    ;; Counting them made `requires-host-runtime?` true for a program whose
    ;; every value is a scalar, so the emitted component core module imported
    ;; the `kotoba:typed` intrinsics -- imports that no WIT interface binds.
    ;; `wasm-tools component new` then rejected the module with `failed to
    ;; resolve import kotoba:typed::literal`, which made every source-level
    ;; `(typed-cap-call <id> :i64 :i64 x)` unrepresentable as a Component even
    ;; though ADR 0076 increment 1's `:scalar-with-capabilities` lowering
    ;; exists precisely to admit it. The four hand-written capability shapes
    ;; never hit this because they emit WAT directly instead of going through
    ;; `emit-component-core`.
    (and (seq? form) (= 'typed-cap-call (first form)) (= 5 (count form)))
    (literal-walk (nth form 4) (literal-walk (nth form 1) found))
    ;; Scalar type keywords (for example :bool) are also valid executable
    ;; keyword literals. Only structured vector descriptors are syntax-only.
    (and (vector? form) (descriptor? form)) found
    (string? form) (conj found [:string form])
    (keyword? form) (conj found [:keyword (str form)])
    (boolean? form) (conj found [:bool form])
    (coll? form) (reduce (fn [result item] (literal-walk item result)) found form)
    :else found))

(defn literal-table [kir]
  (->> (:functions kir)
       (reduce (fn [found function]
                 (literal-walk (:body function) found)) #{})
       (sort-by pr-str)
       vec))

(defn literal-indices [kir]
  (into {} (map-indexed (fn [index literal] [literal index]) (literal-table kir))))

(defn- encode-literal [[kind value]]
  (case kind
    :string (into [0] (text-bytes value))
    :keyword (into [1] (text-bytes value))
    :bool [(if value 3 2)]
    (throw (ex-info "unsupported Wasm typed literal"
                    {:phase :wasm-typed-metadata :literal [kind value]}))))

(defn metadata-bytes [kir]
  (let [descriptors (descriptor-table kir)
        literals (literal-table kir)
        schemas (:schemas kir)
        identities (:schema-identities kir)
        contracts (capability-contracts kir)
        document? (some #(= :document %) descriptors)
        symbol? (some #(= :symbol %) descriptors)
        list? (some #(and (vector? %) (= :list (first %))) descriptors)
        bytes? (some #(= :bytes %) descriptors)
        compact-graph?
        (some #(or (= :string-index %) (= :disjoint-set-i64 %)) descriptors)
        schema? (or (seq schemas) (seq contracts))
        extended-schema? (or schema? compact-graph? document? symbol? list? bytes?)
        indices (descriptor-indices kir)]
    (vec (concat [(cond bytes? bytes-abi-version
                        list? list-abi-version
                        symbol? symbol-abi-version
                        document? document-abi-version
                        compact-graph? compact-graph-abi-version
                        schema? schema-abi-version
                        :else abi-version)]
                 (uleb (count descriptors))
                 (mapcat encode-descriptor descriptors)
                 (uleb (count literals))
                 (mapcat encode-literal literals)
                 (when extended-schema?
                   (concat
                    (uleb (count schemas))
                    (mapcat (fn [[schema-name descriptor]]
                              (concat (text-bytes (keyword-text schema-name))
                                      (text-bytes (get identities schema-name))
                                      (encode-descriptor descriptor)))
                            (sort-by (comp str key) schemas))
                    (uleb (count contracts))
                    (mapcat (fn [{:keys [id request-type result-type]}]
                              (concat (uleb id)
                                      (uleb (get indices request-type))
                                      (uleb (get indices result-type))))
                            contracts)))))))

(defn reference-type? [type]
  ;; Profile-5 :bool is a 0/1 i64 word inside modules, not a sealed ref.
  (not (contains? #{:i64 :f32 :f64 :bool} type)))

(defn wasm-type [type]
  ;; :bool is the 0/1 i64 word. Export ABI boxing to host boolean is the
  ;; emitter's job at function boundaries, not a different local type.
  (case type :i64 0x7e :f32 0x7d :f64 0x7c :bool 0x7e 0x6f))

(declare infer-type)

(defn infer-type [form env signatures]
  (cond
    #?(:clj (integer? form) :cljs (or (i64/bigint-value? form) (integer? form))) :i64
    #?(:clj (instance? Float form) :cljs false) :f32
    #?(:clj (instance? Double form) :cljs (number? form)) :f64
    (string? form) :string
    (keyword? form) :keyword
    (boolean? form) :bool
    (symbol? form) (or (get env form)
                       (throw (ex-info "unbound typed Wasm symbol"
                                       {:phase :wasm-typed-lowering :symbol form})))
    :else
    (let [[op & args] form]
      (cond
        (= op 'let)
        (let [[bindings body] args
              env' (reduce (fn [current [name value]]
                             (assoc current name (infer-type value current signatures)))
                           env (partition 2 bindings))]
          (infer-type body env' signatures))
        (= op 'if) (infer-type (second args) env signatures)
        (= op 'do) (infer-type (last args) env signatures)
        (= op 'typed-cap-call) (nth args 2)
        (= op 'component-assert-bool) :bool
        (= op 'component-i64-to-i32) :bool
        (= op 'component-i32-to-f32) :f32
        (= op 'component-i64-to-f32) :f32
        (= op 'component-i64-to-f64) :f64
        (= op 'component-string-byte-length) :i64
        (= op 'component-list-count) :i64
        (= op 'component-list-at-i64) :i64
        (= op 'component-list-at-f64) :f64
        (= op 'component-list-get-i64) :i64
        (= op 'component-list-get-f64) :f64
        (= op 'component-option-list-capability-count) :i64
        (= op 'component-result-list-capability-count) :i64
        (contains?
         '{component-option-record-capability-project-i64 :i64
           component-option-record-capability-project-f32 :f32
           component-option-record-capability-project-f64 :f64
           component-option-record-capability-project-bool :bool}
         op)
        ('{component-option-record-capability-project-i64 :i64
           component-option-record-capability-project-f32 :f32
           component-option-record-capability-project-f64 :f64
           component-option-record-capability-project-bool :bool}
         op)
        (contains? '#{+ - * quot bit-xor bit-and bit-or bit-not
                      cap-call pair pair-first pair-second
                      i32-wrap u32-wrap i32-wrapping-add i32-wrapping-mul i32-xor
                      i32-shift-left i32-shift-right u32-shift-right xorshift32
                      i64-shift-left i64-shift-right u64-shift-right
                      i64-extend-i32-u
                      string-byte-length string-code-point-at map-get vector-count vector-get vector-f64-count
                      vector-at hetero-vector-count typed-set-count
                      typed-map-count xml-path-count xml-name-count string-index-count
                      disjoint-set-i64-count document-count} op) :i64
        (= op 'f64-to-bits) :i64
        (= op 'f64-from-bits) :f64
        (contains? '#{i64-to-f64-checked i64-to-f64-rounded} op) :f64
        (contains? '#{f64-to-i64-checked f64-to-i64-truncating} op) :i64
        (contains? '#{f64-add f64-sub f64-mul f64-div f64-min f64-max f64-neg f64-abs f64-sqrt
                      f64-sin-quarter-turn f64-cos-quarter-turn
                      f64-sin-bounded f64-cos-bounded
                      f64-exp-near-zero f64-log-near-one f64-atan2-bounded
                      f64-exp-bounded f64-log-bounded} op) :f64
        (contains? '#{f64-eq f64-lt f64-le f64-gt f64-ge f64-unordered} op) :bool
        (= op 'f32-to-bits) :i64
        (= op 'f32-from-bits) :f32
        (= op 'f64-to-f32-rounded) :f32
        (= op 'f32-to-f64-exact) :f64
        (contains? '#{i64-to-f32-checked i64-to-f32-rounded} op) :f32
        (contains? '#{f32-to-i64-checked f32-to-i64-truncating} op) :i64
        (contains? '#{f32-add f32-sub f32-mul f32-div f32-min f32-max f32-neg f32-abs f32-sqrt} op) :f32
        (contains? '#{f32-eq f32-lt f32-le f32-gt f32-ge f32-unordered} op) :bool
        (contains? '#{= < > <= >= hetero-vector-equal typed-set-equal
                      typed-map-equal record-equal} op) :i64
        (= op 'string=?) :i64
        (= op 'string-contains?) :i64
        (= op 'string-split-count) :i64
        (contains? '#{bool-not option-some? result-ok?
                      result-ok?-of option-some?-of typed-set-contains
                      typed-map-contains string-index-contains} op) :bool
        (contains? '#{document-contains document-equal? document-set-contains?} op) :bool
        (contains? '#{string-concat string-substring string-replace-all string-fold-case keyword-name} op) :string
        (contains? '#{keyword-from-string document-kind} op) :keyword
        (= op 'symbol) :symbol
        (contains? '#{xml-name-text xml-path-text xml-path-attr} op) [:option :string]
        (= op 'decimal-f64-parse) [:option :f64]
        (= op 'decimal-f64x3-parse) [:option [:vector [:f64 :f64 :f64]]]
        (= op 'vector-new) :vector-i64
        (= op 'bytes-empty) :bytes
        (= op 'vector-f64-new) :vector-f64
        (= op 'string-index-new) :string-index
        (= op 'string-index-get) [:option :i64]
        (= op 'string-index-assoc) :string-index
        (= op 'disjoint-set-i64-new) :disjoint-set-i64
        (= op 'disjoint-set-i64-union) [:option :disjoint-set-i64]
        (contains? '#{document-null document-bool document-i64 document-f64
                      document-string document-keyword document-symbol document-vector document-list document-set document-map
                      document-vector-assoc document-vector-conj document-vector-drop
                      document-vector-remove document-map-entry-at
                      document-assoc document-dissoc document-merge} op) :document
        (contains? '#{document-get document-vector-at document-list-at document-map-entry-at} op) [:option :document]
        (= op 'document-sha256) :string
        (= op 'document-print) :string
        (= op 'document-read) :document
        (= op 'document-edn-print) :string
        (= op 'document-edn-read) :document
        (= op 'document-string-value) [:option :string]
        (= op 'document-keyword-value) [:option :keyword]
        (= op 'document-symbol-value) [:option :symbol]
        (= op 'document-bool-value) [:option :bool]
        (= op 'document-i64-value) [:option :i64]
        (= op 'document-f64-value) [:option :f64]
        (contains? '#{vector-f64-get vector-f64-at} op) :f64
        (contains? '#{vector-f64-drop vector-f64-assoc vector-f64-conj} op) :vector-f64
        (contains? '#{vector-drop vector-assoc vector-conj} op) :vector-i64
        (= op 'variant-new) (first args)
        (contains? '#{option-some-of option-none-of result-ok-of result-err-of
                      typed-list-new hetero-vector-new typed-set-new typed-map-new record-new} op) (first args)
        (= op 'result-match-of)
        (let [[type _ ok-name ok-body] args]
          (infer-type ok-body (assoc env ok-name (second type)) signatures))
        (= op 'variant-match)
        (let [[type _ branches] args
              [[_ binder body]] branches
              payload-type (second (first (nth type 2)))]
          (infer-type body (assoc env binder payload-type) signatures))
        (= op 'option-match)
        (let [[type _ _ some-name some-body] args]
          (infer-type some-body (assoc env some-name (second type)) signatures))
        (contains? '#{result-value-of result-error-of} op)
        (if (= op 'result-value-of) (second (first args)) (nth (first args) 2))
        (= op 'option-value-of) (second (first args))
        (= op 'typed-map-get) [:option (nth (first args) 2)]
        (= op 'typed-map-entry-at)
        [:option [:vector [(second (first args)) (nth (first args) 2)]]]
        (= op 'typed-set-nth) (second (first args))
        (= op 'hetero-vector-at) (nth (second (first args)) (nth args 2))
        (= op 'record-get)
        (let [[type _ field] args]
          (second (some #(when (= field (first %)) %) (nth type 2))))
        (contains? '#{hetero-vector-assoc typed-set-conj typed-set-disj
                      typed-map-assoc typed-map-dissoc record-assoc} op)
        (first args)
        :else (or (:result (get signatures op))
                  (throw (ex-info "unsupported typed Wasm expression"
                                  {:phase :wasm-typed-lowering :operation op :form form})))))))


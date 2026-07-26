(ns kotoba.wasm.typed
  #?(:cljs (:require [kotoba.kir.cljs-i64 :as i64])))

(def abi-version 8)
(def schema-abi-version 9)
(def compact-graph-abi-version 10)
(def document-abi-version 11)
(def symbol-abi-version 12)
(def list-abi-version 13)
(def custom-section-name "kotoba.typed")

(def ^:private primitive-tags
  {:i64 0 :string 1 :keyword 2 :bool 3 :symbol 19 :vector-i64 11 :f64 12 :f32 13
   :vector-f64 14 :string-index 16 :disjoint-set-i64 17 :document 18})

(def ^:private scalar-adt-aliases
  {:option-i64 [:option :i64]
   :result-i64 [:result :i64 :i64]})

(def ^:private boolean-result-ops
  '#{f64-eq f64-lt f64-le f64-gt f64-ge f64-unordered
     f32-eq f32-lt f32-le f32-gt f32-ge f32-unordered
     bool-not option-some? result-ok? option-some?-of result-ok?-of
     typed-set-contains typed-map-contains document-contains document-equal?})


(defn descriptor? [value]
  ;; nbb cannot hash JavaScript BigInt as a map key. Guard before contains?
  ;; because literal i64 values are walked alongside descriptors.
  (or (and (keyword? value)
           (or (contains? primitive-tags value)
               (contains? scalar-adt-aliases value)))
      (and (vector? value)
           (contains? #{:option :result :variant :vector :set :map :record :ref :list}
                      (first value)))))

(defn- uleb [n]
  (loop [n n out []]
    (let [byte (bit-and n 0x7f)
          remaining (unsigned-bit-shift-right n 7)]
      (if (zero? remaining)
        (conj out byte)
        (recur remaining (conj out (bit-or byte 0x80)))))))

(defn- utf8 [text]
  #?(:clj (mapv #(bit-and (int %) 0xff) (.getBytes ^String text "UTF-8"))
     :cljs (vec (js/Array.from (.encode (js/TextEncoder.) text)))))

(defn- text-bytes [text]
  (let [bytes (utf8 text)]
    (into (uleb (count bytes)) bytes)))

(defn- keyword-text [value]
  (str value))

(declare encode-descriptor)

(defn- encode-named-members [members]
  (into (uleb (count members))
        (mapcat (fn [[member-name member-type]]
                  (concat (text-bytes (keyword-text member-name))
                          (encode-descriptor member-type)))
                members)))

(defn encode-descriptor [descriptor]
  (if-let [expanded (get scalar-adt-aliases descriptor)]
    (encode-descriptor expanded)
    (if-let [tag (get primitive-tags descriptor)]
    [tag]
    (case (first descriptor)
      :option (into [4] (encode-descriptor (second descriptor)))
      :result (into [5] (concat (encode-descriptor (second descriptor))
                                (encode-descriptor (nth descriptor 2))))
      :variant (into [6] (concat (text-bytes (keyword-text (second descriptor)))
                                 (encode-named-members (nth descriptor 2))))
      :vector (into [7] (concat (uleb (count (second descriptor)))
                                (mapcat encode-descriptor (second descriptor))))
      :set (into [8] (encode-descriptor (second descriptor)))
      :list (into [20] (encode-descriptor (second descriptor)))
      :map (into [10] (concat (encode-descriptor (second descriptor))
                              (encode-descriptor (nth descriptor 2))))
      :record (into [9] (concat (text-bytes (keyword-text (second descriptor)))
                                (encode-named-members (nth descriptor 2))))
      :ref (into [15] (text-bytes (keyword-text (second descriptor))))
      (throw (ex-info "unsupported Wasm typed descriptor"
                      {:phase :wasm-typed-metadata :descriptor descriptor}))))))

(defn- walk [value found]
  (cond
    (descriptor? value)
    (let [found (conj found value)]
      (if-not (vector? value)
        found
        (case (first value)
          :option (walk (second value) found)
          :result (->> found (walk (second value)) (walk (nth value 2)))
          :variant (reduce (fn [result [_ type]] (walk type result)) found (nth value 2))
          :vector (reduce (fn [result type] (walk type result)) found (second value))
          :set (walk (second value) found)
          :list (walk (second value) found)
          :map (->> found (walk (second value)) (walk (nth value 2)))
          :record (reduce (fn [result [_ type]] (walk type result)) found (nth value 2))
          :ref found
          found)))

    (and (seq? value) (contains? boolean-result-ops (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :bool)
            value)
    (and (seq? value)
         (contains? '#{option-some option-none option-some? option-value}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :option-i64)
            value)
    (and (seq? value)
         (contains? '#{result-ok result-err result-ok? result-value result-error}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :result-i64)
            value)
    (and (seq? value)
         (contains? '#{vector-f64-new vector-f64-count vector-f64-get vector-f64-at
                      vector-f64-drop vector-f64-assoc vector-f64-conj}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :vector-f64)
            value)
    (and (seq? value)
         (contains? '#{string-index-new string-index-count string-index-contains
                      string-index-get string-index-assoc}
                    (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :string-index) value)
    (and (seq? value)
         (contains? '#{disjoint-set-i64-new disjoint-set-i64-count disjoint-set-i64-union}
                    (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :disjoint-set-i64) value)
    (and (seq? value)
         (contains? '#{document-null document-bool document-i64 document-f64
                      document-string document-keyword document-vector document-map
                      document-count document-kind document-vector-at document-map-entry-at document-vector-assoc
                      document-vector-conj document-vector-drop document-vector-remove
                      document-equal? document-contains document-get document-assoc
                      document-dissoc document-merge document-string-value document-keyword-value
                      document-bool-value document-i64-value document-f64-value}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (cond-> (conj found :document)
              (= 'document-kind (first value)) (conj :keyword))
            value)
    (and (seq? value) (= 'keyword-from-string (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :keyword) value)
    (and (seq? value) (= 'symbol (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :symbol) value)
    (map? value) (reduce (fn [result item] (walk item result)) found (vals value))
    (coll? value) (reduce (fn [result item] (walk item result)) found value)
    (string? value) (conj found :string)
    (keyword? value) (conj found :keyword)
    (boolean? value) (conj found :bool)
    :else found))

(defn descriptor-table [kir]
  (->> (walk kir #{})
       (sort-by pr-str)
       vec))

(defn descriptor-indices [kir]
  (into {} (map-indexed (fn [index descriptor] [descriptor index])
                        (descriptor-table kir))))

(defn capability-contracts
  "Returns the sealed typed capability contracts used by KIR. One capability
  id has exactly one request/result contract per module."
  [kir]
  (let [contracts (->> (:functions kir)
                       (mapcat #(tree-seq coll? seq (:body %)))
                       (keep (fn [form]
                               (when (and (seq? form) (= 'typed-cap-call (first form)))
                                 (let [[_ id request-type result-type] form]
                                   {:id id :request-type request-type :result-type result-type}))))
                       distinct
                       (sort-by (juxt :id (comp pr-str :request-type) (comp pr-str :result-type)))
                       vec)]
    (doseq [[id grouped] (group-by :id contracts)]
      (when (> (count grouped) 1)
        (throw (ex-info "typed capability id has conflicting contracts"
                        {:phase :wasm-typed-metadata :capability id :contracts grouped}))))
    contracts))

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
        document? (some #{:document} descriptors)
        symbol? (some #{:symbol} descriptors)
        list? (some #(and (vector? %) (= :list (first %))) descriptors)
        compact-graph? (some #{:string-index :disjoint-set-i64} descriptors)
        schema? (or (seq schemas) (seq contracts))
        extended-schema? (or schema? compact-graph? document? symbol? list?)
        indices (descriptor-indices kir)]
    (vec (concat [(cond list? list-abi-version
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
  (not (contains? #{:i64 :f32 :f64} type)))

(defn wasm-type [type]
  (case type :i64 0x7e :f32 0x7d :f64 0x7c 0x6f))

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
        (contains? '#{bool-not option-some? result-ok?
                      result-ok?-of option-some?-of typed-set-contains
                      typed-map-contains string-index-contains} op) :bool
        (contains? '#{document-contains document-equal?} op) :bool
        (contains? '#{string-concat string-substring string-replace-all string-fold-case keyword-name} op) :string
        (contains? '#{keyword-from-string document-kind} op) :keyword
        (= op 'symbol) :symbol
        (contains? '#{xml-name-text xml-path-text xml-path-attr} op) [:option :string]
        (= op 'decimal-f64-parse) [:option :f64]
        (= op 'decimal-f64x3-parse) [:option [:vector [:f64 :f64 :f64]]]
        (= op 'vector-new) :vector-i64
        (= op 'vector-f64-new) :vector-f64
        (= op 'string-index-new) :string-index
        (= op 'string-index-get) [:option :i64]
        (= op 'string-index-assoc) :string-index
        (= op 'disjoint-set-i64-new) :disjoint-set-i64
        (= op 'disjoint-set-i64-union) [:option :disjoint-set-i64]
        (contains? '#{document-null document-bool document-i64 document-f64
                      document-string document-keyword document-vector document-map
                      document-vector-assoc document-vector-conj document-vector-drop
                      document-vector-remove document-map-entry-at
                      document-assoc document-dissoc document-merge} op) :document
        (contains? '#{document-get document-vector-at document-map-entry-at} op) [:option :document]
        (= op 'document-string-value) [:option :string]
        (= op 'document-keyword-value) [:option :keyword]
        (= op 'document-bool-value) [:option :bool]
        (= op 'document-i64-value) [:option :i64]
        (= op 'document-f64-value) [:option :f64]
        (contains? '#{vector-f64-get vector-f64-at} op) :f64
        (contains? '#{vector-f64-drop vector-f64-assoc vector-f64-conj} op) :vector-f64
        (contains? '#{vector-drop vector-assoc vector-conj} op) :vector-i64
        (= op 'variant-new) (first args)
        (contains? '#{option-some-of option-none-of result-ok-of result-err-of
                      hetero-vector-new typed-set-new typed-map-new record-new} op) (first args)
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

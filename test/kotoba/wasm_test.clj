(ns kotoba.wasm-test
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [kotoba.wasm.core :as wasm]
            [kotoba.wasm.typed :as typed]
            [kotoba.wasm.canonical-abi :as canonical]
            [kotoba.wasm.tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Load gate: the split must not break namespace resolution. Each extracted
;; namespace must load standalone from this repo's own dependency closure.
(deftest every-extracted-namespace-loads
  (is (some? (find-ns 'kotoba.wasm.core)) "kotoba.wasm.core must load")
  (is (some? (find-ns 'kotoba.wasm.typed)) "kotoba.wasm.typed must load")
  (is (some? (find-ns 'kotoba.wasm.canonical-abi)) "kotoba.wasm.canonical-abi must load")
  (is (some? (find-ns 'kotoba.wasm.tools)) "kotoba.wasm.tools must load"))

(deftest fuel-ceiling-is-the-exact-positive-i64-subset
  (let [kir {:format :kotoba.kir/v2
             :entry 'main
             :exports ['main]
             :effects #{}
             :functions [{:name 'main :params [] :body 42}]}]
    (is (= 4611686018427387903 wasm/max-fuel))
    (is (bytes? (wasm/emit kir :wasm32-kotoba-v1 {:fuel wasm/max-fuel})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"exceeds the representable ceiling"
         (wasm/emit kir :wasm32-kotoba-v1 {:fuel (inc wasm/max-fuel)})))))

(deftest document-map-infers-only-key-argument-types
  (let [kir {:format :kotoba.kir/v4
             :exports ['value]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'value :params [] :param-types [] :result :document :effects #{}
               :body '(document-map
                        :ready (document-bool true)
                        (document-vector (document-i64 1)) (document-string "value"))}]}
        bytes (wasm/emit kir :wasm32-browser-kotoba-v1)]
    (is (bytes? bytes))
    (is (pos? (count bytes)))))

(deftest typed-modules-lower-legacy-pair-intrinsics
  (let [kir {:format :kotoba.kir/v4
             :exports ['main]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'main :params [] :param-types [] :result :i64 :effects #{}
               :body '(pair-first (pair 42 7))}]}
        bytes (wasm/emit kir :wasm32-browser-kotoba-v1)
        path (Files/createTempFile "kotoba-wasm-typed-pair-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit validated)) (:err validated)))
      (finally
        (Files/deleteIfExists path)))))

(deftest bounded-list-descriptors-use-the-versioned-recursive-metadata-tag
  (let [descriptor [:list [:ref :demo/item]]
        schema [:record :demo/item [[:x :i64] [:enabled :bool]]]
        kir {:format :kotoba.kir/v4
             :exports ['echo]
             :schemas {:demo/item schema}
             :schema-identities
             {:demo/item
              "0000000000000000000000000000000000000000000000000000000000000000"}
             :effects #{}
             :functions
             [{:name 'echo :params ['items] :param-types [descriptor]
               :result descriptor :effects #{} :body 'items}]}]
    (is (= [20 3] (typed/encode-descriptor [:list :bool])))
    (is (= typed/list-abi-version (first (typed/metadata-bytes kir))))))

(deftest bytes-values-use-typed-abi-v14-and-a-dedicated-empty-constructor
  (let [kir {:format :kotoba.kir/v4
             :exports ['empty 'discard]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'empty :params [] :param-types [] :result :bytes
               :effects #{} :body '(bytes-empty)}
              {:name 'discard :params [] :param-types [] :result :i64
               :effects #{} :body '(do (bytes-empty) 1)}]}
        bytes (wasm/emit kir :wasm32-browser-kotoba-v1)
        path (Files/createTempFile "kotoba-wasm-bytes-empty-" ".wasm"
                                   (make-array FileAttribute 0))]
    (is (= [21] (typed/encode-descriptor :bytes)))
    (is (= typed/bytes-abi-version (first (typed/metadata-bytes kir))))
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))]
        (is (zero? (:exit validated)) (:err validated))
        (is (str/includes? (:out printed) "(import \"kotoba:typed\" \"bytes-empty\"")))
      (finally
        (Files/deleteIfExists path)))))

(deftest vector-count-uses-the-actual-canonical-list-descriptor
  (let [descriptor [:list :string]
        kir {:format :kotoba.kir/v4
             :exports ['count-items]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'count-items
               :params ['items]
               :param-types [descriptor]
               :result :i64
               :effects #{}
               :body '(vector-count items)}]}
        bytes (wasm/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-wasm-generic-list-count-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit validated)) (:err validated)))
      (finally
        (Files/deleteIfExists path)))))

(deftest canonical-list-construction-uses-the-list-descriptor
  (let [descriptor [:list :i64]
        kir {:format :kotoba.kir/v4
             :exports ['count-items]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'count-items :params [] :param-types [] :result :i64 :effects #{}
               :body (list 'vector-count (list 'typed-list-new descriptor 4 5 6))}]}
        bytes (wasm/emit kir :wasm32-wasi-kotoba-v1)
        path (Files/createTempFile
              "kotoba-wasm-generic-list-new-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit validated)) (:err validated)))
      (finally
        (Files/deleteIfExists path)))))

(deftest canonical-bool-validation-exclusions-are-function-scoped
  (let [kir {:format :kotoba.kir/v4
             :exports ['joined 'ordinary]
             :schemas {}
             :effects #{}
             :functions [{:name 'joined :params ['payload]
                          :param-types [:bool] :result :bool :body 'payload}
                         {:name 'ordinary :params ['flag]
                          :param-types [:bool] :result :bool :body 'flag}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1
               {:component-canonical-scalars? true
                :component-unchecked-bool-params {'joined #{0}}})
        path (Files/createTempFile "kotoba-wasm-function-bool-scope-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [joined (shell/sh "wasmtime" "run" "--invoke" "cm32p2||joined"
                             (str path) "2")
            ordinary (shell/sh "wasmtime" "run" "--invoke" "cm32p2||ordinary"
                               (str path) "2")]
        (is (zero? (:exit joined)) (:err joined))
        (is (= "2" (str/trim (:out joined))))
        (is (not (zero? (:exit ordinary)))
            "an exclusion for one function must not disable another function"))
      (finally
        (Files/deleteIfExists path)))))

(deftest public-vector-descriptors-share-the-canonical-list-layout
  (doseq [[public item] [[:vector-i64 :i64]
                         [:vector-f64 :f64]]]
    (let [public-layout (canonical/layout public)
          internal-layout (canonical/layout [:list item])]
      (is (= (dissoc internal-layout :descriptor)
             (dissoc public-layout :descriptor)))
      (is (= public (:descriptor public-layout)))
      (is (= [:i32 :i32] (:flat public-layout)))
      (is (= item (get-in public-layout [:item-layout :descriptor])))
      (is (pos-int? (:max-items public-layout)))))
  (let [layout (canonical/layout [:option :vector-i64])]
    (is (= :option (:kind layout)))
    (is (= [:i32 :i32 :i32] (:flat layout)))
    (is (= :vector-i64
           (get-in layout [:cases 1 :layout :descriptor])))))

;; A `typed-cap-call`'s two TYPE arguments are syntax, not values. Counting them
;; as keyword literals made a program whose every value is a scalar look like it
;; needed the `kotoba:typed` host intrinsics, so the emitted component core
;; module imported `kotoba:typed`/`kotoba:heap` -- imports no WIT interface can
;; bind. `wasm-tools component new` then rejected the module, which made every
;; source-level `(typed-cap-call <id> :i64 :i64 x)` unrepresentable as a
;; Component despite `:scalar-with-capabilities` existing to admit exactly that.
(deftest typed-cap-call-type-arguments-are-not-keyword-literals
  (let [scalar-kir {:format :kotoba.kir/v4
                    :exports ['call]
                    :schemas {}
                    :effects #{[:cap/call 8]}
                    :functions [{:name 'call :params ['request]
                                 :param-types [:i64] :result :i64
                                 :body '(typed-cap-call 8 :i64 :i64 request)}]}]
    (is (not (typed/requires-host-runtime? scalar-kir))
        "a scalar capability call needs no host runtime")
    (is (empty? (typed/literal-table scalar-kir))
        "the :i64 type arguments must not appear as guest keyword literals")
    ;; The capability contract still reads those same positions -- suppressing
    ;; them as literals must not suppress them as the sealed contract.
    (is (= [{:id 8 :request-type :i64 :result-type :i64}]
           (typed/capability-contracts scalar-kir))))
  ;; A keyword the guest actually computes with is still a literal, and a
  ;; non-scalar capability type still needs the host runtime.
  (let [keyword-kir {:format :kotoba.kir/v4
                     :exports ['call]
                     :schemas {}
                     :effects #{[:cap/call 8]}
                     :functions [{:name 'call :params ['request]
                                  :param-types [:i64] :result :keyword
                                  :body '(typed-cap-call 8 :i64 :keyword request)}]}]
    (is (typed/requires-host-runtime? keyword-kir)
        "a keyword-valued capability result is not a native Wasm scalar")))

(deftest canonical-scalar-capability-requires-an-explicit-named-binding
  (let [kir {:format :kotoba.kir/v4
             :exports ['call]
             :schemas {}
             :effects #{[:cap/call 8]}
             :functions [{:name 'call :params ['request]
                          :param-types [:i64] :result :i64
                          :body '(typed-cap-call 8 :i64 :i64 request)}]}
        opts {:component-canonical-scalars? true
              :capability-imports
              [{:id 8
                :module "cm32p2|kotoba:application/clock@1"
                :field "now"
                :type [0x60 1 0x7e 1 0x7e]}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1 opts)
        text (String. (byte-array (map unchecked-byte bytes)) "ISO-8859-1")]
    (is (str/includes? text "cm32p2|kotoba:application/clock@1"))
    (is (not (str/includes? text (str "kotoba:typed" (char 8) "cap-call"))))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"requires a named import"
         (wasm/emit-component-core
          kir :wasm32-wasi-kotoba-v1
          {:component-canonical-scalars? true}))
        "canonical adapters cannot fall back to the generic ambient ABI")))

(deftest component-option-list-capability-count-has-a-flat-named-import
  (let [kir {:format :kotoba.kir/v4
             :exports ['call]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'call
               :params ['pointer 'count 'fallback]
               :param-types [:bool :bool :i64]
               :result :i64
               :body '(component-option-list-capability-count
                        7 pointer count fallback 16 8 8 16 8 8
                        1 1048576)}]}
        opts {:component-canonical-scalars? true
              :component-unchecked-bool-params {'call #{0 1}}
              :core-param-types {'call [0x7f 0x7f 0x7e]}
              ;; option<list<s64>> standard32: disc, pointer, count, retptr -> ().
              :capability-imports
              [{:id 7
                :module "cm32p2|kotoba:application/clock@1"
                :field "now"
                :type [0x60 4 0x7f 0x7f 0x7f 0x7f 0]}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1 opts)
        path (Files/createTempFile
              "kotoba-wasm-component-option-list-capability-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit validated)) (:err validated)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"requires a named import"
           (wasm/emit-component-core
            kir :wasm32-wasi-kotoba-v1
            (dissoc opts :capability-imports))))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"item validation plan is invalid"
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 8 8 16 8 8
                        1 -1))
            :wasm32-wasi-kotoba-v1 opts)))
      (is (bytes?
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 1 1 12 4 4
                        2 0))
            :wasm32-wasi-kotoba-v1 opts))
          "kind 2 validates inline Canonical bool items")
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"item validation plan is invalid"
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 1 1 12 4 4
                        2 1))
            :wasm32-wasi-kotoba-v1 opts)))
      (is (bytes?
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 16 8 16 8 8
                        3 2 8 12))
            :wasm32-wasi-kotoba-v1 opts))
          "kind 3 validates every declared inline-record bool offset")
      (doseq [invalid-plan [[3 2 8 8]
                            [3 2 8 16]
                            [3 1]]]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"item validation plan is invalid"
             (wasm/emit-component-core
              (assoc-in
               kir [:functions 0 :body]
               (apply list 'component-option-list-capability-count
                      7 'pointer 'count 'fallback 16 16 8 16 8 8
                      invalid-plan))
              :wasm32-wasi-kotoba-v1 opts))))
      (is (bytes?
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 8 4 12 4 4
                        4 16 2 8 4 8 8))
            :wasm32-wasi-kotoba-v1 opts))
          "kind 4 admits a fixed-depth nested-list layout plan")
      (doseq [invalid-plan [[4 16 0]
                            [4 16 2 8 4]
                            [4 16 1 8 3]
                            [4 16 1 0 1]]]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"item validation plan is invalid"
             (wasm/emit-component-core
              (assoc-in
               kir [:functions 0 :body]
               (apply list 'component-option-list-capability-count
                      7 'pointer 'count 'fallback 16 8 4 12 4 4
                      invalid-plan))
              :wasm32-wasi-kotoba-v1 opts))))
      (is (bytes?
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 12 4 12 4 4
                        5 2 4 1048576 1 65536 2 0))
            :wasm32-wasi-kotoba-v1 opts))
          "kind 5 validates only the selected union case payload")
      (doseq [invalid-plan [[5 2 4 1048576 1 65536]
                            [5 257 4 1048576]
                            [5 2 8 1048576 1 65536 2 0]
                            [5 2 4 1048576 1 1048577 2 0]
                            [5 2 4 1048576 3 0 2 0]
                            [5 2 4 1048576 1 65536 2 1]]]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"item validation plan is invalid"
             (wasm/emit-component-core
              (assoc-in
               kir [:functions 0 :body]
               (apply list 'component-option-list-capability-count
                      7 'pointer 'count 'fallback 16 12 4 12 4 4
                      invalid-plan))
              :wasm32-wasi-kotoba-v1 opts))))
      (is (bytes?
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-list-capability-count
                        7 pointer count fallback 16 12 4 12 4 4
                        6 1048576 16384
                        4 2 4
                        2 65536
                        5 16 1 1 1))
            :wasm32-wasi-kotoba-v1 opts))
          "kind 6 admits a closed recursive union/list validation plan")
      (doseq [invalid-plan
              [[6 1048576 16384 4 2 4 2 65536]
               [6 1048576 16384 4 2 12 2 65536 1]
               [6 1048576 16384 5 16 1 3 1]
               [6 1048576 16384 3 1 12 1]
               [6 1048576 16384 1 0]]]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"item validation plan is invalid"
             (wasm/emit-component-core
              (assoc-in
               kir [:functions 0 :body]
               (apply list 'component-option-list-capability-count
                      7 'pointer 'count 'fallback 16 12 4 12 4 4
                      invalid-plan))
              :wasm32-wasi-kotoba-v1 opts))))
      (finally
        (Files/deleteIfExists path)))))

(deftest component-result-list-capability-count-has-a-flat-named-import
  (let [kir {:format :kotoba.kir/v4
             :exports ['call]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'call
               :params ['pointer 'count]
               :param-types [:bool :bool]
               :result :i64
               :body '(component-result-list-capability-count
                        7 0 pointer count 16 8 8 16 8)}]}
        opts {:component-canonical-scalars? true
              :component-unchecked-bool-params {'call #{0 1}}
              :core-param-types {'call [0x7f 0x7f]}
              ;; result<list<s64>, list<s64>> has the same flat standard32
              ;; import shape as option<list<s64>>.
              :capability-imports
              [{:id 7
                :module "cm32p2|kotoba:application/clock@1"
                :field "now"
                :type [0x60 4 0x7f 0x7f 0x7f 0x7f 0]}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1 opts)
        path (Files/createTempFile
              "kotoba-wasm-component-result-list-capability-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit validated)) (:err validated)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"request case is invalid"
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-result-list-capability-count
                        7 2 pointer count 16 8 8 16 8))
            :wasm32-wasi-kotoba-v1 opts)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"result layout is invalid"
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-result-list-capability-count
                        7 0 pointer count 16 1 1 16 8 3))
            :wasm32-wasi-kotoba-v1 opts)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"requires a named import"
           (wasm/emit-component-core
            kir :wasm32-wasi-kotoba-v1
            (dissoc opts :capability-imports))))
      (finally
        (Files/deleteIfExists path)))))

(deftest component-option-record-capability-projects-a-validated-field
  (let [kir {:format :kotoba.kir/v4
             :exports ['call]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'call
               :params ['x 'enabled 'weight 'fallback]
               :param-types [:i64 :bool :f32 :i64]
               :result :i64
               :body '(component-option-record-capability-project-i64
                        7 [x enabled weight] fallback
                        24 8 8 [16] 8)}]}
        opts {:component-canonical-scalars? true
              :core-param-types {'call [0x7e 0x7f 0x7d 0x7e]}
              :capability-imports
              [{:id 7
                :module "cm32p2|kotoba:application/clock@1"
                :field "now"
                ;; option<record{x:s64,enabled:bool,weight:f32}> + retptr.
                :type [0x60 5 0x7f 0x7e 0x7f 0x7d 0x7f 0]}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1 opts)
        path (Files/createTempFile
              "kotoba-wasm-component-record-capability-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit validated)) (:err validated)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"requires a named import"
           (wasm/emit-component-core
            kir :wasm32-wasi-kotoba-v1
            (dissoc opts :capability-imports))))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"result layout is invalid"
           (wasm/emit-component-core
            (assoc-in kir [:functions 0 :body]
                      '(component-option-record-capability-project-i64
                        7 [x enabled weight] fallback
                        24 8 8 [24] 8))
            :wasm32-wasi-kotoba-v1 opts)))
      (finally
        (Files/deleteIfExists path)))))

(deftest component-string-length-validates-the-selected-flat-range
  (let [kir {:format :kotoba.kir/v4
             :exports ['length]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'length
               :params ['pointer 'length]
               ;; Canonical bool uses core i32; these generated adapter
               ;; parameters are explicitly excluded from bool validation.
               :param-types [:bool :bool]
               :result :i64
               :body '(component-string-byte-length pointer length 16)}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1
               {:component-canonical-scalars? true
                :component-unchecked-bool-params {'length #{0 1}}
                :core-param-types {'length [0x7f 0x7f]}})
        path (Files/createTempFile
              "kotoba-wasm-component-string-length-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [valid (shell/sh "wasmtime" "run" "--invoke" "cm32p2||length"
                            (str path) "0" "16")
            over-bound (shell/sh "wasmtime" "run" "--invoke" "cm32p2||length"
                                 (str path) "0" "17")
            wrapped (shell/sh "wasmtime" "run" "--invoke" "cm32p2||length"
                              (str path) "-1" "2")
            out-of-memory
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||length"
                      (str path) "16777215" "2")]
        (is (zero? (:exit valid)) (:err valid))
        (is (= "16" (str/trim (:out valid))))
        (is (not (zero? (:exit over-bound))))
        (is (not (zero? (:exit wrapped))))
        (is (not (zero? (:exit out-of-memory)))))
      (finally
        (Files/deleteIfExists path)))))

(deftest component-list-count-validates-the-selected-flat-range
  (let [kir {:format :kotoba.kir/v4
             :exports ['count-items]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'count-items
               :params ['pointer 'count]
               :param-types [:bool :bool]
               :result :i64
               :body '(component-list-count pointer count 16 8 8)}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1
               {:component-canonical-scalars? true
                :component-unchecked-bool-params {'count-items #{0 1}}
                :core-param-types {'count-items [0x7f 0x7f]}})
        path (Files/createTempFile
              "kotoba-wasm-component-list-count-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [valid (shell/sh "wasmtime" "run" "--invoke" "cm32p2||count-items"
                            (str path) "8" "16")
            over-bound (shell/sh "wasmtime" "run" "--invoke" "cm32p2||count-items"
                                 (str path) "8" "17")
            unaligned (shell/sh "wasmtime" "run" "--invoke" "cm32p2||count-items"
                                (str path) "1" "1")
            wrapped (shell/sh "wasmtime" "run" "--invoke" "cm32p2||count-items"
                              (str path) "-8" "2")
            out-of-memory
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||count-items"
                      (str path) "16777208" "2")]
        (is (zero? (:exit valid)) (:err valid))
        (is (= "16" (str/trim (:out valid))))
        (is (not (zero? (:exit over-bound))))
        (is (not (zero? (:exit unaligned))))
        (is (not (zero? (:exit wrapped))))
        (is (not (zero? (:exit out-of-memory)))))
      (finally
        (Files/deleteIfExists path)))))

(deftest component-list-at-validates-before-loading-i64-or-f64
  (let [kir {:format :kotoba.kir/v4
             :exports ['at-i64 'at-f64]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'at-i64
               :params ['pointer 'count 'index]
               :param-types [:bool :bool :i64]
               :result :i64
               :body '(component-list-at-i64 pointer count index 16 8 8)}
              {:name 'at-f64
               :params ['pointer 'count 'index]
               :param-types [:bool :bool :i64]
               :result :f64
               :body '(component-list-at-f64 pointer count index 16 8 8)}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1
               {:component-canonical-scalars? true
                :component-unchecked-bool-params
                {'at-i64 #{0 1} 'at-f64 #{0 1}}
                :core-param-types
                {'at-i64 [0x7f 0x7f 0x7e]
                 'at-f64 [0x7f 0x7f 0x7e]}})
        path (Files/createTempFile
              "kotoba-wasm-component-list-at-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [i64-valid (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-i64"
                                (str path) "8" "2" "1")
            f64-valid (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-f64"
                                (str path) "8" "2" "1")
            negative-index
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-i64"
                      (str path) "8" "2" "-1")
            equal-index
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-i64"
                      (str path) "8" "2" "2")
            over-bound
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-i64"
                      (str path) "8" "17" "0")
            unaligned
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-i64"
                      (str path) "1" "1" "0")
            wrapped
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||at-i64"
                      (str path) "-8" "2" "0")]
        (is (zero? (:exit i64-valid)) (:err i64-valid))
        (is (= "0" (str/trim (:out i64-valid))))
        (is (zero? (:exit f64-valid)) (:err f64-valid))
        (is (= "0" (str/trim (:out f64-valid))))
        (doseq [run [negative-index equal-index over-bound unaligned wrapped]]
          (is (not (zero? (:exit run))))))
      (finally
        (Files/deleteIfExists path)))))

(deftest component-list-get-validates-the-list-before-index-fallback
  (let [kir {:format :kotoba.kir/v4
             :exports ['get-i64 'get-f64]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'get-i64
               :params ['pointer 'count 'index 'fallback]
               :param-types [:bool :bool :i64 :i64]
               :result :i64
               :body '(component-list-get-i64
                        pointer count index fallback 16 8 8)}
              {:name 'get-f64
               :params ['pointer 'count 'index 'fallback]
               :param-types [:bool :bool :i64 :f64]
               :result :f64
               :body '(component-list-get-f64
                        pointer count index fallback 16 8 8)}]}
        bytes (wasm/emit-component-core
               kir :wasm32-wasi-kotoba-v1
               {:component-canonical-scalars? true
                :component-unchecked-bool-params
                {'get-i64 #{0 1} 'get-f64 #{0 1}}
                :core-param-types
                {'get-i64 [0x7f 0x7f 0x7e 0x7e]
                 'get-f64 [0x7f 0x7f 0x7e 0x7c]}})
        path (Files/createTempFile
              "kotoba-wasm-component-list-get-" ".wasm"
              (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [present (shell/sh "wasmtime" "run" "--invoke" "cm32p2||get-i64"
                              (str path) "8" "2" "1" "77")
            negative (shell/sh "wasmtime" "run" "--invoke" "cm32p2||get-i64"
                               (str path) "8" "2" "-1" "77")
            equal (shell/sh "wasmtime" "run" "--invoke" "cm32p2||get-i64"
                            (str path) "8" "2" "2" "77")
            f64-fallback
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||get-f64"
                      (str path) "8" "2" "2" "3.5")
            malformed-list-still-traps
            (shell/sh "wasmtime" "run" "--invoke" "cm32p2||get-i64"
                      (str path) "1" "2" "9" "77")]
        (is (zero? (:exit present)) (:err present))
        (is (= "0" (str/trim (:out present))))
        (is (= "77" (str/trim (:out negative))))
        (is (= "77" (str/trim (:out equal))))
        (is (= "3.5" (str/trim (:out f64-fallback))))
        (is (not (zero? (:exit malformed-list-still-traps)))))
      (finally
        (Files/deleteIfExists path)))))

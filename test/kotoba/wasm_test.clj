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

(deftest frontend-loop-helpers-use-structured-wasm-without-growing-host-stack
  (let [kir {:format :kotoba.kir/v3
             :exports ['scalar 'countdown]
             :effects #{}
             :functions
             [{:name 'scalar :params ['iterations] :result :i64 :effects #{}
               :body '(__kotoba_loop_1 0 0 iterations)}
              {:name '__kotoba_loop_1 :params ['index 'total 'iterations]
               :result :i64 :effects #{}
               :body '(if (= index iterations)
                        total
                        (if (< index iterations)
                          (__kotoba_loop_1 (+ index 1) (+ total (* 6 7)) iterations)
                          total))}
              ;; Ordinary source recursion remains a charged call and must not
              ;; inherit the frontend loop-helper optimization.
              {:name 'countdown :params ['remaining] :result :i64 :effects #{}
               :body '(if (= remaining 0) 0 (countdown (- remaining 1)))}]}
        bytes (wasm/emit kir :wasm32-kotoba-v1)
        path (Files/createTempFile "kotoba-wasm-structured-loop-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))
            deep-loop (shell/sh "wasmtime" "run" "--invoke" "scalar"
                                (str path) "100000")
            fuel-trap (shell/sh "wasmtime" "run" "--invoke" "countdown"
                                (str path) "600")]
        (is (zero? (:exit validated)) (:err validated))
        (is (str/includes? (:out printed) "      loop")
            "frontend loop helper must contain a structured Wasm loop")
        (is (= "4200000" (str/trim (:out deep-loop))) (:err deep-loop))
        (is (not (zero? (:exit fuel-trap)))
            "ordinary recursion must retain the fixed fuel boundary"))
      (finally
        (Files/deleteIfExists path)))))

(deftest typed-loop-helpers-also-use-structured-wasm
  (let [kir {:format :kotoba.kir/v4
             :exports ['scalar]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'scalar :params ['iterations] :param-types [:i64]
               :result :i64 :effects #{}
               :body '(__kotoba_loop_2 0 0 iterations)}
              {:name '__kotoba_loop_2 :params ['index 'total 'iterations]
               :param-types [:i64 :i64 :i64] :result :i64 :effects #{}
               :body '(if (= index iterations)
                        total
                        (__kotoba_loop_2 (+ index 1) (+ total 42) iterations))}]}
        bytes (wasm/emit kir :wasm32-kotoba-v1)
        path (Files/createTempFile "kotoba-wasm-typed-structured-loop-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            deep-loop (shell/sh "wasmtime" "run" "--invoke" "scalar"
                                (str path) "100000")]
        (is (zero? (:exit validated)) (:err validated))
        (is (= "4200000" (str/trim (:out deep-loop))) (:err deep-loop)))
      (finally
        (Files/deleteIfExists path)))))

(deftest typed-u32-let-chain-keeps-intermediates-in-i32-locals
  (let [kir {:format :kotoba.kir/v4
             :exports ['mix-step]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'mix-step :params ['state] :param-types [:i64]
               :result :i64 :effects #{}
               :body '(let [x0 (u32-wrap state)
                            x1 (u32-wrap (i32-xor x0 (i32-shift-left x0 13)))
                            x2 (u32-wrap (i32-xor x1 (u32-shift-right x1 17)))
                            x3 (u32-wrap (i32-xor x2 (i32-shift-left x2 5)))]
                        x3)}]}
        path (Files/createTempFile "kotoba-wasm-u32-local-chain-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (wasm/emit kir :wasm32-kotoba-v1)
                   (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))
            result (shell/sh "wasmtime" "run" "--invoke" "mix-step"
                             (str path) "2463534242")]
        (is (zero? (:exit validated)) (:err validated))
        (is (str/includes? (:out printed) "(local i32 i32 i32 i32)")
            "u32-only let intermediates must not occupy i64 locals")
        (is (<= (count (re-seq #"i64.extend_i32_u" (:out printed))) 1)
            "the u32 chain should extend only at its i64 result boundary")
        (is (= "723471715" (str/trim (:out result))) (:err result)))
      (finally
        (Files/deleteIfExists path)))))

(deftest bounded-vector-literal-reads-and-counts-use-checked-wasm-locals
  (let [kir {:format :kotoba.kir/v4
             :exports ['pick 'let-pick 'empty-pick 'immediate-count
                       'let-count 'count-and-pick]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'pick :params ['index] :param-types [:i64]
               :result :i64 :effects #{}
               :body '(vector-at (vector-new 3 5 8 13 21 34 55 89) index)}
              {:name 'let-pick :params ['index] :param-types [:i64]
               :result :i64 :effects #{}
               :body '(let [items (vector-new 3 5 8 13 21 34 55 89)]
                        (vector-at items index))}
              {:name 'empty-pick :params [] :param-types []
               :result :i64 :effects #{}
               :body '(vector-at (vector-new) 0)}
              {:name 'immediate-count :params [] :param-types []
               :result :i64 :effects #{}
               :body '(vector-count (vector-new 3 5 8 13 21 34 55 89))}
              {:name 'let-count :params [] :param-types []
               :result :i64 :effects #{}
               :body '(let [items (vector-new 3 5 8)] (vector-count items))}
              {:name 'count-and-pick :params ['index] :param-types [:i64]
               :result :i64 :effects #{}
               :body '(let [items (vector-new 3 5 8 13 21 34 55 89)]
                        (+ (vector-count items) (vector-at items index)))}]}
        path (Files/createTempFile "kotoba-wasm-scalar-vector-at-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (wasm/emit kir :wasm32-kotoba-v1)
                   (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))
            first-item (shell/sh "wasmtime" "run" "--invoke" "pick"
                                 (str path) "0")
            last-item (shell/sh "wasmtime" "run" "--invoke" "pick"
                                (str path) "7")
            let-item (shell/sh "wasmtime" "run" "--invoke" "let-pick"
                               (str path) "5")
            immediate-count (shell/sh "wasmtime" "run" "--invoke" "immediate-count"
                                      (str path))
            let-count (shell/sh "wasmtime" "run" "--invoke" "let-count"
                                (str path))
            count-and-pick (shell/sh "wasmtime" "run" "--invoke" "count-and-pick"
                                     (str path) "0")
            negative (shell/sh "wasmtime" "run" "--invoke" "pick"
                               (str path) "-1")
            past-end (shell/sh "wasmtime" "run" "--invoke" "pick"
                               (str path) "8")
            empty-vector (shell/sh "wasmtime" "run" "--invoke" "empty-pick"
                                   (str path))]
        (is (zero? (:exit validated)) (:err validated))
        (is (not (str/includes? (:out printed) "(import \"kotoba:typed\""))
            "a fully scalar-replaced module must not retain typed-host imports")
        (is (= "3" (str/trim (:out first-item))) (:err first-item))
        (is (= "89" (str/trim (:out last-item))) (:err last-item))
        (is (= "34" (str/trim (:out let-item))) (:err let-item))
        (is (= "8" (str/trim (:out immediate-count))) (:err immediate-count))
        (is (= "3" (str/trim (:out let-count))) (:err let-count))
        (is (= "11" (str/trim (:out count-and-pick))) (:err count-and-pick))
        (is (not (zero? (:exit negative)))
            "a negative literal-vector index must trap")
        (is (not (zero? (:exit past-end)))
            "an index equal to the literal-vector count must trap")
        (is (not (zero? (:exit empty-vector)))
            "reading an empty literal vector must trap"))
      (finally
        (Files/deleteIfExists path)))))

(deftest escaping-and-wide-vector-literals-use-the-bounded-bulk-host-path
  (let [wide-vector (apply list 'vector-new (range 33))
        kir {:format :kotoba.kir/v4
             :exports ['escape 'wide-read]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'escape :params [] :param-types []
               :result :vector-i64 :effects #{}
               :body '(let [items (vector-new 1 2 3)] items)}
              {:name 'wide-read :params [] :param-types []
               :result :i64 :effects #{}
               :body (list 'vector-at wide-vector 0)}]}
        path (Files/createTempFile "kotoba-wasm-host-vector-fallback-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (wasm/emit kir :wasm32-kotoba-v1)
                   (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))]
        (is (zero? (:exit validated)) (:err validated))
        (is (str/includes? (:out printed)
                           "(import \"kotoba:typed\" \"vector-from-memory-i64\"")
            "escaping and over-local-limit vectors must use one bulk host copy")
        (is (str/includes? (:out printed)
                           "(import \"kotoba:typed\" \"scratch\" (memory (;0;) 2 2))")
            "bulk construction must use the fixed imported scratch memory")
        (is (str/includes? (:out printed)
                           "(global (;1;) (mut i32) i32.const 0)")
            "bulk construction must reserve from a private per-instance bump pointer")
        (is (and (str/includes? (:out printed) "global.get 1")
                 (str/includes? (:out printed) "global.set 1")
                 (str/includes? (:out printed) "i32.const 131072")
                 (str/includes? (:out printed) "i32.lt_u")
                 (str/includes? (:out printed) "i32.gt_u"))
            "the LIFO reservation must reject wraparound and scratch exhaustion")
        (is (not (str/includes? (:out printed) "(export \"scratch\""))
            "typed scratch and its bump pointer must remain host/module private")
        (is (str/includes? (:out printed) "i64.store offset=256")
            "a 33-item vector must store its final item at the checked offset")
        (is (str/includes? (:out printed) "(import \"kotoba:typed\" \"vector-at-i64\"")
            "the wide-vector fallback must retain checked host indexing"))
      (finally
        (Files/deleteIfExists path)))))

(deftest structured-loop-recur-replaces-parameters-simultaneously
  (let [untyped-kir
        {:format :kotoba.kir/v3
         :exports ['rotate]
         :effects #{}
         :functions
         [{:name 'rotate :params ['iterations] :result :i64 :effects #{}
           :body '(__kotoba_loop_7 1 2 iterations)}
          {:name '__kotoba_loop_7 :params ['left 'right 'remaining]
           :result :i64 :effects #{}
           ;; An odd number of simultaneous swaps must produce 21. Updating
           ;; `left` before evaluating `right` would incorrectly produce 22.
           :body '(if (= remaining 0)
                    (+ (* left 10) right)
                    (__kotoba_loop_7 right left (- remaining 1)))}]}
        typed-kir
        {:format :kotoba.kir/v4
         :exports ['rotate]
         :schemas {}
         :effects #{}
         :functions
         [{:name 'rotate :params ['iterations] :param-types [:i64]
           :result :i64 :effects #{}
           :body '(__kotoba_loop_8 1 2 iterations)}
          {:name '__kotoba_loop_8 :params ['left 'right 'remaining]
           :param-types [:i64 :i64 :i64] :result :i64 :effects #{}
           :body '(if (= remaining 0)
                    (+ (* left 10) right)
                    (__kotoba_loop_8 right left (- remaining 1)))}]}]
    (doseq [[label kir] [["untyped" untyped-kir] ["typed" typed-kir]]]
      (let [path (Files/createTempFile (str "kotoba-wasm-parallel-recur-" label "-")
                                       ".wasm" (make-array FileAttribute 0))]
        (try
          (Files/write path ^bytes (wasm/emit kir :wasm32-kotoba-v1)
                       (make-array java.nio.file.OpenOption 0))
          (let [validated (shell/sh "wasm-tools" "validate" (str path))
                result (shell/sh "wasmtime" "run" "--invoke" "rotate"
                                 (str path) "100001")]
            (is (zero? (:exit validated)) (str label ": " (:err validated)))
            (is (= "21" (str/trim (:out result)))
                (str label ": " (:err result))))
          (finally
            (Files/deleteIfExists path)))))))

(deftest malformed-loop-helper-arity-is-not-structured
  (let [kir {:format :kotoba.kir/v3
             :exports ['__kotoba_loop_10]
             :effects #{}
             :functions
             [{:name '__kotoba_loop_10 :params ['remaining 'total]
               :result :i64 :effects #{}
               :body '(if (= remaining 0)
                        total
                        (__kotoba_loop_10 (- remaining 1)))}]}
        path (Files/createTempFile "kotoba-wasm-loop-arity-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes (wasm/emit kir :wasm32-kotoba-v1)
                   (make-array java.nio.file.OpenOption 0))
      (let [printed (shell/sh "wasm-tools" "print" (str path))
            validated (shell/sh "wasm-tools" "validate" (str path))]
        (is (zero? (:exit printed)) (:err printed))
        (is (not (str/includes? (:out printed) "      loop"))
            "wrong-arity self-call must not retain stale parameter locals")
        (is (not (zero? (:exit validated)))
            "historical call lowering leaves malformed KIR fail-closed at validation"))
      (finally
        (Files/deleteIfExists path)))))

(deftest unsupported-helper-self-call-shapes-fall-back-to-valid-call-lowering
  (let [kir {:format :kotoba.kir/v3
             :exports ['__kotoba_loop_9]
             :effects #{}
             :functions
             [{:name '__kotoba_loop_9 :params ['remaining]
               :result :i64 :effects #{}
               ;; Deliberately not a frontend recur shape: the self-call is an
               ;; operand of `+`, so rewriting it to `br` would target the
               ;; wrong value context.
               :body '(if (= remaining 0)
                        0
                        (+ 1 (__kotoba_loop_9 (- remaining 1))))}]}
        bytes (wasm/emit kir :wasm32-kotoba-v1)
        path (Files/createTempFile "kotoba-wasm-loop-fallback-" ".wasm"
                                   (make-array FileAttribute 0))]
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))
            result (shell/sh "wasmtime" "run" "--invoke" "__kotoba_loop_9"
                             (str path) "10")]
        (is (zero? (:exit validated)) (:err validated))
        (is (not (str/includes? (:out printed) "      loop"))
            "unsupported self-call context must not be rewritten to a branch")
        (is (= "10" (str/trim (:out result))) (:err result)))
      (finally
        (Files/deleteIfExists path)))))

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

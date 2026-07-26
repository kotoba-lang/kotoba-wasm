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

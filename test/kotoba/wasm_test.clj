(ns kotoba.wasm-test
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [kotoba.wasm.core :as wasm]
            [kotoba.wasm.typed]
            [kotoba.wasm.canonical-abi]
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

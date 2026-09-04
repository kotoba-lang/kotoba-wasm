(ns kotoba.wasm-string-upper-test
  "string-upper wasm32 lowering: a module using only string-upper must emit a
  valid typed wasm binary whose import table carries exactly one
  kotoba:typed/string-upper entry. Mirrors the emit-then-inspect pattern of
  the bytes-values test (wasm-tools validate + print)."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [kotoba.wasm.core :as wasm])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest string-upper-lowers-to-the-typed-string-upper-import
  (let [kir {:format :kotoba.kir/v4
             :exports ['up]
             :schemas {}
             :effects #{}
             :functions
             [{:name 'up :params ['s] :param-types [:string]
               :result :string :effects #{} :body '(string-upper s)}]}
        bytes (wasm/emit kir :wasm32-browser-kotoba-v1)
        path (Files/createTempFile "kotoba-wasm-string-upper-" ".wasm"
                                   (make-array FileAttribute 0))]
    (is (bytes? bytes))
    (is (pos? (count bytes)))
    (try
      (Files/write path ^bytes bytes (make-array java.nio.file.OpenOption 0))
      (let [validated (shell/sh "wasm-tools" "validate" (str path))
            printed (shell/sh "wasm-tools" "print" (str path))]
        (is (zero? (:exit validated)) (:err validated))
        (is (str/includes? (:out printed) "(import \"kotoba:typed\" \"string-upper\""))
        ;; exactly one string-upper import, and no fold-case import (the
        ;; module does not use it -- conditional import admission)
        (is (= 1 (count (re-seq #"string-upper" (:out printed)))))
        (is (not (str/includes? (:out printed) "string-fold-case"))))
      (finally
        (Files/deleteIfExists path)))))

(ns kotoba.wasm.tools
  "Pinned official tooling used only for Component Model encoding."
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def version "1.243.0")

(defn- reject [message data]
  (throw (ex-info message (assoc data :phase :wasm-tools))))

(defn run-command! [args]
  (let [process (.start (doto (ProcessBuilder. ^java.util.List (vec args))
                          (.redirectErrorStream true)))
        output (String. (.readAllBytes (.getInputStream process)) StandardCharsets/UTF_8)
        exit (.waitFor process)]
    (when-not (zero? exit)
      (reject "pinned wasm-tools command failed"
              {:command (vec args) :exit exit :output output}))
    output))

(def ^:private version-pattern
  ;; `wasm-tools --version` prints `wasm-tools <version>` optionally followed
  ;; by a ` (<git-hash> <date>)` build-metadata suffix that varies between
  ;; otherwise-identical pinned builds. Pin the version token exactly; ignore
  ;; the suffix. \Q..\E quotes the dotted version so it matches literally.
  (re-pattern (str "wasm-tools \\Q" version "\\E(?: \\(.*\\))?")))

(defn assert-version! []
  (let [output (.trim ^String (run-command! ["wasm-tools" "--version"]))]
    (when-not (re-matches version-pattern output)
      (reject "wasm-tools version is not pinned"
              {:expected version :actual output}))))

(defn parse-wat
  "Encode a generated core WAT module with the pinned official parser."
  [source]
  (assert-version!)
  (let [dir (Files/createTempDirectory "kotoba-component-core-"
                                       (make-array FileAttribute 0))
        wat (.resolve dir "core.wat")
        wasm (.resolve dir "core.wasm")]
    (try
      (Files/writeString wat ^String source (make-array java.nio.file.OpenOption 0))
      (run-command! ["wasm-tools" "parse" (str wat) "-o" (str wasm)])
      (Files/readAllBytes wasm)
      (finally
        (Files/deleteIfExists wasm)
        (Files/deleteIfExists wat)
        (Files/deleteIfExists dir)))))

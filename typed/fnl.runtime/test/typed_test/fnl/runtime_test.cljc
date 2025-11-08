(ns typed-test.fnl.runtime-test
  (:require [clojure.test :refer [deftest is]]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.java.shell :as shell])))

#?(:clj
(deftest demo-fennel-test
  (let [project-root (-> (io/file ".")
                         .getCanonicalPath
                         io/file)
        demo-file (io/file project-root "test-resources/typed_test/fnl/runtime/demo.fnl")
        fennel-bin (io/file project-root "fennel-bootstrap/fennel")
        src-path (str (.getCanonicalPath (io/file project-root "src")) "/?.fnl")]
    (is (.exists demo-file) "demo.fnl should exist in test-resources")
    (is (.exists fennel-bin) "Fennel binary should exist")
    (is (.canExecute fennel-bin) "Fennel binary should be executable")
    
    ;; Run the demo with macro path to load typed.fennel
    (let [result (shell/sh (.getCanonicalPath fennel-bin)
                          "--add-macro-path" src-path
                          (.getCanonicalPath demo-file))]
      (is (zero? (:exit result))
          (str "Fennel demo should execute successfully. "
               "Error: " (:err result)))
      (is (re-find #"Hello from Typed Fennel!" (:out result))
          (str "Demo should print expected output. "
               "Got: " (:out result)))))))



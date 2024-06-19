(ns cljs.core.typed.test-rt
  (:require [cljs.core.typed :as tcljs]
            [clojure.core.typed.errors :as err]
            [clojure.java.io :as io])
  (:use clojure.test))

(deftest checking-cljs-ops
  (if (io/resource "cljs/core/typed.cljc")
    (is true)
    (do (is (thrown? java.io.FileNotFoundException
                     (tcljs/load-if-needed)))
        (is (nil? (tcljs/reset-caches)))
        (is (thrown? java.io.FileNotFoundException
                     (tcljs/check-form* 1 nil nil)))
        (is (thrown? java.io.FileNotFoundException
                     (tcljs/cf 1)))
        (is (thrown? java.io.FileNotFoundException
                     (tcljs/check-ns*)))
        (is (thrown? java.io.FileNotFoundException
                     (tcljs/check-ns* 'foo)))
        ; these throw at macroexpansion time
        (is (thrown? Exception
                     (eval '(cljs.core.typed/check-ns))))
        (is (thrown? Exception
                     (eval '(cljs.core.typed/check-ns foo)))))))

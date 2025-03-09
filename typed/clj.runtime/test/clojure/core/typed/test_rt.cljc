(ns ^:typed.clojure clojure.core.typed.test-rt
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            #?(:cljr [clojure.clr.io :as io] :default [clojure.java.io :as io])
            [clojure.test :refer [deftest is]]))

(deftest typed-clojure-loaded
  (is (nil? (require 'clojure.core.typed))))

(deftest ^:typed/skip-from-repo-root checking-ops
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/load-if-needed)))
  (is (nil? (t/reset-caches)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/method-type 'foo)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/into-array> 'foo 'bar [1])))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/cf 1)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/check-form* 1)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/check-form-info 1)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/check-ns 'foo)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/check-ns-info 'foo)))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/statistics ['foo])))
  (is (thrown? #?(:cljr System.IO.FileNotFoundException :default java.io.FileNotFoundException)
        (t/var-coverage))))

(defmacro thrown-blame? [& e]
  `(try (try (do ~@e)
             false
             (catch #?(:cljr clojure.lang.Compiler+CompilerException :default clojure.lang.Compiler$CompilerException) e#
               (throw (#?(:cljr .FileSource :default .source) e#))))
        (catch clojure.lang.ExceptionInfo e#
          (boolean (-> e# ex-data :blame)))))

; commented - these require c.t.lib.clojure which is not a dependency of this project atm

#_
(deftest pred-test
  ;pred forces a few namespaces to load
  (is ((t/pred Number) 1)))

#_
(deftest cast-test
  (is (= 1 (t/cast t/Int 1)))
  (is (= nil (t/cast nil nil)))
  (is (= 1 (t/cast t/Int 1)))
  ;; unions
  (is (thrown-blame? (t/cast t/Int nil 
                             {:positive '+ve 
                              :negative '-ve
                              :file "my/file.clj"
                              :line 20
                              :column 30})))
  (is (= 1 (t/cast (t/U t/Int) 1)))
  (is (thrown-blame? (t/cast (t/U t/Int) nil)))
  (is (thrown-blame? (t/cast (t/U t/Bool t/Int) nil)))
  (is (= 1 (t/cast (t/U (t/U t/Int)) 1)))
  (is (thrown-blame? (t/cast (t/U (t/U t/Int)) nil)))
  ;; intersections
  (is (= 1 (t/cast (t/I t/Int) 1)))
  (is (thrown-blame? (t/cast (t/I t/Int) nil)))
  (is (= 1 (t/cast (t/I (t/I t/Int)) 1)))
  (is (thrown-blame? (t/cast (t/I (t/I t/Int)) nil)))

  (is (thrown-blame? (t/cast (t/I (t/I t/Int)) nil)))
  )

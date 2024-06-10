(ns typed-test.cljs.checker.parse-unparse
  (:require [clojure.test :refer [deftest]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljs.checker.test-utils :refer :all]))

(deftest parse-prims-cljs-test
  (is-cljs (= (prs/parse-cljs 'typed.clojure/JSnumber)
              (r/JSNumber-maker)))
  (is-cljs (= (prs/parse-cljs 'typed.clojure/CLJSInteger)
              (r/CLJSInteger-maker)))
  (is-cljs (= (prs/parse-cljs 'typed.clojure/JSboolean)
              (r/JSBoolean-maker)))
  #_
  (is-cljs (= (prs/parse-cljs 'typed.clojure/JSobject)
              (r/JSObject-maker)))
  (is-cljs (= (prs/parse-cljs 'typed.clojure/JSstring)
              (r/JSString-maker))))

(deftest parse-array-cljs-test
  (is-cljs (= (prs/parse-cljs '(Array typed.clojure/JSnumber))
              (r/ArrayCLJS-maker (prs/parse-cljs 'typed.clojure/JSnumber)
                                 (prs/parse-cljs 'typed.clojure/JSnumber)))))

(deftest unparse-prims-cljs-test
  (is-cljs (= 'typed.clojure/JSnumber
              (prs/unparse-type (prs/parse-cljs 'typed.clojure/JSnumber) cljs-opts)))
  (is-cljs (= 'typed.clojure/JSboolean
              (prs/unparse-type (prs/parse-cljs 'typed.clojure/JSboolean) cljs-opts)))
  (is-cljs (= 'typed.clojure/CLJSInteger
              (prs/unparse-type (prs/parse-cljs 'typed.clojure/CLJSInteger) cljs-opts)))
  (is-cljs (= '(Array typed.clojure/JSnumber)
              (prs/unparse-type (prs/parse-cljs '(Array typed.clojure/JSnumber)) cljs-opts)))
  (is-cljs (= '(Array2 typed.clojure/JSNumber typed.clojure/JSboolean)
              (prs/unparse-type (prs/parse-cljs '(Array2 typed.clojure/JSNumber typed.clojure/JSboolean)) cljs-opts))))

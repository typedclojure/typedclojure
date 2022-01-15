(ns typed-test.cljs.checker.parse-unparse
  (:require [clojure.core.typed :as t]
            [clojure.test :refer [deftest]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljs.checker.test-utils :refer :all]))

(deftest parse-prims-cljs-test
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSNumber)
              (r/JSNumber-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/CLJSInteger)
              (r/CLJSInteger-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSBoolean)
              (r/JSBoolean-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSObject)
              (r/JSObject-maker)))
  (is-cljs (= (prs/parse-cljs 'cljs.core.typed/JSString)
              (r/JSString-maker))))

(deftest parse-array-cljs-test
  (is-cljs (= (prs/parse-cljs '(Array cljs.core.typed/JSNumber))
              (r/ArrayCLJS-maker (prs/parse-cljs 'cljs.core.typed/JSNumber)
                                 (prs/parse-cljs 'cljs.core.typed/JSNumber)))))

(deftest unparse-prims-cljs-test
  (is-cljs (= 'cljs.core.typed/JSNumber
              (prs/unparse-type (prs/parse-cljs 'cljs.core.typed/JSNumber))))
  (is-cljs (= 'cljs.core.typed/JSBoolean
              (prs/unparse-type (prs/parse-cljs 'cljs.core.typed/JSBoolean))))
  (is-cljs (= 'cljs.core.typed/CLJSInteger
              (prs/unparse-type (prs/parse-cljs 'cljs.core.typed/CLJSInteger))))
  (is-cljs (= '(Array cljs.core.typed/JSNumber)
              (prs/unparse-type (prs/parse-cljs '(Array cljs.core.typed/JSNumber)))))
  (is-cljs (= '(Array2 cljs.core.typed/JSNumber cljs.core.typed/JSBoolean)
              (prs/unparse-type (prs/parse-cljs '(Array2 cljs.core.typed/JSNumber cljs.core.typed/JSBoolean))))))

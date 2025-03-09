(ns typed-example.zero-deps-test
  (:require [typed.clojure :as t]
            [clojure.test :refer [deftest is]]))

(deftest zero-deps-test
  (is (t/check-ns-clj 'typed-example.zero-deps))
  ;; WARNING: Use of undeclared Var typed-example.zero-deps/reduce at line 13 /Users/ambrose/Projects/typedclojure-local-dev/opt-in/example-projects/zero-deps/src/typed_example/zero_deps.cljc
  ;; WARNING: Use of undeclared Var typed-example.zero-deps/conj at line 22 /Users/ambrose/Projects/typedclojure-local-dev/opt-in/example-projects/zero-deps/src/typed_example/zero_deps.cljc
  #_ ;;FIXME
  (is (t/check-ns-cljs 'typed-example.zero-deps))
  )

(ns clojure.core.typed.test.field-override
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [typed.cljc.checker.type-ctors :refer :all]
    [typed.cljc.checker.type-rep :refer :all]))

(deftest clojure-1-10-kv-destructure-test
  (is-tc-e (fn [& args]
             (let* [map__65083 args
                    map__65083 (if (seq? map__65083) 
                                 (clojure.lang.PersistentHashMap/create (clojure.core/seq map__65083)) 
                                 map__65083)] 
               map__65083))
           [& :optional {:foo t/Str} -> (t/Option (t/HMap :optional {:foo t/Str}))])
  ;; kw args are nilable
  (is-tc-err (fn [& args]
               (let* [map__65083 args
                      map__65083 (if (seq? map__65083) 
                                   (clojure.lang.PersistentHashMap/create (clojure.core/seq map__65083)) 
                                   map__65083)] 
                 map__65083))
             [& :optional {:foo t/Str} -> (t/HMap :optional {:foo t/Str})])
  )

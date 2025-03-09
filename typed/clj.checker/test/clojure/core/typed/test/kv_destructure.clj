(ns ^:typed.clojure clojure.core.typed.test.kv-destructure
  (:require 
    [typed.clj.checker.test-utils :refer :all]
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

(deftest clojure-1-11-kv-destructure-test
  ;; optional kws
  (is-tc-e (fn [& args]
             (let* [map__65083 args
                    map__65083 (if (seq? map__65083) 
                                 (if (next map__65083)
                                   (do
                                     (t/ann-form map__65083 (t/CountRange 1))
                                     (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array map__65083)))
                                   (do
                                     (t/ann-form map__65083 (t/CountRange 0 1))
                                     (if (seq map__65083)
                                       (t/ann-form :unreachable (t/U))
                                       (do (t/ann-form map__65083 (t/ExactCount 0))
                                           clojure.lang.PersistentArrayMap/EMPTY)))) 
                                 map__65083)] 
               map__65083))
           [& :optional {:foo t/Str} -> (t/Option (t/HMap :optional {:foo t/Str}))])
  ;; mandatory kws
  (is-tc-e (fn [& args]
             (let* [map__65083 args
                    map__65083 (if (seq? map__65083) 
                                 (if (next map__65083)
                                   (do
                                     (t/ann-form map__65083 (t/CountRange 1))
                                     (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array map__65083)))
                                   (do
                                     (t/ann-form map__65083 (t/CountRange 0 1))
                                     (if (seq map__65083)
                                       (do (t/ann-form map__65083 (t/ExactCount 1))
                                           (t/ann-form [:unreachable map__65083] (t/U)))
                                       (do (t/ann-form map__65083 (t/ExactCount 0))
                                           clojure.lang.PersistentArrayMap/EMPTY)))) 
                                 map__65083)] 
               map__65083))
           [& :mandatory {:foo t/Str} -> (t/Option (t/HMap :mandatory {:foo t/Str}))]))

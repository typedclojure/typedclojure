(ns ^:typed.clojure clojure.core.typed.test.rclass-supers
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.type-rep :refer :all]
            [typed.cljc.checker.type-ctors :refer :all]
            [typed.clj.checker.parse-unparse :refer [unparse-type]]
            [clojure.set :as set])
  (:import (clojure.lang IPersistentVector IPersistentCollection
                         Indexed IPersistentMap APersistentVector PersistentVector APersistentMap PersistentHashMap
                         PersistentHashSet PersistentTreeSet IPersistentSet APersistentSet
                         IPersistentList PersistentList ISeq ASeq IPersistentCollection Associative
                         IPersistentStack
                         PersistentTreeMap)))

(deftest rclass-supers-test
  (is-clj 
    (let [num (RClass-of Number (clj-opts))
          nnum (Un [num -nil] (clj-opts))]
        (doseq [[t r] {(-name `t/Seqable num) (Un [-nil num] (clj-opts))
                       (RClass-of IPersistentVector [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of APersistentVector [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of PersistentVector [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of IPersistentList [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of PersistentList [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of ISeq [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of ASeq [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of clojure.lang.Cons [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of clojure.lang.LazySeq [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of IPersistentCollection [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of IPersistentSet [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of APersistentSet [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of PersistentTreeSet [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of PersistentHashSet [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (-name `t/Associative num num) -any
                       (RClass-of IPersistentStack [num] (clj-opts)) (Un [-nil num] (clj-opts))
                       (RClass-of String (clj-opts)) (Un [-nil (RClass-of Character (clj-opts))] (clj-opts))
                       (RClass-of CharSequence (clj-opts)) (Un [-nil (RClass-of Character (clj-opts))] (clj-opts))
                       ; only supports key/val
                       ;(RClass-of clojure.lang.IMapEntry [num num] (clj-opts)) num
                       (RClass-of clojure.lang.AMapEntry [num num] (clj-opts)) num
                       (RClass-of clojure.lang.MapEntry [num num] (clj-opts)) num
                       (RClass-of IPersistentMap [num num] (clj-opts)) (Un [-nil (-hvec [num num] {} (clj-opts))] (clj-opts))
                       (RClass-of APersistentMap [num num] (clj-opts)) (Un [-nil (-hvec [num num] {} (clj-opts))] (clj-opts))
                       (RClass-of PersistentHashMap [num num] (clj-opts)) (Un [-nil (-hvec [num num] {} (clj-opts))] (clj-opts))
                       (RClass-of PersistentTreeMap [num num] (clj-opts)) (Un [-nil (-hvec [num num] {} (clj-opts))] (clj-opts))}]
          (t/check-form* '(fn [a] (first a))
                         (unparse-type
                           (make-FnIntersection
                             (make-Function [t] r))
                           (assoc (clj-opts) ::vs/verbose-types true))))
        true)))

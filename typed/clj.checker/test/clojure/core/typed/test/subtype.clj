(ns clojure.core.typed.test.subtype
  (:require [typed.clojure :as t]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.type-ctors :refer :all]
            [typed.cljc.checker.type-rep :refer :all :as r]
            [typed.clj.checker.parse-unparse :refer [parse-type] :as prs]
            [clojure.test :refer :all])
  (:import (clojure.lang Seqable)))

(deftest subtype-test
  (is-tc-e :load)
  (is-clj (sub? Integer Integer))
  (is-clj (sub? Integer Object))
  (is-clj (not (sub? Object Integer)))
  (is-clj (sub? Object Object))
  (is-clj (sub? Integer Number))
  (is-clj (sub? (typed.clojure/Seqable Integer)
                (typed.clojure/Seqable Integer)))
  (is-clj (sub? (typed.clojure/Seqable Integer)
                (typed.clojure/Seqable Number)))
  (is-clj (not
            (sub? (typed.clojure/Seqable Number)
                  (typed.clojure/Seqable Integer))))
  (is-clj (sub? (clojure.lang.Cons Integer)
                (clojure.lang.Cons Number)))
  (is-clj (sub? (clojure.lang.Cons Integer)
                (typed.clojure/Seqable Number))))

(deftest subtype-java-exceptions-test
  (is-tc-e :load)
  (is-clj (subtype? (RClass-of IndexOutOfBoundsException nil)
                    (RClass-of Exception nil))))

;TODO uncomment
; See CTYP-150
#_(deftest subtype-intersection
  (is-clj (not (subtype? (-name `t/Seqable -any)
                         (In (-name `t/Seqable -any)
                             (make-CountRange 1)))))
  (is-clj (sub?-q `(t/NonEmptyASeq t/Num)
                  `(t/NonEmptySeq t/Num)))
  (is-clj (sub?-q `(t/NonEmptyASeq (t/Val 1))
                  `(t/NonEmptySeq t/Num))))

(deftest subtype-Object
  (is-tc-e :load)
  (is-clj (subtype? (RClass-of clojure.lang.IPersistentList [-any]) (RClass-of Object nil))))

(deftest subtype-hmap
  (is-tc-e :load)
  (is (sub?-q `(t/HMap :mandatory {:a '1} :complete? true)
              `(t/HMap :mandatory {:a Number} :complete? true)))
  (is (sub?-q `(t/HMap :mandatory {:a '1} :complete? true)
              `(t/HMap :mandatory {} :complete? false)))
  (is (sub?-q `(t/HMap :mandatory {:a '1 :b '2 :c '3} :complete? true)
              `(t/HMap :mandatory {:a '1 :b '2} :complete? false)))
  (is (not (sub?-q `'{:a nil}
                   `'{:a '1})))
  (is (not (sub?-q `(t/HMap :mandatory {:a '1} :complete? true)
                   `(t/HMap :mandatory {} :complete? true))))
  (is (not (sub?-q `(t/HMap :mandatory {:a '1 :b '2} :complete? true)
                   `(t/HMap :mandatory {:a '1 :b '2 :c '3} :complete? false)))))

(deftest subtype-poly
  (is-clj (sub?-q `(t/All [x#] (clojure.lang.ASeq x#))
                  `(t/All [y#] (t/Seqable y#)))))

(deftest subtype-rec
  (is-clj (sub?-q `t/Int
                  `(t/Rec [x#] (t/U t/Int (t/Seqable x#)))))
  (is-clj (sub?-q `(t/Seqable (t/Seqable t/Int))
                  `(t/Rec [x#] (t/U t/Int (t/Seqable x#)))))
  (is-clj (not (sub?-q `t/Num
                       `(t/Rec [x#] (t/U t/Int (t/Seqable x#))))))
  (is-clj (sub?-q `'{:op ':if
                     :test '{:op ':var
                             :var (clojure.lang.Var t/Nothing t/Any)}
                     :then '{:op ':nil}
                     :else '{:op ':false}}
                  `(t/Rec [x#] 
                          (t/U '{:op ':if
                                 :test x#
                                 :then x#
                                 :else x#}
                               '{:op ':var
                                 :var (clojure.lang.Var clojure.core.typed/Nothing clojure.core.typed/Any)}
                               '{:op ':nil}
                               '{:op ':false}))))

  (is-clj (sub?-q `(t/Rec [x#] (t/U Integer (clojure.lang.ILookup x# x#)))
                  `(t/Rec [x#] (t/U Number (clojure.lang.ILookup x# x#))))))

(deftest count-subtype-test
  (is-clj (subtype? (make-CountRange 1)
                    (make-CountRange 1)))
  (is-clj (not (subtype? (make-CountRange 1)
                         (make-ExactCountRange 1))))
  (is-clj (subtype? (make-ExactCountRange 1)
                    (make-CountRange 1)))
  (is-clj (subtype? (make-ExactCountRange 4)
                    (make-CountRange 1)))
  (is-clj (subtype? (make-ExactCountRange 4)
                    (make-CountRange 0)))
  (is-clj (subtype? (make-CountRange 2)
                    (make-CountRange 1))))

(deftest array-subtype-test
  (is-tc-e :load)
  (is-clj (sub? (Array int) (Array int)))
  (is-clj (sub? (Array int) (ReadOnlyArray int)))
  (is-clj (sub? (Array Long) (typed.clojure/Seqable Long)))
  ;FIXME
  ;(is-clj (not (sub? (Array Object) (typed.clojure/Seqable Long))))
  (is-clj (not (sub? (ReadOnlyArray int) (Array int)))))

(deftest top-function-subtype-test
  (is-clj (subtype? (parse-type `[t/Any ~'-> t/Any])
                    (parse-type `t/AnyFunction))))

(deftest complete-hash-subtype-test
  (is-clj (sub? (clojure.core.typed/HMap :complete? true)
                (clojure.core.typed/Map Integer Long))))

(deftest latent-filter-subtype-test 
  (is-clj (not (sub?-q `[t/Any :-> t/Any :filters {:then (~'is Number 0)}]
                       `[t/Any :-> t/Any :filters {:then (~'is t/Nothing 0)}]))))

(deftest subtype-tfn-test
  (is-clj (sub?-q `(t/TFn [[x# :variance :covariant]] Number)
                  `(t/TFn [[x# :variance :covariant]] t/Any)))
  (is-clj (not (sub?-q `(t/TFn [[x# :variance :covariant]] t/Any)
                       `(t/TFn [[x# :variance :covariant]] Number))))
  (is-clj (sub?-q 
            `(t/Map t/Any t/Any)
            `((t/TFn [[x# :variance :covariant]] (t/Map t/Any t/Any)) t/Any))))

(deftest union-intersection-subtype-test
  ;; not sure how to support this. see :or translation in typed.malli.schema-to-type for motivation
  #_
  (is-clj (sub?-q `(t/I t/AnyInteger t/Num)
                  `t/AnyInteger))
  (is-clj (sub?-q `t/AnyInteger
                  `(t/I t/AnyInteger t/Num))))

(deftest regex-subtype-test
  (is-tc-e :load)
  (is (subtype? (r/regex [(prs/parse-clj `t/Int)] :cat)
                (r/regex [(prs/parse-clj `t/Int)] :cat)))
  #_ ;;TODO
  (is (subtype? (r/regex [(prs/parse-clj `t/Int)] :cat)
                (r/regex [(prs/parse-clj `t/Num)] :cat)))
  )

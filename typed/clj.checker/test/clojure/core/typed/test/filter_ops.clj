(ns clojure.core.typed.test.filter-ops
  (:require 
    ; this loads the type system, must go first
    [typed.clj.checker.test-utils :refer :all]
    [typed.clojure :as t]
    [typed.cljc.checker.filter-ops :refer [-and -filter -not-filter -or]]
    [typed.cljc.checker.filter-rep :refer [-top make-AndFilter make-OrFilter -bot]]
    [typed.cljc.checker.type-rep :refer []]
    [typed.clj.checker.parse-unparse :refer [parse-type]]
    [typed.clj.checker.subtype :refer [subtype-type-filter?]]
    [clojure.test :refer :all]))

(deftest and-filter
  (is-tc-e 1)
  (is (= (-and [-top] (clj-opts))
         -top))
  (is (= (-and [-top -top] (clj-opts))
         -top))
  (is (= (-and [-bot -bot] (clj-opts))
         -bot))
  (is (= (-and [-top -bot] (clj-opts))
         -bot))
  (clj
    (let [f (-filter (parse-type `t/Num (clj-opts)) 'x)]
      (is (= (-and [f] (clj-opts))
             f))
      (is (= (-and [f -top] (clj-opts))
             f))))
  (is-clj 
    (let [pf (-filter (parse-type `t/NonEmptyCount (clj-opts)) 0)
          nf (-not-filter (parse-type nil (clj-opts)) 0)]
      (= (-and [pf nf] (clj-opts))
         (make-AndFilter pf nf))))
  (is-clj
    (subtype-type-filter? (-and [(-filter (parse-type `(t/U nil (t/NonEmptyVec t/Num)) (clj-opts)) 0)
                                 (-filter (parse-type `(t/U nil t/EmptyCount) (clj-opts)) 0)]
                                (clj-opts))
                          (-filter (parse-type `nil (clj-opts)) 0)
                          (clj-opts))))

(deftest or-filter
  (is-tc-e 1)
  (is (= (-or [-bot -bot] (clj-opts))
         -bot))
  (is (= (-or [-top -top] (clj-opts))
         -top))
  (is (= (-or [-top -bot] (clj-opts))
         -top))
  (is (= (-or [-top (-filter (parse-type `nil (clj-opts)) 0)] (clj-opts))
         -top))

  ;normalise to conjunctions of disjunctions
  (is (= (-or [(-and [(-filter (parse-type nil (clj-opts)) 0)
                      (-filter (parse-type nil (clj-opts)) 1)]
                     (clj-opts))
               (-and [(-filter (parse-type nil (clj-opts)) 2)
                      (-filter (parse-type nil (clj-opts)) 3)]
                     (clj-opts))]
              (clj-opts))
         (apply make-AndFilter
                (for [l [0 1]
                      r [2 3]]
                  (make-OrFilter (-filter (parse-type nil (clj-opts)) l)
                                 (-filter (parse-type nil (clj-opts)) r)))))))


;(typed.cljc.checker.filter-ops/-and                                                                                         
;  (! (U nil false) and__3941__auto____#0)
;  (is (U nil false) v2__#1)
;  (! (U nil false) and__3941__auto____#0)
;  (when
;    (is (U nil false) v1__#1)
;    (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (is (U nil false) v1__#1)
;  (when
;    (! (U nil false) v2__#1)
;    (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;    (is (U nil false) and__3941__auto____#0)
;    (! (U nil false) v1__#1))
;  (when
;    (! (U nil false) v1__#1)
;    (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
;  (when
;    (is (U nil false) v2__#1)
;    (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
;
;(&
; (! (U nil false) and__3941__auto____#0)
; (when
;  (is (U nil false) v1__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v1__#0))
; (is (U nil false) v1__#1)
; (when
;  (! (U nil false) v2__#1)
;  (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
; (when
;  (is (U nil false) and__3941__auto____#0)
;  (! (U nil false) v1__#1))
; (when
;  (! (U nil false) v1__#1)
;  (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
; (is (U nil false) v2__#1)
; (when
;  (is (U nil false) v2__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
;
;(typed.cljc.checker.filter-ops/-or
; (&
;  (! (U nil false) v2__#1)
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (is (U nil false) v1__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (&
;  (! nil v1__#0)
;  (! (U nil false) v1__#1)
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0))))
;
;(&
; (| (! nil v1__#0) 
;    (! (U nil false) and__3941__auto____#0))
; (|
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1))
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (|
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v2__#1)
;  (is clojure.core.typed/NonEmptyCount v1__#0))
; (|
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (| (! (U nil false) v1__#1) (! (U nil false) and__3941__auto____#0))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (| (! nil v1__#0) (is (U nil false) v1__#1))
; (|
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (! nil v1__#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (| (! (U nil false) v2__#1) (is (U nil false) and__3941__auto____#0))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (is (U nil false) v1__#1))
; (|
;  (! nil v1__#0)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (! (U nil false) and__3941__auto____#0))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (! nil v1__#0)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0)))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (|
;  (! nil v1__#0)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (when
;  (is (U nil false) v1__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v1__#0))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v2__#1)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (! nil v1__#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (is (U nil false) v1__#1))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (| (is (U nil false) v1__#1) (is (U nil false) and__3941__auto____#0))
; (|
;  (is (U nil false) v1__#1)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (! (U nil false) v1__#1)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (when
;  (! (U nil false) v2__#1)
;  (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (! (U nil false) v1__#1)
;   (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))))
; (|
;  (is clojure.core.typed/NonEmptyCount v1__#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (| (! nil v1__#0) (! (U nil false) v2__#1))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (is (U nil false) v1__#1)
;  (when
;   (! (U nil false) and__3941__auto____#0)
;   (is (U nil false) v1__#1)))
; (|
;  (is (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (when
;   (! (U nil false) v2__#1)
;   (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))
;  (when
;   (is (U nil false) and__3941__auto____#0)
;   (! (U nil false) v1__#1)))
; (|
;  (when
;   (is (U nil false) v1__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v1__#0))
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (|
;  (is (U nil false) v1__#1)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0)))
; (when
;  (is (U nil false) v2__#1)
;  (is (U clojure.core.typed/EmptyCount nil) v2__#0))
; (| (! (U nil false) v2__#1) (! (U nil false) v1__#1))
; (|
;  (! (U nil false) and__3941__auto____#0)
;  (when
;   (is (U nil false) v2__#1)
;   (is (U clojure.core.typed/EmptyCount nil) v2__#0))))

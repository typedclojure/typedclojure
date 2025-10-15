(ns ^:typed.clojure clojure.core.typed.test.proposition-ops
  (:require 
    ; this loads the type system, must go first
    [typed.clj.checker.test-utils :refer :all]
    [typed.clojure :as t]
    [typed.cljc.checker.proposition-ops :refer [-and -proposition -not-proposition -or]]
    [typed.cljc.checker.proposition-rep :refer [-top make-AndProposition make-OrProposition -bot]]
    [typed.clj.checker.parse-unparse :refer [parse-clj]]
    [typed.clj.checker.subtype :refer [subtype-type-proposition?]]
    [clojure.test :refer :all]))

(deftest and-proposition
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
    (let [f (-proposition (parse-clj `t/Num) 'x)]
      (is (= (-and [f] (clj-opts))
             f))
      (is (= (-and [f -top] (clj-opts))
             f))))
  (is-clj 
    (let [pf (-proposition (parse-clj `t/NonEmptyCount) 0)
          nf (-not-proposition (parse-clj nil) 0)]
      (= (-and [pf nf] (clj-opts))
         (make-AndProposition pf nf))))
  (is-clj
    (subtype-type-proposition? (-and [(-proposition (parse-clj `(t/U nil (t/NonEmptyVec t/Num))) 0)
                                 (-proposition (parse-clj `(t/U nil t/EmptyCount)) 0)]
                                (clj-opts))
                          (-proposition (parse-clj `nil) 0)
                          (clj-opts))))

(deftest or-proposition
  (is-tc-e 1)
  (is (= (-or [-bot -bot] (clj-opts))
         -bot))
  (is (= (-or [-top -top] (clj-opts))
         -top))
  (is (= (-or [-top -bot] (clj-opts))
         -top))
  (is (= (-or [-top (-proposition (parse-clj `nil) 0)] (clj-opts))
         -top))

  ;normalise to conjunctions of disjunctions
  (is (= (-or [(-and [(-proposition (parse-clj nil) 0)
                      (-proposition (parse-clj nil) 1)]
                     (clj-opts))
               (-and [(-proposition (parse-clj nil) 2)
                      (-proposition (parse-clj nil) 3)]
                     (clj-opts))]
              (clj-opts))
         (apply make-AndProposition
                (for [l [0 1]
                      r [2 3]]
                  (make-OrProposition (-proposition (parse-clj nil) l)
                                 (-proposition (parse-clj nil) r)))))))


;(typed.cljc.checker.proposition-ops/-and                                                                                         
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
;(typed.cljc.checker.proposition-ops/-or
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

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.open-result
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.filter-rep :as fl]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.subst-obj :as subst-obj]))

;; Used to "instantiate" a Result from a function call.
;; eg. (let [a (ann-form [1] (U nil (Seqable Number)))]
;;       (if (seq a)
;;         ...
;;
;; Here we want to instantiate the result of (seq a).
;; objs is each of the arguments' objects, ie. [obj/-empty]
;; ts is each of the arugments' types, ie. [(U nil (Seqable Number))]
;;
;; The latent result:
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) 0)
;           :else (| (is nil 0)
;                    (is (ExactCount 0) 0))}]))
;; instantiates to 
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) a)
;           :else (| (is nil a)
;                    (is (ExactCount 0) a))}]))
;;
;; Notice the objects are instantiated from 0 -> a
;
;[Result (Seqable RObject) (Option (Seqable Type)) 
;  -> '[Type FilterSet RObject]]
(defn open-Result 
  "Substitute ids for objs in Result t"
  ([r objs] (open-Result r objs nil))
  ([{t :t fs :fl old-obj :o :as r} objs ts]
   {:pre [(r/Result? r)
          (every? obj/RObject? objs)
          ((some-fn fl/FilterSet? fl/NoFilter?) fs)
          (obj/RObject? old-obj)
          ((some-fn nil? (con/every-c? r/Type?)) ts)]
    :post [((con/hvector-c? r/Type? fl/FilterSet? obj/RObject?) %)]}
   ;  (prn "open-result")
   ;  (prn "result type" (prs/unparse-type t))
   ;  (prn "result filterset" (prs/unparse-filter-set fs))
   ;  (prn "result (old) object" old-obj)
   ;  (prn "objs" objs)
   ;  (prn "ts" (mapv prs/unparse-type ts))
   (reduce (fn [[t fs old-obj] [[o k] arg-ty]]
             {:pre [(r/Type? t)
                    ((some-fn fl/FilterSet? fl/NoFilter?) fs)
                    (obj/RObject? old-obj)
                    (integer? k)
                    (obj/RObject? o)
                    ((some-fn false? r/Type?) arg-ty)]
              :post [((con/hvector-c? r/Type? fl/FilterSet? obj/RObject?) %)]}
             (let [r [(subst-obj/subst-type t k o true)
                      (subst-obj/subst-filter-set fs k o true arg-ty)
                      (subst-obj/subst-object old-obj k o true)]]
               ;              (prn [(prs/unparse-type t) (prs/unparse-filter-set fs) old-obj])
               ;              (prn "r" r)
               r))
           [t fs old-obj]
           ; this is just a sequence of pairs of [not-neg? RObject] and Type?
           ; Represents the object and type of each argument, and its position
           (map vector 
                (map-indexed #(vector %2 %1) ;racket's is opposite..
                             objs)
                (if ts
                  ts
                  (repeat false))))))

(defn open-Result->TCResult
  ([r objs] (open-Result->TCResult r objs nil))
  ([r objs ts] (let [[t-r f-r o-r] (open-Result r objs ts)]
                 (r/ret t-r f-r o-r))))

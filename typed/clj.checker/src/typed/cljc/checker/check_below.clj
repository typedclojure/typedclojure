;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check-below
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.object-rep :as obj]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.check.utils :as cu]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]))

;; returns true when f1 <: f2
(defn simple-filter-better? [f1 f2]
  {:pre [(fl/Filter? f1)
         (fl/Filter? f2)]}
  (cond (fl/NoFilter? f2) true
        (fl/NoFilter? f1) false
        :else (sub/subtype-filter? f1 f2)))

;; returns true when f1 <: f2
(defn filter-better? [{f1+ :then f1- :else :as f1}
                      {f2+ :then f2- :else :as f2}]
  {:pre [(fl/FilterSet? f1)
         (fl/FilterSet? f2)]
   :post [(boolean? %)]}
  (cond
    (= f1 f2) true
    :else
    (let [f+-better? (simple-filter-better? f1+ f2+)
          f--better? (simple-filter-better? f1- f2-)] 
      (and f+-better? f--better?))))

(defn bad-filter-delayed-error [{f1 :fl :as actual} {f2 :fl :as expected}]
  {:pre [(r/TCResult? actual)
         (r/TCResult? expected)]}
  (err/tc-delayed-error (str "Expected result with filter " (pr-str f2) ", got filter "  (pr-str f1))
                        :expected expected))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [(or (and (r/TCResult? tr1)
                  (r/TCResult? expected))
             (and (r/Type? tr1)
                  (r/Type? expected)))]
   :post [(if (r/TCResult? tr1)
            (r/TCResult? %)
            (r/Type? %))]}
  (letfn [;; returns true when o1 <: o2
          (object-better? [o1 o2]
            {:pre [(obj/RObject? o1)
                   (obj/RObject? o2)]
             :post [(boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn obj/NoObject? obj/EmptyObject?) o2) true
              :else false))
          (choose-result-filter [f1 f2]
            {:pre [(fl/Filter? f1)
                   (fl/Filter? f2)]
             :post [(fl/Filter? %)]}
            ;(prn "check-below choose-result-filter"
            ;     f1 f2
            ;     (fl/infer-top? f1)
            ;     (fl/infer-top? f2))
            (cond
              (and (fl/infer-top? f2)
                   (not (fl/NoFilter? f1)))
              f1
              :else f2))
          (construct-ret [tres tr1 expected]
            {:pre [(r/AnyType? tres)
                   ((every-pred r/TCResult?) tr1 expected)]
             :post [(r/TCResult? %)]}
            (r/ret tres
                   (let [exp-f (r/ret-f expected)
                         tr-f (r/ret-f tr1)]
                     ;(prn "check-below exp-f" exp-f)
                     ;(prn "check-below tr-f" tr-f)
                     (fo/-FS (choose-result-filter
                               (:then tr-f)
                               (:then exp-f))
                             (choose-result-filter
                               (:else tr-f)
                               (:else exp-f))))
                   (let [exp-o (r/ret-o expected)
                         tr-o (r/ret-o tr1)]
                     ;(prn "exp-o" exp-o (obj/infer-obj? exp-o))
                     (if ((some-fn obj/NoObject? obj/infer-obj?) exp-o)
                       tr-o
                       exp-o))))]
    ;tr1 = arg
    ;expected = dom
    (let [type-special-case? (not (r/TCResult? tr1))
          {t1 :t f1 :fl o1 :o} (cond-> tr1
                                 type-special-case? r/ret)
          {t2 :t f2 :fl o2 :o} (cond-> expected
                                 type-special-case? r/ret)
          tres (or (cgen/eliminate-wild t1 t2)
                   (cu/expected-error t1 expected))
          better-fs? (filter-better? f1 f2)
          ;_ (prn "better-fs?" better-fs? f1 f2)
          better-obj? (object-better? o1 o2)
          _ (cond
              (and (not better-fs?)
                   better-obj?)
              (bad-filter-delayed-error tr1 expected)

              (and better-fs? 
                   (not better-obj?))
              (err/tc-delayed-error (str "Expected result with object " (pr-str o2) ", got object " (pr-str o1))
                                    :expected expected)

              (and (not better-fs?)
                   (not better-obj?))
              (err/tc-delayed-error (str "Expected result with object " (pr-str o2) ", got object"  o1 " and filter "
                                         (pr-str f2) " got filter " (pr-str f1))
                                    :expected expected))]
      (cond-> tres
        (not type-special-case?) (construct-ret tr1 expected)))))

(defn maybe-check-below
  [tr1 expected]
  {:pre [(r/TCResult? tr1)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (-> tr1
      (cond-> expected (check-below expected))
      (vary-meta update :origin-exprs (fnil conj []) vs/*current-expr*)))

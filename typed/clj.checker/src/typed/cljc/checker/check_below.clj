;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
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
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.experimental.infer-vars :as infer-vars]))

;; returns true when f1 <: f2
(defn simple-filter-better? [f1 f2]
  {:pre [(fl/Filter? f1)
         (fl/Filter? f2)]}
  (cond (fl/NoFilter? f2) true
        (fl/NoFilter? f1) false
        :else (sub/subtype-filter? f1 f2)))

(defn subtype? [t1 t2]
  (let [s (sub/subtype? t1 t2)]
    (when (r/Unchecked? t1)
      (when-some [vsym (:vsym t1)]
        (infer-vars/add-inferred-type
          (cu/expr-ns vs/*current-expr*)
          vsym
          t2)))
    s))

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

;; apply f1 to the current environment, and if the type filter
;; is boring enough it will reflect in the updated type environment
;(defn can-extract-in? [env f1 f2]
;  (cond
;    (fl/TypeFilter? f2) (let [good? (atom true)
;                              new-env (update/env+ env [f1] good?)]
;                          (boolean
;                            (when @good?
;                            )))

;; check-below : (/\ (Result Type -> Result)
;;                   (Result Result -> Result)
;;                   (Type Result -> Type)
;;                   (Type Type -> Type))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [((some-fn r/TCResult? r/Type?) tr1)
         ((some-fn r/TCResult? r/Type?) expected)]
   :post [(cond
            (r/TCResult? tr1) (r/TCResult? %)
            (r/Type? tr1) (r/Type? %))]}
  (letfn [;; returns true when o1 <: o2
          (object-better? [o1 o2]
            {:pre [(obj/RObject? o1)
                   (obj/RObject? o2)]
             :post [(boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn obj/NoObject? obj/EmptyObject?) o2) true
              :else false))
          (choose-result-type [t1 t2]
            {:pre [(r/Type? t1)
                   (r/Type? t2)]
             :post [(r/Type? %)]}
            #_
            (prn "choose-result-type"
                 t1 t2
                 (r/infer-any? t1)
                 (r/infer-any? t2))
            (cond
              (r/infer-any? t2) t1
              (and (r/FnIntersection? t2)
                   (= 1 (count (:types t2)))
                   (r/infer-any? (-> t2 :types first :rng :t)))
              (let [rng-t (cgen/unify-or-nil
                            {:fresh [x]
                             :out x}
                            t1
                            (r/make-FnIntersection
                              (r/make-Function
                                (-> t2 :types first :dom)
                                x)))]
                ;(prn "rng-t" rng-t)
                (if rng-t
                  (assoc-in t2 [:types 0 :rng :t] rng-t)
                  t2))
              :else t2))
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
          (construct-ret [tr1 expected]
            {:pre [((every-pred r/TCResult?) tr1 expected)]
             :post [(r/TCResult? %)]}
            (r/ret (choose-result-type
                     (r/ret-t tr1)
                     (r/ret-t expected))
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
                     (prn "exp-o" exp-o (obj/infer-obj? exp-o))
                     (if ((some-fn obj/NoObject? obj/infer-obj?) exp-o)
                       tr-o
                       exp-o))))]
    ;tr1 = arg
    ;expected = dom
    (cond
      (and (r/TCResult? tr1)
           (r/TCResult? expected))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t f2 :fl o2 :o} expected]
        (cond
          (not (subtype? t1 t2)) (cu/expected-error t1 expected)

          :else
          (let [better-fs? (filter-better? f1 f2)
                ;_ (prn "better-fs?" better-fs? f1 f2)
                better-obj? (object-better? o1 o2)
                ]
            (cond
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
                                         (pr-str f2) " got filter " (pr-str f1)))
                                    :expected expected)))
        (construct-ret tr1 expected))

      (and (r/TCResult? tr1)
           (r/Type? expected))
      (let [{t1 :t f :fl o :o} tr1
            t2 expected]
        (when-not (subtype? t1 t2)
          (cu/expected-error t1 (r/ret t2)))
        (r/ret (choose-result-type t1 t2) f o))

      (and (r/Type? tr1)
           (r/TCResult? expected))
      (let [t1 tr1
            {t2 :t f :fl o :o} expected]
        (if (subtype? t1 t2)
          (err/tc-delayed-error (str "Expected result with filter " (pr-str f) " and object " (pr-str o)
                                     ", got trivial filter and empty object."))
          (cu/expected-error t1 expected))
        t1)

      (and (r/Type? tr1)
           (r/Type? expected))
      (let [t1 tr1
            t2 expected]
        (when-not (subtype? t1 t2)
          (cu/expected-error t1 (r/ret t2)))
        (choose-result-type t1 t2))

      :else (let [a tr1
                  b expected]
              (err/int-error (str "Unexpected input for check-below " a b))))))

(defn maybe-check-below
  [tr1 expected]
  {:pre [(r/TCResult? tr1)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (if expected
    (check-below tr1 expected)
    tr1))

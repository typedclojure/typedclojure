;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.nth
  (:require [clojure.core.typed :as t] 
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.invoke :as invoke]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.path-rep :as pe]
            [typed.cljc.checker.object-rep :as obj]
            [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.check.method :as method]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.indirect-ops :as ind])
  (:import (clojure.lang ISeq Seqable)))

(defn ^:private expr->type [expr]
  (some-> expr u/expr-type r/ret-t))

(defn ^:private expr->object [expr]
  (some-> expr u/expr-type r/ret-o))

(defn ^:private expression? [expr]
  (r/TCResult? (u/expr-type expr)))

;; TODO try and replace with cgen + intersection
;; (All [x] [(I x AnyHSequential) -> x])
(defn ^:private find-hsequential [t]
  {:pre [(r/Type? t)]
   :post [(r/HSequential? %)]}
  (or (c/find-hsequential-in-non-union t)
      (err/int-error (str "Cannot find HSequential ancestor: "
                          (prs/unparse-type t)))))

(defn nth-type [types idx default-t]
  {:pre [(every? r/Type? types)
         (nat-int? idx)
         ((some-fn nil? r/Type?) default-t)]
   :post [(r/Type? %)]}
  (apply c/Un
         (doall
           (for [t types]
             (if-some [res-t (cond
                               ;; (nth nil ...) returns default (if any), otherwise nil
                               (ind/subtype? t r/-nil) (or default-t r/-nil)
                               :else (-> t
                                         find-hsequential
                                         ;; TODO handle other HSequential fields like :rest
                                         :types
                                         ;; when default-t is nil and idx is out of bounds,
                                         ;; this returns nil, executing the err/tc-delayed-error
                                         (nth idx default-t)))]
               res-t
               (err/tc-delayed-error (str "Cannot get index " idx
                                          " from type " (prs/unparse-type t))))))))

(defn ^:private nth-positive-filter-default-truthy [target-o default-o]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)]
   :post [(fl/Filter? %)]}
  (fo/-and (fo/-filter-at (c/Un r/-nil (c/RClass-of ISeq [r/-any]))
                          target-o)
           (fo/-not-filter-at r/-falsy
                              default-o)))

(defn ^:private nth-positive-filter-default-falsy [target-o default-o idx]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)
         (nat-int? idx)]
   :post [(fl/Filter? %)]}
  (fo/-and (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                (r/make-CountRange (inc idx)))
                          target-o)
           (fo/-filter-at r/-falsy
                          default-o)))

(defn ^:private nth-positive-filter-default [target-o default-o idx]
  {:pre [(obj/RObject? target-o)
         (obj/RObject? default-o)
         (nat-int? idx)]
   :post [(fl/Filter? %)]}
  (fo/-or (nth-positive-filter-default-truthy target-o default-o)
          (nth-positive-filter-default-falsy target-o default-o idx)))

(defn ^:private nth-positive-filter-no-default [target-o idx]
  {:pre [(obj/RObject? target-o)
         (nat-int? idx)]
   :post [(fl/Filter? %)]}
  (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                       (r/make-CountRange (inc idx)))
                 target-o))

(defn ^:private nth-filter [target-expr default-expr idx default-t]
  {:pre [(expression? target-expr)
         ((some-fn nil? expression?) default-expr)
         (nat-int? idx)
         ((some-fn nil? r/Type?) default-t)]
   :post [(fl/Filter? %)]}
  (let [target-o (expr->object target-expr)
        default-o (expr->object default-expr)

        filter+ (if default-t
                  (nth-positive-filter-default target-o default-o idx)
                  (nth-positive-filter-no-default target-o idx))]
    (fo/-FS filter+
            ;; not sure if there's anything worth encoding here
            fl/-top)))

(defn ^:private nth-object [target-expr idx]
  {:pre [(expression? target-expr)
         (nat-int? idx)]
   :post [(obj/RObject? %)]}
  (let [target-o (expr->object target-expr)]
    (cond-> target-o 
      (obj/Path? target-o)
      (update :path concat [(pe/NthPE-maker idx)]))))

(def nat-value? (every-pred r/Value? (comp nat-int? :val)))

(defn nth-function-type [n]
  {:pre [(nat-int? n)]
   :post [(r/Type? %)]}
  (let [; gensyms are too ugly to read in errors
        x 'x
        y 'y]
    (impl/with-clojure-impl
      (prs/parse-type
        `(t/All [~x ~y]
          (t/IFn 
            [(t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x)) t/Int :-> ~x]
            [(t/I (t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x))
                  (t/CountRange ~(inc n)))
             (t/Val ~n) t/Any :-> ~x]
            [(t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x) nil) t/Int ~y :-> (t/U ~x ~y)]
            [(t/U (clojure.lang.Indexed ~x) (t/SequentialSeqable ~x) nil) t/Int :-> (t/U ~x nil)]))))))

(defn valid-first-arg-for-3-arity-nth? [t]
  {:pre [(r/Type? t)]
   :post [(boolean? %)]}
  (ind/subtype? t
                (impl/with-clojure-impl
                  (prs/parse-type
                    `(t/U (clojure.lang.Indexed t/Any)
                          (t/SequentialSeqable t/Any)
                          nil)))))

(defn invoke-nth [check-fn expr expected]
  {:pre [(#{:host-call :invoke} (:op expr))
         (every? (every-pred (comp #{:unanalyzed} :op)
                             (complement u/expr-type))
                 (:args expr))]
   :post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (if (:target %)
                     (-> % :target u/expr-type r/TCResult?)
                     true)
                   (every? (every-pred (comp (complement #{:unanalyzed}) :op)
                                       (comp r/TCResult? u/expr-type))
                           (:args %))))]}
  (when (#{2 3} (count (:args expr)))
    (let [{[te ne de] :args :as expr} (cond-> (-> expr
                                                  ;TODO avoid repeated checks
                                                  (update :args #(mapv check-fn %)))
                                        (#{:host-call} (:op expr))
                                        (update :target check-fn))
          types (let [ts (c/fully-resolve-type (expr->type te))]
                  (if (r/Union? ts)
                    (:types ts)
                    [ts]))
          num-t (expr->type ne)
          default-t (expr->type de)]
      ;(prn "nth" types)
      (cond
        (and (nat-value? num-t)
             (let [super (c/Un r/-nil r/-any-hsequential)]
               (every? #(ind/subtype? % super)
                       types)))
        (let [idx (:val num-t)]
          (-> expr
              (assoc
                u/expr-type (below/maybe-check-below
                              (r/ret (nth-type types idx default-t)
                                     (nth-filter te de idx default-t)
                                     (nth-object te idx))
                              expected))))

        ; rewrite nth type to be more useful when we have an exact (and interesting) index.
        (nat-value? num-t)
        (let [ft (nth-function-type (-> num-t :val))
              expr (ana2/run-post-passes expr)]
          (case (:op expr)
            :invoke (invoke/normal-invoke
                      check-fn
                      expr
                      (:fn expr)
                      (:args expr)
                      expected
                      :cfexpr (assoc (:fn expr) u/expr-type (r/ret ft))
                      :cargs (:args expr))
            :static-call
            (method/check-invoke-method
              expr expected
              :method-override (nth-function-type (-> num-t :val)))))))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.ast-utils
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST ops


;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(defn emit-form-fn [expr opts]
  (impl/impl-case opts
    :clojure ((requiring-resolve 'typed.clj.analyzer.passes.emit-form/emit-form) expr)
    :cljs ((requiring-resolve 'typed.cljs.checker.util/emit-form) expr)))

(defn constant-expr [expr]
  {:pre [(#{:quote} (:op expr))
         (#{:const} (:op (:expr expr)))]}
  (-> expr :expr :val))

(defn map-expr-at [expr key opts]
  (impl/impl-case opts
    :clojure (case (:op expr)
               :map (let [const ((requiring-resolve 'typed.cljc.analyzer.passes.constant-lifter/constant-lift) expr)]
                      (assert (#{:const} (:op const)))
                      (map-expr-at const key opts))
               :const (let [v (:val expr)]
                        (assert (contains? v key) key)
                        (get v key)))
    :cljs (let [_ (assert (#{:map} (:op expr)))
                m (zipmap (map :form (:keys expr))
                          (:vals expr))
                _ (assert (contains? m key))
                vexpr (get m key)]
            (:form vexpr))))

(defn constant-exprs [exprs]
  (map constant-expr exprs))

(defn quote-expr-val [{:keys [op expr] :as q}]
  {:pre [(or (and (#{:quote} op)
                  (#{:const} (:op expr)))
             (#{:const} op))]}
  (if (#{:quote} op)
    (:val expr)
    (:val q)))

(defn dummy-if-expr [test then else env]
  {:op :if
   :test test
   :then then
   :else else
   :children [:test :then :else]
   :env env})

(defn dummy-invoke-expr [fexpr args env]
  {:op :invoke
   :env env
   :children [:fn :args]
   :fn fexpr
   :args args})

(defn dummy-fn-method-expr [body required-params rest-param env]
  (let [params (vec (concat required-params (when rest-param [rest-param])))]
    {:op :fn-method
     :env env
     :children [:body]
     :body body
     :params params
     :fixed-arity (count params)
     :variadic? (boolean rest-param)}))

(defn dummy-fn-expr [methods variadic-method env]
  {:op :fn
   :env env
   :children [:methods]
   :methods (vec (concat methods (when variadic-method [variadic-method])))
   :variadic? (boolean variadic-method)})

(defn dummy-local-binding-expr [sym env]
  {:op :local
   :env env
   :name sym})

(defn dummy-var-expr [vsym env]
  (let [v (resolve vsym)]
    (assert (var? v))
    {:op :var
     :env env
     :var v
     :form vsym}))

(defn dummy-do-expr [statements ret env]
  {:op :do
   :statements statements
   :ret ret
   :env env})

(defn dummy-const-expr [val env]
  {:op :const
   :val val
   :env env
   :form val})

;; FIXME delete
(defn method-body-kw []
  :body)

(defn method-required-params [method]
  (case (:op method)
    (:fn-method) ((if (:variadic? method) butlast identity)
                  (:params method))
    ;include deftype's 'this' param
    (:method) (concat [(:this method)] (:params method))))

(defn visit-method-params [method f]
  (case (:op method)
    :fn-method (-> method
                   (update :params #(mapv f %)))
    :method (-> method
                (update :this f)
                (update :params #(mapv f %)))))

(defn method-rest-param [method]
  (case (:op method)
    ;deftype methods are never variadic
    (:method) nil
    (:fn-method) ((if (:variadic? method) last (constantly nil))
                  (:params method))))

(defn reconstruct-arglist [method required-params rest-param opts]
  (impl/impl-case opts
    :clojure (case (:op method) 
               :fn-method (assoc method
                                 :params (vec (concat required-params
                                                      (when rest-param
                                                        [rest-param]))))
               :method (do (assert (nil? rest-param))
                           (assert (seq required-params))
                           (assoc method
                                  :this (first required-params)
                                  :params (vec (rest required-params)))))
    :cljs (assoc method
                 :params (vec (concat required-params
                                      (when rest-param
                                        [rest-param]))))))

(defn let-body-kw []
  :body)

(defn def-var-name [expr opts]
  {:post [(symbol? %)]}
  (impl/impl-case opts
    :clojure ((requiring-resolve 'clojure.core.typed.coerce-utils/var->symbol) (:var expr))
    :cljs (:name expr)))

(defn new-op-class [expr]
  {:pre [(#{:new} (:op expr))
         (#{:const} (:op (:class expr)))]
   :post [(class? %)]}
  (-> expr :class :val))

(defn catch-op-class [expr]
  {:pre [(#{:catch} (:op expr))]
   :post [(class? %)]}
  ; future tools.analyzer
  (-> expr :class :val))

(def deftype-method? (fn [m opts]
                       (impl/impl-case opts
                         :clojure ((every-pred map? (comp #{:method} :op))
                                   m)
                         ; FIXME should be nyi-error but c.c.t.errors depends on this namespace
                         :cljs (assert nil "Method for CLJS"))))

(def fn-method? (fn [m]
                  ((every-pred map? (comp #{:fn-method} :op))
                   m)))
#_
(def fn-methods? (fn [ms opts]
                   (impl/impl-case opts
                     :clojure ((con/vec-c? fn-method?) ms)
                     :cljs ((every-pred (con/every-c? fn-method?)
                                        seq?)
                            ms))))

(defn variadic-method? [m opts]
  {:pre [((some-fn fn-method? #(deftype-method? % opts)) m)]
   :post [(boolean? %)]}
  (cond
    (fn-method? m) (:variadic? m)
    ; :method does not have :variadic? field
    :else false))

(defn fixed-arity
  "Returns the number of parameters for a :fn-method or :method.
  Note :method AST nodes include the 'this' parameter."
  [m opts]
  {:pre [((some-fn fn-method? #(deftype-method? % opts)) m)]
   :post [(integer? %)]}
  (let [fixed (:fixed-arity m)]
    (assert (integer? fixed))
    ((if (fn-method? m) identity inc) fixed)))

(defn walk-children [check {:keys [children] :as expr}]
  (reduce
    (fn [expr c]
      (update expr c 
              (fn [ce]
                (if (vector? ce)
                  (mapv check ce)
                  (check ce)))))
    expr
    children))

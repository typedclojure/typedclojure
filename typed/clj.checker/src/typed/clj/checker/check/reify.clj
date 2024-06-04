;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;TODO share with typed.clj.checker.check.deftype
(ns typed.clj.checker.check.reify
  (:require [clojure.core.typed.errors :as err]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check-below :as below]
            [typed.clj.ext.clojure.core__reify :as reify]
            [typed.clj.analyzer.utils :as ju]
            [typed.cljc.runtime.env :as env]
            [typed.clj.checker.method-override-env :as mth-override]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.check.fn-methods :as fn-methods]
            [typed.cljc.checker.check.utils :as cu]
            [typed.clj.checker.parse-unparse :as prs]))

(defn IFn-invoke-method-t [this-t t]
  (let [t (c/fully-resolve-type t)]
    (cond
      (r/FnIntersection? t) (apply r/make-FnIntersection
                                   (map (fn [t]
                                          (cond
                                            (not= :fixed (:kind t)) (assert nil "TODO IFn-invoke-method-t (or rest drest prest pdot kws)")
                                            :else (update t :dom #(cons this-t %))))
                                        (:types t)))
      (and (r/RClass? t)
           (= 'clojure.lang.IFn (:the-class t))) (r/TopFunction-maker)
      :else (assert nil (str "TODO: " `IFn-invoke-method-t " " (class t))))))

(defn check-inst-fn-methods [inst-methods {:keys [kind expected-ifn] :as _opts}]
  {:pre [(#{:reify :deftype} kind)
         (every? (comp #{:method} :op) inst-methods)]
   :post [(every? (comp #{:method} :op) %)]}
  (:methods
    (fn-methods/check-fn-methods
      inst-methods
      expected-ifn
      {:check-rest-fn (fn [& args] (err/int-error (format "%s method cannot have rest parameter" (name kind))))
       :recur-target-fn
       (fn [{:keys [dom] :as f}]
         {:pre [(r/Function? f)]
          :post [(recur-u/RecurTarget? %)]}
         (recur-u/RecurTarget-maker (rest dom) nil nil nil))
       :validate-expected-fn
       (fn [fin]
         {:pre [(r/FnIntersection? fin)]}
         (when (not-every? #(= :fixed (:kind %)) (:types fin))
           (err/int-error
             (str "Cannot provide rest arguments to " (name kind) " method: "
                  (prs/unparse-type fin)))))})))

(defn check-reify
  [expr expected]
  (let [{original-reify-form :form :as original-reify-expr} (-> expr :form first meta ::reify/original-reify-expr)
        _ (assert original-reify-form)
        class->info (into {}
                          (comp (partition-all 2)
                                (drop-while (every-pred #(= 2 (count %))
                                                        (comp keyword? first)))
                                (mapcat identity)
                                (remove seq?)
                                (map (fn [prot+class-syn]
                                       (let [v (let [v (resolve prot+class-syn)]
                                                 (when (var? v)
                                                   v))
                                             cls (ju/maybe-class
                                                   (if v
                                                     (:on @v)
                                                     prot+class-syn))
                                             _ (assert (class? cls) (pr-str cls))]
                                         (when-not (#{Object clojure.lang.IObj} cls)
                                           [cls (cond-> {:prot+interface-syntax prot+class-syn
                                                         :static-type (or (when-some [[_ t] (-> prot+class-syn meta (find :typed.clojure/replace))]
                                                                            (prs/parse-type t))
                                                                          (when v
                                                                            (c/Protocol-with-unknown-params (symbol v)))
                                                                          (c/RClass-of-with-unknown-params cls))}
                                                  v (assoc :protocol v))])))))
                          (next original-reify-form))
        class->info (-> class->info
                        (assoc clojure.lang.IObj {:static-type (c/RClass-of clojure.lang.IObj)}
                               Object {:static-type (c/RClass-of Object)}))
        this-t (apply c/In (map (comp :static-type val) class->info))]
    (binding [vs/*current-expr* original-reify-expr]
      (-> expr
          (update :methods (fn [methods]
                             (let [methods (map #(into % (select-keys (ana2/run-passes %) [:methods])) methods)]
                               (doseq [[method-name methods] (group-by :name methods)
                                       :let [inst-methods (mapcat :methods methods)
                                             declaring-class (-> inst-methods first :declaring-class)
                                             _ (assert (simple-symbol? declaring-class) (vec inst-methods))
                                             _ (assert (apply = declaring-class (map :declaring-class inst-methods))
                                                       (vec inst-methods))
                                             msym (symbol (str declaring-class) (name method-name))
                                             {:keys [static-type] :as info} (get class->info (ju/maybe-class declaring-class))
                                             _ (assert info)
                                             override-t (or (mth-override/get-method-override (env/checker) msym)
                                                            (cond
                                                              (= msym 'clojure.lang.IFn/invoke) (IFn-invoke-method-t this-t (:static-type info))
                                                              (:protocol info) (let [static-type (c/fully-resolve-type static-type)
                                                                                     _ (when-not (r/Protocol? static-type)
                                                                                         (let [prot-sym (symbol (:protocol info))
                                                                                               prot-name (-> prot-sym name symbol)]
                                                                                           (err/int-error
                                                                                             (str "Missing annotation for reify ancestor "
                                                                                                  (symbol (:protocol info))
                                                                                                  ". e.g., "
                                                                                                  (format "(reify ^{:typed.clojure/replace (%s ...)} %s)"
                                                                                                          prot-name prot-name)))))
                                                                                     mtype (get-in static-type [:methods method-name])]
                                                                                 (assert mtype (format "Missing annotation for method %s on protocol %s" 
                                                                                                       method-name
                                                                                                       (symbol (:protocol info))
                                                                                                       ". Please add with ann-protocol."))
                                                                                 mtype)))]]
                                 (if override-t
                                   (check-inst-fn-methods
                                     methods
                                     {:kind :reify
                                      :expected-ifn override-t})
                                   (doseq [method methods
                                           :let [_ (assert (= 1 (count (:methods method))))
                                                 m (first (:methods method))
                                                 _ (prn m)
                                                 rfin-type (cu/extend-method-expected this-t (cu/instance-method->Function m))]]
                                     (check-inst-fn-methods
                                       [method]
                                       {:kind :reify
                                        :expected-ifn rfin-type})))))
                             methods))
          (assoc u/expr-type (below/maybe-check-below
                               (r/ret this-t
                                      (fo/-true-filter))
                               expected))))))

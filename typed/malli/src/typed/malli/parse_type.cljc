;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.malli.parse-type
  "Parsing Typed Clojure syntax to malli"
  (:require [clojure.set :as set]
            [typed.clojure :as t]
            [clojure.core.typed.parse-ast :as ast]
            [clojure.core.typed.unsafe :as unsafe]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.ast-ops :as ops]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [malli.core :as m]
            [typed.malli :as tm]))

(declare ast->malli-syntax)

(defmulti Class->malli-syntax
  "Returns an unevaluated malli schema."
  (fn [{:keys [name]} _args _opts] name))
(defmethod Class->malli-syntax 'java.lang.Number [_ _ _] `number?)
(defmethod Class->malli-syntax 'java.lang.String [_ _ _] :string)
(defmethod Class->malli-syntax 'java.lang.Boolean [_ _ _] `boolean?)
(defmethod Class->malli-syntax 'clojure.lang.IPersistentMap [_ args opts]
  (if (nil? args)
    `map?
    (case (count args)
      2 [:map
         (ast->malli-syntax (first args))
         (ast->malli-syntax (second args))])))
(defmethod Class->malli-syntax 'clojure.lang.IPersistentVector [_ args opts]
  (if (nil? args)
    `vector?
    (case (count args)
      1 [:vector
         (ast->malli-syntax (first args))])))
(defmethod Class->malli-syntax :default [t _ _] (err/int-error (str "Don't know how to convert Class annotation " (:form t) " to malli "
                                                                    "via " `Class->malli-syntax)))

(defmulti Name->malli-syntax
  "Returns an unevaluated malli schema or nil."
  (fn [{nme :name} _args _opts]
    (when-some [nme-nsym (some-> nme namespace symbol)]
      (symbol 
        (name ({'clojure.core.typed 'typed.clojure}
               nme-nsym
               nme-nsym))
        (name nme)))))
(defmethod Name->malli-syntax 'typed.clojure/AnyInteger [_ _ _] :int)
(defmethod Name->malli-syntax :default [_ _ _] nil)

(defn ast->malli-syntax 
  "Returns an unevaluated malli schema."
  [t]
  (letfn [(gen-inner [{:keys [op] :as t} opts]
            (let [_ (assert (not ((::tm/seen opts) t))
                            (str "Unhandled recursive type detected: " (:form t)))
                  opts (update opts ::tm/seen conj t)]
              (case op
                ;; TODO use lazy registries to delay resolution until runtime
                ;; https://github.com/metosin/malli#lazy-registries
                :Name (or (Name->malli-syntax t nil opts)
                          (gen-inner (ops/resolve-Name t) opts))
                :Class (Class->malli-syntax t nil opts)
                :U (let [{:keys [types]} t
                         ;; note: would skip any Name/Class overrides
                         all-resolved (map ops/fully-resolve-type types)
                         dispatch-key (when (every? (comp #{:HMap} :op) all-resolved)
                                        (let [mandatories (map (comp #(apply hash-map %) :mandatory) all-resolved)
                                              common-keys (apply set/intersection
                                                                 (map (fn [mandatory]
                                                                        (into #{}
                                                                              (filter (every-pred (comp #{:singleton} :op)
                                                                                                  (comp keyword? :val)))
                                                                              (keys mandatory)))
                                                                      mandatories))]
                                          (some (fn [k]
                                                  (when (every? (comp #{:singleton} :op #(get % k))
                                                                mandatories)
                                                    k))
                                                common-keys)))]
                     (cond
                       dispatch-key (into [:multi {:dispatch (:val dispatch-key)}]
                                          (map (fn [t]
                                                 (let [mandatory (apply hash-map (:mandatory t))
                                                       dispatch-val (-> mandatory (get dispatch-key) :val)]
                                                   [dispatch-val
                                                    (gen-inner t opts)])))
                                          all-resolved)
                       :else (let [names (mapv (comp ::tm/name meta :form) types)]
                               (if (and (every? identity names)
                                        (apply distinct? names))
                                 (into [:orn]
                                       (map (fn [t]
                                              [(-> t :form meta ::tm/name)
                                               (gen-inner t opts)]))
                                       types)
                                 (into [:or]
                                       (map #(gen-inner % opts))
                                       types)))))
                :singleton (cond
                             (nil? (:val t)) :nil
                             :else [:= (:val t)])
                :HVec (let [{:keys [types drest rest repeat]} t]
                        (assert (not drest))
                        (assert (<= (count (filter identity [drest rest repeat]))
                                    1))
                        (cond
                          drest (err/int-error "HVec with unexpanded ... is unsupported in ast->malli-syntax")
                          ;; TODO coerce to vector schema
                          ;; TODO wrap in :schema
                          rest (if (seq types)
                                 (-> [:cat]
                                     (into (map #(gen-inner % opts)) types)
                                     (conj [:* (gen-inner rest opts)]))
                                 [:* (gen-inner rest opts)])
                          ;; TODO coerce to vector schema
                          ;; TODO wrap in :schema
                          repeat [:* (-> [:cat]
                                         (into (map #(gen-inner % opts)) types))]
                          :else (-> [:tuple]
                                    (into (map #(gen-inner % opts)) types))))
                :HMap (let [{:keys [mandatory optional absent-keys complete?]} t
                            mandatory (sort-by
                                        first
                                        (sequence
                                          (map (fn [[k v]]
                                                 [(case (:op k)
                                                    :singleton (:val k))
                                                  (gen-inner v opts)]))
                                          (partition 2 mandatory)))
                            optional (sort-by
                                       first
                                       (sequence
                                         (map (fn [[k v]]
                                                [(case (:op k)
                                                   :singleton (:val k))
                                                 {:optional true}
                                                 (gen-inner v opts)]))
                                         (partition 2 optional)))]
                          (-> [:map]
                              ;; malli does not support absent-keys
                              (cond->
                                (or complete? (seq absent-keys))
                                (conj {:closed true}))
                              (into mandatory)
                              (into optional)))
                :TApp (let [{:keys [rator rands]} t]
                        (case (:op rator)
                          :Name (or (Name->malli-syntax rator (vec rands) opts)
                                    (gen-inner (update t :rator ops/resolve-Name) opts))
                          :Class (Class->malli-syntax rator (vec rands) opts)
                          :TFn (gen-inner (ops/instantiate-TFn rator rands) opts)
                          (err/int-error (str "Don't know how to apply type: " (:form t)))))
                :Any :any
                (err/int-error (str op " not supported in ast->malli-syntax " (:form t))))))]
    (gen-inner t {::tm/seen #{}})))

(defn type-syntax->malli-syntax
  "Returns an unevaluated malli schema."
  [t]
  (impl/with-impl impl/clojure
    (-> (ast/parse t)
        ast->malli-syntax)))

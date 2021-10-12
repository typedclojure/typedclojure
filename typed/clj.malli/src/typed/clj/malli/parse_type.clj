;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.malli.parse-type
  "Parsing Typed Clojure syntax to malli"
  (:require [clojure.set :as set]
            [clojure.core.typed :as t]
            [clojure.core.typed.parse-ast :as ast]
            [clojure.core.typed.unsafe :as unsafe]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.ast-ops :as ops]
            [clojure.core.typed.errors :as err]
            [malli.core :as m]
            [typed.clj.malli :as tm]))

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
(defmethod Class->malli-syntax :default [t _ _] (err/int-error (str "Don't know how to convert " (:form t)
                                                             " to malli")))

(defmulti Name->malli-syntax
  "Returns an unevaluated malli schema or nil."
  (fn [{:keys [name]} _args _opts] name))
(defmethod Name->malli-syntax 'clojure.core.typed/AnyInteger [_ _ _] :int)
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

(defn malli-syntax->parser-type
  "The types of values returned by malli.core/parse as Typed Clojure syntax.
  Takes unevalated malli syntax."
  [m]
  (letfn [(gen-inner [m opts]
            (or (get-in opts [:schema-form->free (m/form m)])
                (let [_ (assert (<= (get (:seen opts) m 0)
                                    2) m)
                      opts (update-in opts [:seen m] (fnil inc 0))
                      gen-inner (fn
                                  ([m] (gen-inner m opts))
                                  ([m opts] (gen-inner m opts)))]
                  (case (m/type m)
                    keyword? `t/Keyword
                    (:any any?) `t/Any
                    (:nil nil?) `(t/Val nil)
                    boolean? `t/Bool
                    number? `Number
                    (:string string?) `t/Str
                    :map-of (let [[kt vt :as cts] (map gen-inner (m/children m))
                                  _ (assert (= 2 (count cts)) (m/children m))]
                              `(t/Map ~kt ~vt))
                    :? `(t/Nilable ~(gen-inner (m/-get m 0 nil)))
                    :* `(t/Vec ~(gen-inner (m/-get m 0 nil)))
                    :ref (let [n (m/-ref m)
                               _ (assert ((some-fn keyword? symbol? string?) n))
                               _ (assert (re-matches #"^[a-zA-Z]+[a-zA-Z0-9]*$" (name n)) n)
                               gn (gensym (name n))]
                           `(t/Rec [~gn] ~(gen-inner (m/-deref m) (assoc-in opts [:schema-form->free (m/form m)] gn))))
                    :int `t/AnyInteger
                    :catn `'~(into {}
                                   (map (fn [[cat-k _props cat-schema :as c]]
                                          (assert (= 3 (count c)) c)
                                          (assert ((some-fn string? keyword? symbol? number?) cat-k) c)
                                          (assert (m/schema? cat-schema) c)
                                          [cat-k (gen-inner cat-schema)]))
                                   (m/children m))
                    :orn `(t/U ~@(mapv (fn [[case-k _props case-schema :as c]]
                                         (assert (= 3 (count c)) c)
                                         (assert ((some-fn string? keyword? symbol? number?) case-k) c)
                                         (assert (m/schema? case-schema) c)
                                         `'[(t/Val ~case-k)
                                            ~(gen-inner case-schema)])
                                       (m/children m)))
                    ::m/schema (gen-inner (m/deref m))
                    := `(t/Val ~(first (m/children m)))
                    :schema (gen-inner (m/deref m))))))]
    `(t/U (t/Val ::m/invalid)
          ~(gen-inner (-> m eval m/schema) {:seen {}}))))
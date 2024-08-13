;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.analyzer.passes.uniquify
  (:require [typed.cljc.analyzer.ast :refer [update-children children]]
            [typed.cljc.analyzer.env :as env]
            [typed.cljc.analyzer.utils :as u]))

(defn push-new-locals-frame
  "Binding expressions like let/loop need to create
  a new locals frame before descending into the body
  to ensure any uniquified locals are not leaked outside
  the expression that introduces them."
  [{::keys [locals-counter locals-frame locals-frame-val] :as env}]
  ;; initialize ::locals-frame etc., when passed the env of an unanalyzed top-level ast
  (let [locals-frame (atom (if locals-frame @locals-frame {}))]
    (-> env
        (assoc ::locals-frame locals-frame)
        (cond->
          (not locals-counter) (assoc ::locals-counter (atom {}))
          ;; immutable copy for type resolution later
          (not locals-frame-val) (assoc ::locals-frame-val @locals-frame)))))

(defn normalize
  "Returns the uniquified binding for name in the current
  locals frame, if any."
  [name
   {::keys [locals-frame] :as _env}]
  {:pre [(symbol? name)]
   :post [(symbol? %)]}
  (@locals-frame name name))

(defn uniquify!
  "Assigns a uniquified binding for name in the current
  locals frame and returns the uniquified binding."
  [name
   {::keys [locals-counter locals-frame] :as _env}]
  {:pre [(symbol? name)]
   :post [(symbol? %)
          (not= % name)]}
  (let [uniquified (symbol (str
                             ;; Add extra gensym so bindings
                             ;; don't get clobbered when they
                             ;; travel between `do` expressions.
                             ;; eg. 
                             ;;   (do (let [m (ann-form 1 Any)]
                             ;;         (assert (number? m))
                             ;;         m)
                             ;;       (let [m (ann-form 1 Any)]
                             ;;         (ann-form m Number)))
                             ;;
                             ;; Actually, unsure if this is possible in practice.
                             ;; I don't have a test case to prove it, so better
                             ;; safe than sorry.
                             ;; - Ambrose
                             (identity #_gensym name)
                             "__#"
                             (-> (swap! locals-counter update name (fnil inc -1))
                                 (get name))))]
    (-> (swap! locals-frame assoc name uniquified)
        (get name))))

(defmulti -uniquify-locals :op)

(defn pre-uniquify-child
  [child-ast env]
  (let [locals-counter (::locals-counter env)
        locals-frame (::locals-frame env)
        locals-frame-val (::locals-frame-val env)]
    (assoc child-ast :env
           (-> (:env child-ast)
               (assoc ::locals-frame locals-frame)
               ;; immutable copy for type resolution later
               (assoc ::locals-frame-val @locals-frame)
               (assoc ::locals-counter locals-counter)))))

(defn uniquify-locals-around
  [{:keys [env] :as ast} opts]
  (let [ast (cond-> ast
              (-> (env/deref-env opts) :passes-opts :uniquify/uniquify-env)
              (update-in [:env :locals]
                         u/update-vals #(update % :name normalize env)))]
   (-uniquify-locals ast)))

(defn uniquify-locals* [{:keys [env] :as ast}]
  (update-children ast #(pre-uniquify-child % env)))

(defmethod -uniquify-locals :local
  [{:keys [env] :as ast}]
  (cond-> ast
    ;; deftype fields cannot be uniquified to allow field access/set! to work
    (not= :field (:local ast)) (update :name normalize env)))

(defn uniquify-binding
  [{:keys [env] :as b}]
  (-> b
      ;; inits need to be uniquified before the local
      ;; to avoid potential shadowings
      (update :init pre-uniquify-child
              (-> env
                  (update ::locals-frame (comp atom deref))))
      (update :name uniquify! env)))

(defmethod -uniquify-locals :letfn
  [{:keys [env] :as ast}]
  (doseq [{:keys [name]} (:bindings ast)] ;; take into account that letfn
    (uniquify! name env))                 ;; accepts parallel bindings
  (uniquify-locals* ast))

(defmethod -uniquify-locals :binding
  [{:keys [env local] :as ast}]
  (case local
    (:let :loop)
    (uniquify-binding ast)

    :letfn
    (-> ast
        (update :name normalize env)
        uniquify-locals*)

    :field
    ast

    (-> ast
        (update :name uniquify! env))))

(defn- is-binding? [node]
  (= (:op node) :binding))

(defn -some
  "Like `clojure.core/some`, but uses an iterator over `lst`."
  [f ^Iterable lst]
  (when-let [it (some-> lst .iterator)]
    (loop []
      (when (.hasNext it)
        (or (f (.next it))
            (recur))))))

(defn- some-child-is-binding? [ast]
  ;; More efficient version of:
  ;; (some #(= :binding (:op %)) (children ast))
  (-some (fn [x]
           (let [child (get ast x)]
             (if (vector? child)
               (-some is-binding? child)
               (is-binding? child))))
         (:children ast)))

(defmethod -uniquify-locals :default
  [{:keys [env] :as ast}]
  (-> ast
      (cond->
        ;; if some expr that introduces new bindings
        (some-child-is-binding? ast)
        ;; then set up frame so locals won't leak
        (update :env push-new-locals-frame))
      uniquify-locals*))

(defn uniquify-locals
  "Walks the AST performing alpha-conversion on the :name field
   of :local/:binding nodes, invalidates :local map in :env field

  Adds ::locals-counter, ::locals-frame, and ::locals-frame-val to
  ast's :env if missing, and propagates to direct children.

  Passes opts:
  * :uniquify/uniquify-env  If true, uniquifies the :env :locals map"
  {:pass-info {:walk :pre :depends #{}}}
  [ast opts]
  (let [env (:env ast)
        locals-counter (::locals-counter env)
        locals-frame (::locals-frame env)
        locals-frame-val (::locals-frame-val env)]
    (-> ast
        ;; initialize top of AST tree
        (cond->
            (not locals-counter) (assoc-in [:env ::locals-counter] (atom {}))
            (not locals-frame) (assoc-in [:env ::locals-frame] (atom {}))
            ;; immutable copy for type resolution later
            (not locals-frame-val) (assoc-in [:env ::locals-frame-val] {}))
        (uniquify-locals-around opts))))

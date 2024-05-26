;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.ast-ops
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  (:require [clojure.core.typed.errors :as err]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

(defn resolve-Name [{:keys [name] :as expr}]
  {:pre [(#{:Name} (:op expr))]}
  (let [e ((requiring-resolve 'typed.cljc.runtime.env-utils/force-type)
           (get ((requiring-resolve 'clojure.core.typed.current-impl/alias-env)) name))
        _ (when-not e
            (err/int-error (str "No alias found for " name)))]
    e))

;copied from tools.analyzer
(defn children*
  "Return a vector of the children expression of the AST node, if it has any.
  The returned vector returns the childrens in the order as they appear in the
  :children field of the AST, and may be either a node or a vector of nodes."
  [{:keys [children] :as ast}]
  (when children
    (mapv ast children)))

;copied from tools.analyzer
(defn update-children
  "Applies `f` to the nodes in the AST nodes children.
  Optionally applies `fix` to the children before applying `f` to the
  children nodes and then applies `fix` to the update children.
  An example of a useful `fix` function is `rseq`."
  ([ast f] (update-children ast f identity))
  ([ast f fix]
   (if-let [c (children* ast)]
     (reduce (fn [ast [k v]]
               (assoc ast k (if (vector? v)
                              (fix (mapv f (fix v)))
                              (f v))))
             ast (mapv list (fix (:children ast)) (fix c)))
     ast)))

;copied from tools.analyzer
(defn rseqv
  "Same as (comp vec rseq)"
  [v]
  (vec (rseq v)))

;copied from tools.analyzer
(defn walk
  "Walk the ast applying pre when entering the nodes, and post when exiting.
  If reversed? is not-nil, pre and post will be applied starting from the last
  children of the AST node to the first one."
  ([ast pre post]
   (walk ast pre post false))
  ([ast pre post reversed?]
   (let [fix (if reversed? rseqv identity)
         walk #(walk % pre post reversed?)]
     (post (update-children (pre ast) walk fix)))))

;copied from tools.analyzer
(defn prewalk
  "Shorthand for (walk ast f identity)"
  [ast f]
  (walk ast f identity))

;copied from tools.analyzer
(defn postwalk
  "Shorthand for (walk ast identity f reversed?)"
  ([ast f]
   (walk ast identity f false))
  ([ast f reversed?]
   (walk ast identity f reversed?)))

(declare -replace-frees)

(defn replace-frees* [ast replacem]
  (update-children ast #(-replace-frees % replacem)))

(defmulti -replace-frees (fn [e & args] (:op e)))
(defmethod -replace-frees :F
  [t replacem]
  (or (replacem (:name t))
      t))

(defmethod -replace-frees :default
  [ast replacem]
  (replace-frees* ast replacem))

(defn replace-frees [t replacem]
  (-replace-frees t replacem))

(defn unwrap-rec [{:keys [f type] :as rec} unwrap-id]
  (replace-frees type 
                 {(:name f) (assoc rec :unwrap-id unwrap-id)}))

(defn instantiate-TFn [{:keys [binder body] :as tfn} args]
  (let [names (map :name binder)]
    (replace-frees body
                   (zipmap names args))))

(defn fully-resolve-type [t]
  (loop [t t
         seen #{}]
    (assert (not (seen t)) (str "Unhandled infinite type " t))
    (let [seen (conj seen t)]
      (case (:op t)
        :Name (recur (resolve-Name t)
                     seen)
        :TFn (recur (instantiate-TFn t)
                    seen)
        :Rec (recur (unwrap-rec t)
                    seen)
        t))))

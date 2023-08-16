;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from clojure.tools.analyzer.ast
(ns typed.cljc.analyzer.ast
  "Utilities for AST walking/updating"
  (:require [typed.cljc.analyzer.utils :refer [into! rseqv mapv']]))

(set! *warn-on-reflection* true)

(defprotocol IASTWalk
  (children-of* [this])
  (update-children* [this f]))

(defn children*
  "Return a vector of vectors of the children node key and the children expression
   of the AST node, if it has any.
   The returned vector returns the childrens in the order as they appear in the
   :children field of the AST, and the children expressions may be either a node
   or a vector of nodes."
  [{:keys [children] :as ast}]
  (when children
    (mapv #(find ast %) children)))

(defn children
  "Return a vector of the children expression of the AST node, if it has any.
   The children expressions are kept in order and flattened so that the returning
   vector contains only nodes and not vectors of nodes."
  [ast]
  (persistent!
   (reduce (fn [acc [_ c]] ((if (vector? c) into! conj!) acc c))
           (transient []) (children* ast))))

;; return transient or reduced holding transient
(defn ^:private -update-children
  [ast f r?]
  (let [fix (if r? rseqv identity)]
    (reduce (fn [ast [k v]]
              (let [multi (vector? v)
                    val (if multi (mapv' f (fix v)) (f v))]
                (if (reduced? val)
                  (reduced (reduced (assoc! ast k (if multi (fix @val) @val))))
                  (assoc! ast k (if multi (fix val) val)))))
            (transient ast)
            (fix (children* ast)))))

;; return ast or reduced holding ast
(defn ^:private -update-children-no-transient
  [ast f r?]
  (let [fix (if r? rseqv identity)]
    (reduce (fn [ast [k v]]
              (let [multi (vector? v)
                    val (if multi (mapv' f (fix v)) (f v))]
                (if (reduced? val)
                  (reduced (reduced (assoc ast k (if multi (fix @val) @val))))
                  (assoc ast k (if multi (fix val) val)))))
            ast
            (fix (children* ast)))))

(defn update-children-reduced
  "Like update-children but returns a reduced holding the AST if f short-circuited."
  ([ast f] (update-children-reduced ast f false))
  ([ast f reversed?]
     (if (and (not (reduced? ast))
              (:children ast))
       (if (instance? typed.cljc.analyzer.ast.IASTWalk ast)
         (let [_ (assert (not reversed?) "Reversed? not supported")
               ret (.update-children* ^typed.cljc.analyzer.ast.IASTWalk ast f)]
           (assert (not (reduced? ret)) "Not supported")
           ret)
         (if (instance? clojure.lang.IEditableCollection ast)
           (let [ret (-update-children ast f reversed?)]
             (if (reduced? ret)
               (reduced (persistent! @ret))
               (persistent! ret)))
           (let [ret (-update-children-no-transient ast f reversed?)]
             (if (reduced? ret)
               (reduced @ret)
               ret))))
       ast)))

(defn update-children
  "Applies `f` to each AST children node, replacing it with the returned value.
   If reversed? is not-nil, `pre` and `post` will be applied starting from the last
   children of the AST node to the first one.
   Short-circuits on reduced."
  ([ast f] (update-children ast f false))
  ([ast f reversed?]
     (unreduced (update-children-reduced ast f reversed?))))

(defn walk
  "Walk the ast applying `pre` when entering the nodes, and `post` when exiting.
   Both functions must return a valid node since the returned value will replace
   the node in the AST which was given as input to the function.
   If reversed? is not-nil, `pre` and `post` will be applied starting from the last
   children of the AST node to the first one.
   Short-circuits on reduced."
  ([ast pre post]
     (walk ast pre post false))
  ([ast pre post reversed?]
     (unreduced
      ((fn walk [ast pre post reversed?]
         (let [walk #(walk % pre post reversed?)]
           (if (reduced? ast)
             ast
             (let [ret (update-children-reduced (pre ast) walk reversed?)]
               (if (reduced? ret)
                 ret
                 (post ret))))))
       ast pre post reversed?))))

(defn prewalk
  "Shorthand for (walk ast f identity)"
  [ast f]
  (walk ast f identity))

(defn postwalk
  "Shorthand for (walk ast identity f reversed?)"
  ([ast f]
     (postwalk ast f false))
  ([ast f reversed?]
     (walk ast identity f reversed?)))

(defn nodes
  "Returns a lazy-seq of all the nodes in the given AST, in depth-first pre-order."
  [ast]
  (lazy-seq
   (cons ast (mapcat nodes (children ast)))))

(defn ast->eav
  "Returns an EAV representation of the current AST that can be used by
   Datomic's Datalog."
  [ast]
  (let [children (set (:children ast))]
    (mapcat (fn [[k v]]
              (if (children k)
                (if (map? v)
                  (into [[ast k v]] (ast->eav v))
                  (mapcat (fn [v] (into [[ast k v]] (ast->eav v))) v))
                [[ast k v]])) ast)))

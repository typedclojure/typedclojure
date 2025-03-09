;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.spec.subtype.test
  "Utilities to help debug and test the spec subtyping lattice."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as db]
            [typed.clj.spec.subtype :as sub]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as gen]))

(db/db-rel distinct-rel t1 t2)
(db/db-rel sub-rel t1 t2)

(defn sup-rel [t2 t1]
  (sub-rel t1 t2))

(def populate-db-from-rels
  "Given a map of rels from (sub/rels), returns a clojure.core.logic.pldb
  database populated with facts from rels."
  (memoize
    (fn [rels]
      (apply db/db-facts
             db/empty-db
             (concat (mapcat (fn [[k vs]]
                               (for [v vs]
                                 [sub-rel k v]))
                             (::sub/sub-rel rels))
                     (mapcat (fn [[k vs]]
                               (for [v vs]
                                 [distinct-rel k v]))
                             (::sub/distinct-rel rels)))))))

(defn db
  "Returns a clojure.core.logic.pldb database populated with facts from (sub/rels)."
  []
  (populate-db-from-rels (sub/rels)))

(defn find-all-sub-rels
  "Returns all edges in the sub-rel relation."
  []
  (let [srels (l/run-db*
                (db) [q]
                (l/fresh [t1 t2]
                         (l/== q [t1 t2])
                         (sub-rel t1 t2)))]
    (doseq [s (mapcat identity srels)]
      (when (symbol? s)
        (assert (resolve s) s)))
    srels))

(defn find-bots
  "Find the bottom types in the direct sub-rel hierarchy.

  Returns a list of all edges to bottom types."
  []
  (let [all-srels (find-all-sub-rels)
        all-rhs (into #{} (map second) all-srels)]
    (sequence
      (remove (comp all-rhs first))
      all-srels)))

(defn find-tops
  "Find the top types in the direct sub-rel hierarchy.

  Returns a list of all edges to top types.
  "
  []
  (let [all-srels (find-all-sub-rels)
        all-lhs (into #{} (map first) all-srels)]
    (sequence
      (remove (comp all-lhs second))
      all-srels)))

(defn find-repeats
  "Return a list of types that occur more than once on the rhs
  of a direct sub-rel rule."
  []
  (let [all-srels (find-all-sub-rels)
        all-rhs (map second all-srels)
        rhs-groups (group-by identity all-rhs)]
    (sequence
      (mapcat
        (fn [[g gs]]
          (when (< 1 (count gs))
            [g])))
      rhs-groups)))

(comment
(defn subo [t1 t2]
  (l/conde
    [(sub-rel t1 t2)]
    [(l/fresh
       [t3]
       (l/!= t3 t2)
       (sub-rel t1 t3)
       (subo t3 t2))]))
  (l/run* [q] (subo q `integer?))
  (l/run* [q] (subo q `number?))
  (l/run* [q] (subo `nat-int? `number?))
  (l/run* [q] (sub-rel `nat-int? `integer?))
  (l/run* [q] (subo `number? `any?))
  (l/run* [q] (subo `integer? q))
  )

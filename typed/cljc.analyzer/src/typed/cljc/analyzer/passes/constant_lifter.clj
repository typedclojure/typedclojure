;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.constant-lifter
(ns ^:typed.clojure typed.cljc.analyzer.passes.constant-lifter
  (:require [typed.cljc.analyzer :as common]
            [typed.cljc.analyzer.utils :refer [const-val]]))

(defmulti constant-lift
  "If the node represents a collection with no metadata, and every item of that
   collection is a literal, transform the node to an equivalent :const node."
  {:pass-info {:walk :post :depends #{}}}
  (fn [ast opts] (:op ast)))

(defmethod constant-lift :vector
  [{:keys [items form env] :as ast} opts]
  (if (and (every? :literal? items)
           (empty? (meta form)))
    (merge (dissoc ast :items :children)
           {:op       :const
            ::common/op ::common/const
            :val      (mapv const-val items)
            :type     :vector
            :literal? true})
    ast))

(defmethod constant-lift :map
  [{:keys [keys vals form env] :as ast} opts]
  (if (and (every? :literal? keys)
           (every? :literal? vals)
           (empty? (meta form)))
    (let [c (into (empty form)
                  (zipmap (mapv const-val keys)
                          (mapv const-val vals)))
          c (if (= (class c) (class form))
              c
              (apply array-map (mapcat identity c)))]
      (merge (dissoc ast :keys :vals :children)
             {:op       :const
              ::common/op ::common/const
              :val      c
              :type     :map
              :literal? true}))
    ast))

(defmethod constant-lift :set
  [{:keys [items form env] :as ast} opts]
  (if (and (every? :literal? items)
           (empty? (meta form)))
    (merge (dissoc ast :items :children)
           {:op       :const
            ::common/op ::common/const
            :val      (into (empty form) (mapv const-val items))
            :type     :set
            :literal? true})
    ast))

(defmethod constant-lift :default [ast opts] ast)

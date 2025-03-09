;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.jvm.classify-invoke
(ns ^:typed.clojure typed.clj.analyzer.passes.classify-invoke
  (:require [typed.cljc.analyzer :as common]
            [typed.cljc.analyzer.utils :as cu]
            [typed.clj.analyzer.utils :as ju]
            [typed.clj.analyzer.passes.validate :as validate]))

(create-ns 'typed.clj.analyzer)
(alias 'jvm 'typed.clj.analyzer)

;;important that this pass depends our `uniquify-locals`
;; (typed.cljc.analyzer.passes.uniquify), not the taj pass
(defn classify-invoke
  "If the AST node is an :invoke, check the node in function position,
   * if it is a keyword, transform the node in a :keyword-invoke node;
   * if it is the clojure.core/instance? var and the first argument is a
     literal class, transform the node in a :instance? node to be inlined by
     the emitter
   * if it is a protocol function var, transform the node in a :protocol-invoke
     node
   * if it is a regular function with primitive type hints that match a
     clojure.lang.IFn$[primitive interface], transform the node in a :prim-invoke
     node"
  {:pass-info {:walk :post :depends #{#'validate/validate}}}
  [{:keys [op] :as ast} opts]
  (if-not (= op :invoke)
    ast
    (let [{:keys [args tag env form] :as ast} ast
          argc (count args)
          the-fn (:fn ast)
          op (:op the-fn)
          var? (= :var op)
          the-var (:var the-fn)]

      (cond

       (and (= :const op)
            (= :keyword (:type the-fn)))
       (if (<= 1 argc 2)
         (if (and (not (namespace (:val the-fn)))
                  (= 1 argc))
           (merge (dissoc ast :fn :args)
                  {:op       :keyword-invoke
                   ::common/op ::common/keyword-invoke
                   :target   (first args)
                   :keyword  the-fn
                   :children [:keyword :target]})
           ast)
         (throw (ex-info (str "Cannot invoke keyword with " argc " arguments")
                         (merge {:form form}
                                (cu/source-info env)))))
       (and (= 2 argc)
            var?
            (= #'clojure.core/instance? the-var)
            (= :const (:op (first args)))
            (= :class (:type (first args))))
       (merge (dissoc ast :fn :args)
              {:op       :instance?
               ::common/op ::jvm/keyword-invoke
               :class    (:val (first args))
               :target   (second args)
               :form     form
               :env      env
               :o-tag    #?(:cljr Boolean :default Boolean/TYPE)
               :tag      (or tag #?(:cljr Boolean :default Boolean/TYPE))
               :children [:target]})

       (and var? (cu/protocol-node? the-var (:meta the-fn)))
       (if (>= argc 1)
         (merge (dissoc ast :fn)
                {:op          :protocol-invoke
                 ::common/op  ::common/protocol-invoke
                 :protocol-fn the-fn
                 :target      (first args)
                 :args        (vec (rest args))
                 :children    [:protocol-fn :target :args]})
         (throw (ex-info "Cannot invoke protocol method with no args"
                         (merge {:form form}
                                (cu/source-info env)))))

       :else
       (let [arglist (cu/arglist-for-arity the-fn argc)
             arg-tags (mapv (comp ju/specials str :tag meta) arglist)
             ret-tag (-> arglist meta :tag str ju/specials)
             tags (conj arg-tags ret-tag)]
         (if-let [prim-interface (ju/prim-interface (mapv #(if (nil? %) Object %) tags))]
           (merge ast
                  {:op             :prim-invoke
                   ::common/op     ::jvm/protocol-invoke
                   :prim-interface prim-interface
                   :args           (mapv (fn [arg tag] (assoc arg :tag tag)) args arg-tags)
                   :o-tag          ret-tag
                   :tag            (or tag ret-tag)})
           ast))))))

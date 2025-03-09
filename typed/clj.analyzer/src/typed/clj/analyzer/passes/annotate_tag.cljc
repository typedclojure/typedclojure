;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from clojure.tools.analyzer.passes.jvm.annotate-tag
(ns ^:typed.clojure typed.clj.analyzer.passes.annotate-tag
  (:require [typed.clj.analyzer.utils :as ju]
            [typed.clj.analyzer.passes.constant-lifter :as constant-lift :refer [constant-lift]])
  (:import (clojure.lang ISeq Var AFunction)))

(defmulti -annotate-tag :op)

(defmethod -annotate-tag :default [ast] ast)

(defn tag-via-val-or-form
  [{:keys [val form] :as ast}]
  (let [t (class (or val form))]
    (assoc ast :o-tag t :tag t)))

(defmethod -annotate-tag :map [ast] (tag-via-val-or-form ast))
(defmethod -annotate-tag :set [ast] (tag-via-val-or-form ast))
(defmethod -annotate-tag :vector [ast] (tag-via-val-or-form ast))

(defmethod -annotate-tag :the-var
  [ast]
  (assoc ast :o-tag Var :tag Var))

(defmethod -annotate-tag :const
  [ast]
  (case (:type ast)

    ;; char and numbers are unboxed by default
    :number
    (let [t (ju/unbox (class (:val ast)))]
      (assoc ast :o-tag t :tag t))

    :char
    (assoc ast :o-tag #?(:cljr Char :default Character/TYPE) :tag #?(:cljr Char :default Character/TYPE))

    :seq
    (assoc ast :o-tag ISeq :tag ISeq)

    (let [t (class (:val ast))]
      (assoc ast :o-tag t :tag t))))

(defmethod -annotate-tag :binding
  [{:keys [form tag o-tag init local name variadic?] :as ast}]
  (let [o-tag (or (:tag init) ;; should defer to infer-tag?
                  (and (= :fn local) AFunction)
                  (and (= :arg local) variadic? ISeq)
                  o-tag
                  Object)
        o-tag (if (#?(:cljr #{Void} :default #{Void Void/TYPE}) o-tag)
                Object
                o-tag)]
    (if-let [tag (or (:tag (meta form)) tag)]
      (-> ast
          (assoc :tag tag :o-tag tag)
          (cond->
            init (assoc-in [:init :tag] (ju/maybe-class tag))))
      (assoc ast :tag o-tag :o-tag o-tag))))

(defmethod -annotate-tag :local
  [{:keys [name form tag atom case-test] :as ast}]
  (let [o-tag (@atom :tag)]
    (assoc ast :o-tag o-tag :tag o-tag)))

;; TODO: move binding/local logic to infer-tag
(defn annotate-tag
  "If the AST node type is a constant object or contains :tag metadata,
   attach the appropriate :tag and :o-tag to the node."
  {:pass-info {:walk :post :depends #{} :after #{#'constant-lift/constant-lift}}}
  [{:keys [op atom tag o-tag] :as ast} opts]
  (let [ast (cond-> ast
              (and atom (:case-test @atom))
              (update :form vary-meta dissoc :tag))

        ast (cond-> ast
              (not (and o-tag tag))
              (-> -annotate-tag
                  (into (when-let [tag (or tag
                                           (-> ast :val meta :tag)
                                           (-> ast :form meta :tag))]
                          {:tag tag}))))]
    (when (= op :binding)
      (assert atom)
      (swap! atom assoc :tag (:tag ast)))
    ast))

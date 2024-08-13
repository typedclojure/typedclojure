;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from tools.analyzer.jvm
; - replace calls to `maybe-class-literal`
(ns typed.clj.analyzer.passes.analyze-host-expr
  (:require [typed.cljc.analyzer :as ana]
            [typed.cljc.analyzer :as common]
            [typed.cljc.analyzer.utils :refer [ctx source-info merge']]
            [typed.clj.analyzer.utils :as u]))

(create-ns 'typed.clj.analyzer)
(alias 'jvm 'typed.clj.analyzer)

(defn maybe-static-field [[_ class sym]]
  (when-let [{:keys [flags type name]} (u/static-field class sym)]
    {:op          :static-field
     ::common/op  ::jvm/static-field
     :assignable? (not (:final flags))
     :class       class
     :field       name
     :o-tag       type
     :tag         type}))

(defn maybe-static-method [[_ class sym]]
  (when-let [{:keys [name return-type]} (u/static-method class sym)]
    {:op      :static-call
     ::common/op  ::jvm/static-call
     :tag     return-type
     :o-tag   return-type
     :class   class
     :method  name}))

(defn maybe-instance-method [target-expr class sym]
  (when-let [{:keys [return-type]} (u/instance-method class sym)]
    {:op       :instance-call
     ::common/op  ::jvm/instance-call
     :tag      return-type
     :o-tag    return-type
     :instance target-expr
     :class    class
     :method   sym
     :children [:instance]}))

(defn maybe-instance-field [target-expr class sym]
  (when-let [{:keys [flags name type]} (u/instance-field class sym)]
    {:op          :instance-field
     ::common/op  ::jvm/instance-field
     :assignable? (not (:final flags))
     :class       class
     :instance    target-expr
     :field       name
     :tag         type
     :o-tag       type
     :children    [:instance]}))

(defn analyze-host-call
  [target-type method args target-expr class env]
  (let [[op common-op] (case target-type
                         :static   [:static-call ::jvm/static-call]
                         :instance [:instance-call ::jvm/instance-call])]
    (into
     {:op     op
      ::common/op common-op
      :method method
      :args   args}
     (case target-type
       :static   {:class    class
                  :children [:args]}
       :instance {:instance target-expr
                  :class    (u/maybe-class (:tag target-expr))
                  :children [:instance :args]}))))

(defn analyze-host-field
  [target-type field target-expr class env]
  (if class
    (case target-type
      :static (or (maybe-static-field (list '. class field))
                  (throw (ex-info (str "Cannot find field "
                                       field " for class " class)
                                  (merge {:class class
                                          :field field}
                                         (source-info env)))))
      :instance (or (maybe-instance-field target-expr class field)
                    {:op          :host-interop
                     ::common/op  ::jvm/host-interop
                     :target      (dissoc target-expr :tag :validated?)
                     :m-or-f      field
                     :assignable? true
                     :children    [:target]}
                    (when (:literal? target-expr)
                      (throw (ex-info (str "Cannot find field "
                                           field " for class " class)
                                      (merge {:instance (dissoc target-expr :env)
                                              :field    field}
                                             (source-info env)))))))
    {:op          :host-interop
     ::common/op  ::jvm/host-interop
     :target      target-expr
     :m-or-f      field
     :assignable? true
     :children    [:target]}))

(defn -analyze-host-expr
  [target-type m-or-f target-expr class env]
  (let [target-class (-> target-expr :tag)
        [field method] (if class
                         [(maybe-static-field (list '. class m-or-f))
                          (maybe-static-method (list '. class m-or-f))]
                         (when target-class
                           [(maybe-instance-field target-expr target-class m-or-f)
                            (maybe-instance-method target-expr target-class m-or-f)]))]
    (cond

     (not (or class target-class))
     {:op          :host-interop
      ::common/op  ::jvm/host-interop
      :target      target-expr
      :m-or-f      m-or-f
      :assignable? true
      :children    [:target]}

     method
     method

     field
     field

     class
     (throw (ex-info (str "Cannot find field or no-arg method call "
                          m-or-f " for class " class)
                     (merge {:class  class
                             :m-or-f m-or-f}
                            (source-info env))))

     target-class
     {:op          :host-interop
      ::common/op  ::jvm/host-interop
      :target      (dissoc target-expr :tag :validated?)
      :m-or-f      m-or-f
      :assignable? true
      :children    [:target]}

     :else
     (when (:literal? target-expr)
       (throw (ex-info (str "Cannot find field or no-arg method call "
                            m-or-f " for class " target-class)
                       (merge {:instance (dissoc target-expr :env)
                               :m-or-f   m-or-f}
                              (source-info env))))))))

(defn analyze-host-expr
  "Performing some reflection, transforms :host-interop/:host-call/:host-field
   nodes in either: :static-field, :static-call, :instance-call, :instance-field
   or :host-interop nodes, and a :var or :maybe-class node in a :const :class node,
   if necessary (class literals shadow Vars).

   A :host-interop node represents either an instance-field or a no-arg instance-method. "
  {:pass-info {:walk :post :depends #{}}}
  [ast opts]
  (let [op (:op ast)]
    (case op
      (:host-interop :host-call :host-field)
      (let [target (:target ast)
            tag (:tag ast)
            env (:env ast)
            target-op (:op target)
            target (if-let [the-class (and (= :local target-op)
                                           (u/maybe-class-literal (:form target)))]
                     (merge target
                            (assoc (ana/analyze-const the-class env :class opts)
                              :tag   #?(:cljr Type :default Class)
                              :o-tag #?(:cljr Type :default Class)))
                     target)
            class? (and (= :const target-op)
                        (= :class (:type target))
                        (:form target))
            target-type (if class? :static :instance)]
        (merge' (dissoc ast :assignable? :target :args :children)
                (case op

                  :host-call
                  (analyze-host-call target-type (:method ast)
                                     (:args ast) target class? env)

                  :host-field
                  (analyze-host-field target-type (:field ast)
                                      target (or class? (:tag target)) env)

                  :host-interop
                  (-analyze-host-expr target-type (:m-or-f ast)
                                      target class? env))
                (when tag
                  {:tag tag})))
      :var
      (let [form (:form ast)
            env (:env ast)]
        (if-let [the-class (and (not (namespace form))
                                (pos? (#?(:cljr .IndexOf :default .indexOf) (str form) "."))
                                (u/maybe-class-literal form))]
          (assoc (ana/analyze-const the-class env :class opts) :form form)
          ast))

      :maybe-class
      (let [form (:form ast)
            env (:env ast)
            class (:class ast)]
       (if-let [the-class (u/maybe-class-literal class)]
         (assoc (ana/analyze-const the-class env :class opts) :form form)
         ast))

      ast)))

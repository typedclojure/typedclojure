;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from tools.analyzer.jvm
; - changed :pass-info for `validate`
; - use ana2/resolve-{sym,ns} instead of u/resolve-{sym,ns}
; - use typed.clj.analyzer.passes.infer-tag
; - use typed.clj.analyzer.passes.analyze-host-expr
; - remove clojure.tools.analyzer.passes.jvm.validate-recur
(ns typed.clj.analyzer.passes.validate
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as env]
            [typed.cljc.analyzer.passes.cleanup :as cleanup]
            [typed.cljc.analyzer.utils :as cu]
            [typed.clj.analyzer.passes.analyze-host-expr :as analyze-host-expr]
            [typed.clj.analyzer.passes.infer-tag :as infer-tag]
            [typed.clj.analyzer.utils :as ju])
  (:import (clojure.lang IFn)))

(defmulti -validate (fn [ast opts] (:op ast)))

(defmethod -validate :maybe-class
  [{:keys [class env] :as ast} opts]
  (if-let [handle (-> (env/deref-env opts) :passes-opts :validate/unresolvable-symbol-handler)]
    (handle nil class ast)
    (if (not (#?(:cljr .Contains :default .contains) (str class) "."))
      (throw (ex-info (str "Could not resolve var: " class)
                      (into {:var class}
                            (cu/source-info env))))

      (throw (ex-info (str "Class not found: " class)
                      (into {:class class}
                            (cu/source-info env)))))))

(defmethod -validate :maybe-host-form
  [{:keys [class field form env] :as ast} {::ana2/keys [resolve-ns] :as opts}]
  (if-let [handle (-> (env/deref-env opts) :passes-opts :validate/unresolvable-symbol-handler)]
    (handle class field ast)
    (if (resolve-ns class env opts)
      (throw (ex-info (str "No such var: " class)
                      (into {:form form}
                            (cu/source-info env))))
      (throw (ex-info (str "No such namespace: " class)
                      (into {:ns   class
                             :form form}
                            (cu/source-info env)))))))

(defmethod -validate :set!
  [{:keys [target form env] :as ast} opts]
  (when (not (:assignable? target))
    (throw (ex-info "Cannot set! non-assignable target"
                    (into {:target (ast/prewalk target cleanup/cleanup)
                           :form   form}
                          (cu/source-info env)))))
  ast)

(defmethod -validate :new
  [{:keys [args] :as ast} opts]
  (if (:validated? ast)
    ast
    (if-not (= :class (-> ast :class :type))
      (throw (ex-info (str "Unable to resolve classname: " (:form (:class ast)))
                      (into {:class (:form (:class ast))
                             :ast   ast}
                            (cu/source-info (:env ast)))))
      (let [^#?(:cljr Type :default Class) class (-> ast :class :val)
            c-name #?(:cljr '.ctor :default (symbol (.getName class)))  ;;                                                                  ;; in .NET, ctors are named .ctor, not with the class name
            argc (count args)
            tags (mapv :tag args)]
        (let [[ctor & rest] (->> (eduction (filter #(= (count (:parameter-types %)) argc))
                                           (ju/members class c-name))
                                 (ju/try-best-match tags))]
          (if ctor
            (if (empty? rest)
              (let [arg-tags (mapv ju/maybe-class (:parameter-types ctor))
                    args (mapv (fn [arg tag] (assoc arg :tag tag)) args arg-tags)]
                (assoc ast
                  :args       args
                  :validated? true))
              ast)
            (throw (ex-info (str "no ctor found for ctor of class: " class " and given signature")
                            (into {:class class
                                   :args  (mapv (fn [a] (ast/prewalk a cleanup/cleanup)) args)}
                                  (cu/source-info (:env ast)))))))))))

(defn validate-call [{:keys [class instance method args tag env op] :as ast}]
  (let [argc (count args)
        instance? (= :instance-call op)
        f (if instance? ju/instance-methods ju/static-methods)
        tags (mapv :tag args)]
    (if-let [matching-methods (not-empty (into [] (f class method argc)))]
      (let [[m & rest :as matching] (ju/try-best-match tags matching-methods)]
        (if m
          (let [all-ret-equals? (apply = (map :return-type matching))]
            (if (or (empty? rest)
                    (and all-ret-equals? ;; if the method signature is the same just pick the first one
                         (apply = (map #(mapv ju/maybe-class (:parameter-types %)) matching))))
             (let [ret-tag  (:return-type m)
                   arg-tags (mapv ju/maybe-class (:parameter-types m))
                   args (mapv (fn [arg tag] (assoc arg :tag tag)) args arg-tags)
                   class (ju/maybe-class (:declaring-class m))]
               (cu/merge' ast
                          {:method     (:name m)
                           :validated? true
                           :class      class
                           :o-tag      ret-tag
                           :tag        (or tag ret-tag)
                           :args       args}
                          (if instance?
                            {:instance (assoc instance :tag class)})))
             (if all-ret-equals?
               (let [ret-tag (:return-type m)]
                 (assoc ast
                   :o-tag   Object
                   :tag     (or tag ret-tag)))
               ast)))
          (if instance?
            (assoc (dissoc ast :class) :tag Object :o-tag Object)
            (throw (ex-info (str "No matching method: " method " for class: " class " and given signature")
                            (into {:method method
                                   :class  class
                                   :args   (mapv (fn [a] (ast/prewalk a cleanup/cleanup)) args)}
                                  (cu/source-info env)))))))
      (if instance?
        (assoc (dissoc ast :class) :tag Object :o-tag Object)
        (throw (ex-info (str "No matching method: " method " for class: " class " and arity: " argc)
                        (into {:method method
                               :class  class
                               :argc   argc}
                              (cu/source-info env))))))))

(defmethod -validate :static-call
  [ast opts]
  (if (:validated? ast)
    ast
    (validate-call (assoc ast :class (ju/maybe-class (:class ast))))))

(defmethod -validate :static-field
  [ast opts]
  (if (:validated? ast)
    ast
    (assoc ast :class (ju/maybe-class (:class ast)))))

(defmethod -validate :instance-call
  [{:keys [class validated? instance] :as ast} opts]
  (let [class (or class (:tag instance))]
    (if (and class (not validated?))
      (validate-call (assoc ast :class (ju/maybe-class class)))
      ast)))

(defmethod -validate :instance-field
  [{:keys [instance class] :as ast} opts]
  (let [class (ju/maybe-class class)]
    (assoc ast :class class :instance (assoc instance :tag class))))

(defmethod -validate :import
  [{:keys [^String class validated? env form] :as ast} opts]
  (if-not validated?
    (let [class-sym (-> class (subs (inc #?(:cljr (.LastIndexOf class ".") :default (.lastIndexOf class ".")))) symbol)
          sym-val (ana2/resolve-sym class-sym env opts)]
      (if (and (class? sym-val) (not= #?(:cljr (.FullName ^Type sym-val) 
	                                     :default (.getName ^Class sym-val)) class)) ;; allow deftype redef
        (throw (ex-info (str class-sym " already refers to: " sym-val
                             " in namespace: " (:ns env))
                        (into {:class     class
                               :class-sym class-sym
                               :sym-val   sym-val
                               :form      form}
                              (cu/source-info env))))
        (assoc ast :validated? true)))
    ast))

(defmethod -validate :def
  [ast opts]
  (when-not (var? (:var ast))
    (throw (ex-info (str "Cannot def " (:name ast) " as it refers to the class "
                         #?(:cljr  (.FullName ^Type (:var ast)) :default (.getName ^Class (:var ast))))
                    (into {:ast (ast/prewalk ast cleanup/cleanup)}
                          (cu/source-info (:env ast))))))
  (into
   ast
   (when-let [tag (-> ast :name meta :tag)]
     (when (and (symbol? tag) (or (ju/specials (str tag)) (ju/special-arrays (str tag))))
       ;; we cannot validate all tags since :tag might contain a function call that returns
       ;; a valid tag at runtime, however if tag is one of ju/specials or ju/special-arrays
       ;; we know that it's a wrong tag as it's going to be evaluated as a clojure.core function
       (if-let [handle (-> (env/deref-env opts) :passes-opts :validate/wrong-tag-handler)]
         (handle :name/tag ast)
         (throw (ex-info (str "Wrong tag: " (eval tag) " in def: " (:name ast))
                         (into {:ast (ast/prewalk ast cleanup/cleanup)}
                               (cu/source-info (:env ast))))))))))

(defmethod -validate :invoke
  [{:keys [args env fn form] :as ast} opts]
  (let [argc (count args)]
    (when (and (= :const (:op fn))
               (not (instance? IFn (:form fn))))
      (throw (ex-info (str (class (:form fn)) " is not a function, but it's used as such")
                      (into {:form form}
                            (cu/source-info env)))))
    (if (and (:arglists fn)
             (not (cu/arglist-for-arity fn argc)))
      (if (-> (env/deref-env opts) :passes-opts :validate/throw-on-arity-mismatch)
        (throw (ex-info (str "No matching arity found for function: " (:name fn))
                        {:arity (count args)
                         :fn    fn}))
        (assoc ast :maybe-arity-mismatch true))
      ast)))

(defn validate-interfaces [{:keys [env form interfaces]}]
  (when-not (every? #?(:cljr #(.IsInterface ^Type %) :default #(.isInterface ^Class %)) (disj interfaces Object))
    (throw (ex-info "only interfaces or Object can be implemented by deftype/reify"
                    (into {:interfaces interfaces
                           :form       form}
                          (cu/source-info env))))))

(defmethod -validate :deftype
  [{:keys [class-name] :as ast} opts]
  (validate-interfaces ast)
  (assoc ast :class-name (ju/maybe-class class-name)))

(defmethod -validate :reify
  [{:keys [class-name] :as ast} opts]
  (validate-interfaces ast)
  (assoc ast :class-name (ju/maybe-class class-name)))

(defmethod -validate :default [ast opts] ast)

(defn validate-tag' [t tag ast opts]
  (or (ju/maybe-class tag)
      (if-let [handle (-> (env/deref-env opts) :passes-opts :validate/wrong-tag-handler)]
        (handle t ast)
        (throw (ex-info (str "Class not found: " tag)
                        (into {:class    tag
                               :ast      (ast/prewalk ast cleanup/cleanup)}
                              (cu/source-info (:env ast))))))))

(defn validate-tag [t ast opts]
  (let [tag (get ast t)
        the-class (validate-tag' t tag ast opts)]
    {t the-class}))

;;important that this pass depends our `uniquify-locals`
;; (typed.cljc.analyzer.passes.uniquify), not the taj pass
;; - remove validate-recur
;; - replace infer-tag
;; - replace analyze-host-expr
(defn validate
  "Validate tags, classes, method calls.
   Throws exceptions when invalid forms are encountered, replaces
   class symbols with class objects.

   Passes opts:
   * :validate/throw-on-arity-mismatch
      If true, validate will throw on potential arity mismatch
   * :validate/wrong-tag-handler
      If bound to a function, will invoke that function instead of
      throwing on invalid tag.
      The function takes the tag key (or :name/tag if the node is :def and
      the wrong tag is the one on the :name field meta) and the originating
      AST node and must return a map (or nil) that will be merged into the AST,
      possibly shadowing the wrong tag with Object or nil.
   * :validate/unresolvable-symbol-handler
      If bound to a function, will invoke that function instead of
      throwing on unresolvable symbol.
      The function takes three arguments: the namespace (possibly nil)
      and name part of the symbol, as symbols and the originating
      AST node which can be either a :maybe-class or a :maybe-host-form,
      those nodes are documented in the tools.analyzer quickref.
      The function must return a valid tools.analyzer.jvm AST node."
  {:pass-info {:walk :post :depends #{;; replace
                                      #'infer-tag/infer-tag
                                      ;; replace
                                      #'analyze-host-expr/analyze-host-expr
                                      ;; validate-recur doesn't seem to play nicely with core.async/go
                                      #_#'validate-recur/validate-recur}}}
  [{:keys [tag] :as ast} opts]
  (let [{:keys [o-tag return-tag] :as ast} (-validate ast opts)
        tag (or tag (:tag ast))]
    (cond-> ast
      tag (assoc :tag (validate-tag' :tag tag ast opts))
      o-tag (assoc :o-tag (validate-tag' :o-tag o-tag ast opts))
      return-tag (assoc :return-tag (validate-tag' :return-tag return-tag ast opts)))))

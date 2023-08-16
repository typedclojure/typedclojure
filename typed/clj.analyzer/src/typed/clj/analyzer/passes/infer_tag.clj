;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from tools.analyzer.jvm
; - changed :pass-info for `infer-tag`
; - use analyzer.env
(ns typed.clj.analyzer.passes.infer-tag
  (:require [typed.cljc.analyzer.env :as env]
            [typed.cljc.analyzer.utils :as cu]
            [typed.clj.analyzer.passes.analyze-host-expr :as analyze-host-expr]
            [typed.clj.analyzer.passes.annotate-host-info :as annotate-host-info]
            [typed.clj.analyzer.passes.annotate-tag :as annotate-tag]
            [typed.clj.analyzer.passes.fix-case-test :as fix-case-test]
            [typed.clj.analyzer.utils :as ju]
            [clojure.set :as set]))

(defmulti -infer-tag :op)
(defmethod -infer-tag :default [ast] ast)

(defmethod -infer-tag :binding
  [{:keys [init atom] :as ast}]
  (if init
    (let [info (select-keys init [:return-tag :arglists])]
      (swap! atom merge info)
      (into ast info))
    ast))

(defmethod -infer-tag :local
  [ast]
  (let [atom-ast @(:atom ast)]
    (-> (merge atom-ast
               ast)
        (assoc :o-tag (:tag atom-ast)))))

(defmethod -infer-tag :var
  [{:keys [var form] :as ast}]
  (let [{:keys [tag arglists]} (:meta ast)
        arglists (cond-> arglists
                   (= 'quote (first arglists)) second)
        form-tag (:tag (meta form))]
    ;;if (not dynamic)
    (-> ast
        (assoc :o-tag Object)
        (into (when-let [tag (or form-tag tag)]
                (if (fn? @var)
                  {:tag clojure.lang.AFunction :return-tag tag}
                  {:tag tag})))
        (into (when arglists
                {:arglists arglists})))))

(defmethod -infer-tag :def
  [{:keys [var init name] :as ast}]
  (let [info (into (select-keys init [:return-tag :arglists :tag])
                   (select-keys (meta name) [:tag :arglists]))]
    (when (and (seq info)
               (not (:dynamic (meta name)))
               (= :global (-> (env/deref-env) :passes-opts :infer-tag/level)))
      (alter-meta! var merge (set/rename-keys info {:return-tag :tag})))
    (-> ast 
        (into info)
        (assoc :tag clojure.lang.Var :o-tag clojure.lang.Var))))

(defmethod -infer-tag :quote
  [ast]
  (let [tag (-> ast :expr :tag)]
    (assoc ast :tag tag :o-tag tag)))

(defmethod -infer-tag :new
  [ast]
  (let [t (-> ast :class :val)]
    (assoc ast :o-tag t :tag t)))

(defmethod -infer-tag :with-meta
  [{:keys [expr] :as ast}]
  (-> ast
      ;;trying to be smart here
      (assoc :tag (or (:tag expr) Object) :o-tag Object)
      (into (select-keys expr [:return-tag :arglists]))))

(defmethod -infer-tag :recur
  [ast]
  (assoc ast :ignore-tag true))

(defmethod -infer-tag :do
  [{:keys [ret] :as ast}]
  (-> ast
      (assoc :o-tag (:tag ret))
      (into (select-keys ret [:return-tag :arglists :ignore-tag :tag]))))

(defmethod -infer-tag :let
  [{:keys [body] :as ast}]
  (-> ast 
      (assoc :o-tag (:tag body))
      (into (select-keys body [:return-tag :arglists :ignore-tag :tag]))))

(defmethod -infer-tag :letfn
  [{:keys [body] :as ast}]
  (-> ast
      (assoc :o-tag (:tag body))
      (into (select-keys body [:return-tag :arglists :ignore-tag :tag]))))

(defmethod -infer-tag :loop
  [{:keys [body] :as ast}]
  (let [tag (:tag body)]
    (-> ast
        (assoc :o-tag tag
               :tag (if (#{Void Void/TYPE} tag)
                      Object
                      tag))
        (into (select-keys body [:return-tag :arglists])))))

(defn =-arglists? [a1 a2]
  (let [tag (fn [x] (-> x meta :tag ju/maybe-class))]
    (and (= a1 a2)
         (every? true? (map (fn [a1 a2]
                              (and (= (tag a1) (tag a2))
                                   (= (mapv tag a1)
                                      (mapv tag a2))))
                            a1 a2)))))

(defmethod -infer-tag :if
  [{:keys [then else] :as ast}]
  (let [then-tag (:tag then)
        else-tag (:tag else)
        ignore-then? (:ignore-tag then)
        ignore-else? (:ignore-tag else)]
    (cond
     (and then-tag
          (or ignore-else? (= then-tag else-tag)))
     (-> ast
         (assoc :tag then-tag :o-tag then-tag)
         (into (when-let [return-tag (:return-tag then)]
                 (when (or ignore-else?
                           (= return-tag (:return-tag else)))
                   {:return-tag return-tag})))
         (into (when-let [arglists (:arglists then)]
                 (when (or ignore-else?
                           (=-arglists? arglists (:arglists else)))
                   {:arglists arglists}))))

     (and else-tag ignore-then?)
     (-> ast
         (assoc :tag else-tag :o-tag else-tag)
         (into (when-let [return-tag (:return-tag else)]
                 {:return-tag return-tag}))
         (into (when-let [arglists (:arglists else)]
                 {:arglists arglists})))

     (and (:ignore-tag then) (:ignore-tag else))
     (assoc ast :ignore-tag true)

     :else
     ast)))

(defmethod -infer-tag :throw
  [ast]
  (assoc ast :ignore-tag true))

(defmethod -infer-tag :case
  [{:keys [thens default] :as ast}]
  (let [thens (conj (mapv :then thens) default)
        exprs (seq (remove :ignore-tag thens))
        tag (:tag (first exprs))]
    (cond
     (and tag
          (every? #(= (:tag %) tag) exprs))
     (-> ast
         (assoc :tag tag :o-tag tag)
         (into (when-let [return-tag (:return-tag (first exprs))]
                 (when (every? #(= (:return-tag %) return-tag) exprs)
                   {:return-tag return-tag})))
         (into (when-let [arglists (:arglists (first exprs))]
                 (when (every? #(=-arglists? (:arglists %) arglists) exprs)
                   {:arglists arglists}))))

     (every? :ignore-tag thens)
     (assoc ast :ignore-tag true)

     :else
     ast)))

(defmethod -infer-tag :try
  [{:keys [body catches] :as ast}]
  (let [{:keys [tag return-tag arglists]} body
        catches (sequence
                  (comp (map :body)
                        (remove :ignore-tag))
                  catches)]
    (cond-> ast
      (and tag (every? #(= (:tag %) tag) catches))
      (assoc :tag tag :o-tag tag)

      (and return-tag (every? #(= (:return-tag %) return-tag) catches))
      (assoc :return-tag return-tag)

      (and arglists (every? #(=-arglists? (:arglists %) arglists) catches))
      (assoc :arglists arglists))))

(defmethod -infer-tag :fn-method
  [{:keys [form body params local] :as ast}]
  (let [annotated-tag (or (:tag (meta (first form)))
                          (:tag (meta (:form local))))
        body-tag (:tag body)
        tag (or annotated-tag body-tag)
        tag (if (#{Void Void/TYPE} tag)
              Object
              tag)]
    (-> ast
        (assoc :arglist (with-meta (into []
                                         (mapcat (fn [{:keys [form variadic?]}]
                                                   (if variadic?
                                                     ['& form]
                                                     [form])))
                                         params)
                                   (when tag {:tag tag})))
        (cond->
          (not= tag body-tag)
          (assoc-in [:body :tag] (ju/maybe-class tag))

          tag
          (assoc :tag tag :o-tag tag)))))

(defmethod -infer-tag :fn
  [{:keys [local methods] :as ast}]
  (-> ast
      (assoc :arglists (seq (mapv :arglist methods))
             :tag      clojure.lang.AFunction
             :o-tag    clojure.lang.AFunction)
      (into (when-let [tag (or (:tag (meta (:form local)))
                               (and (apply = (map :tag methods))
                                    (:tag (first methods))))]
              {:return-tag tag}))))

(defmethod -infer-tag :invoke
  [{:keys [fn args] :as ast}]
  (if (:arglists fn)
    (let [argc (count args)
          arglist (cu/arglist-for-arity fn argc)
          tag (or (:tag (meta arglist))
                  (:return-tag fn)
                  (and (= :var (:op fn))
                       (:tag (:meta fn))))]
      (cond-> ast
        tag
        (assoc :tag tag :o-tag tag)))
    (if-let [tag (:return-tag fn)]
      (assoc ast :tag tag :o-tag tag)
      ast)))

(defmethod -infer-tag :method
  [{:keys [form body params] :as ast}]
  (let [tag (or (:tag (meta (first form)))
                (:tag (meta (second form))))
        body-tag (:tag body)]
    (assoc ast :tag (or tag body-tag) :o-tag body-tag)))

(defmethod -infer-tag :reify
  [{:keys [class-name] :as ast}]
  (assoc ast :tag class-name :o-tag class-name))

(defmethod -infer-tag :set!
  [ast]
  (let [t (:tag (:target ast))]
    (assoc ast :tag t :o-tag t)))

;;redefine passes mainly to move dependency on `uniquify-locals`
;; to `uniquify2/uniquify-locals`
(defn infer-tag
  "Performs local tag inference on the AST--adds, when possible,
   one or more of the following keys to the AST:
   * :o-tag      represents the current type of the
                 expression represented by the node
   * :tag        represents the type the expression represented by the
                 node is required to have, possibly the same as :o-tag
   * :return-tag implies that the node will return a function whose
                 invocation will result in a object of this type
   * :arglists   implies that the node will return a function with
                 this arglists
   * :ignore-tag true when the node is untyped, does not imply that
                 all untyped node will have this

  Passes opts:
  * :infer-tag/level  If :global, infer-tag will perform Var tag
                      inference"
  {:pass-info {:walk :post :depends #{#'annotate-tag/annotate-tag 
                                      #'annotate-host-info/annotate-host-info 
                                      #'fix-case-test/fix-case-test 
                                      #'analyze-host-expr/analyze-host-expr} 
               ; trim is incompatible with core.typed
               #_#_:after #{#'trim}}}
  [{:keys [tag form] :as ast}]
  (let [tag (or tag (:tag (meta form)))]
    (-> ast
        -infer-tag 
        (cond->
          tag (assoc :tag tag)))))


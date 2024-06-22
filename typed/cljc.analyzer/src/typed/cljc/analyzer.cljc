;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from clojure.tools.analyzer
(ns typed.cljc.analyzer
  (:refer-clojure :exclude [macroexpand-1 var?])
  (:require #?@(:clj []
                :default [[typed.clojure :as-alias t]])
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.utils :as u])
  #?(:clj (:import (clojure.lang IType))))

;; clojure 1.9 compat
#?(:clj (do (create-ns 'typed.clojure)
            (alias 't 'typed.clojure)))

(set! *warn-on-reflection* true)

(def ^{:dynamic  true
       :arglists '([form env opts])
       :doc      "If form represents a macro form, returns its expansion,
                  else returns form."}
  macroexpand-1)

(def ^{:dynamic  true
       :arglists '([sym env])
       :doc      "Creates a var for sym and returns it"}
  create-var)

(def ^{:dynamic  true
       :arglists '([obj])
       :doc      "Returns true if obj represent a var form as returned by create-var"}
  var?)

(def ^{:dynamic  true
       :doc      "A map of functions such that

                 (ast/walk ast (:pre scheduled-passes) (:post scheduled-passes))

                 runs the passes currently scheduled, and
                 
                 ((:init-ast scheduled-passes) ast)
                 
                 initializes the AST for traversal."}
  scheduled-passes)

(def ^{:dynamic  true
       :doc      "Resolves the value mapped by the given sym in the global env"
       :arglists '([sym env])}
  resolve-sym)

(def ^{:dynamic  true
       :doc      "Evaluates an AST node, attaching result to :result."
       :arglists '([a opts])}
  eval-ast)

(def ^{:dynamic  true
       :doc      "If given a var, returns the fully qualified symbol for that var, otherwise nil."
       :arglists '([v])}
  var->sym)

(def ^{:dynamic  true
       :doc      "If ast is :unanalyzed, then call analyze-form on it, otherwise returns ast."
       :arglists '([ast opts])}
  analyze-outer)

(def ^{:dynamic  true
       :doc      "Create an AST node for a form without expanding it."
       :arglists '([form env opts])}
  unanalyzed)

(declare analyze-outer-root)

(defn run-pre-passes
  [ast opts]
  ((:pre scheduled-passes) ast opts))

(defn run-post-passes
  [ast opts]
  ((:post scheduled-passes) ast opts))

(declare eval-top-level)

(defn run-passes
  "Function that will be invoked on the AST tree immediately after it has been constructed."
  [ast opts]
  (ast/walk ast
            #(-> % (analyze-outer-root opts) (run-pre-passes opts))
            #(-> % (run-post-passes opts) (eval-top-level opts))))

(def specials
  '#{do if new quote set! try var
     catch throw finally def .
     let* letfn* loop* recur fn*})

(declare analyze-symbol
         analyze-vector
         analyze-map
         analyze-set
         analyze-seq
         analyze-const)

(defn analyze-form
  "Like analyze, but does not mark the form with :top-level true"
  [form env opts]
  (cond
    (symbol? form) (analyze-symbol form env opts)
    #?@(:clj [(instance? IType form) (analyze-const form env :type opts)])
    (record? form) (analyze-const form env :record opts)
    (seq? form) (if-let [form (seq form)]
                  (analyze-seq form env opts)
                  (analyze-const form env nil opts))
    (map? form) (analyze-map form env opts)
    (vector? form) (analyze-vector form env opts)
    (set? form) (analyze-set form env opts)
    :else (analyze-const form env nil opts)))

(defn analyze
  "Given a top-level form to analyze and an environment, a map containing:
   * :locals     a map from binding symbol to AST of the binding value
   * :context    a keyword describing the form's context from the :ctx/* hierarchy.
    ** :ctx/expr      the form is an expression: its value is used
    ** :ctx/return    the form is an expression in return position, derives :ctx/expr
    ** :ctx/statement the value of the form is not used
   * :ns         a symbol representing the current namespace of the form to be
                 analyzed

   returns one level of the AST for that form, with all children
   stubbed out with :unanalyzed nodes."
  [form env opts]
  (assoc (analyze-form form env opts) :top-level true))

(def defexpr-info {})

(defmacro defexpr [name fields & methods]
  (let [nsym (ns-name *ns*)
        qname (symbol (str nsym "." name))
        info {qname {:fields fields :ns nsym :name name}}]
    (alter-var-root #'defexpr-info merge info)
    `(do
       (alter-var-root #'defexpr-info merge '~info)
       (defrecord ~name ~fields ~@methods))))

(defmacro create-expr [m cls]
  {:pre [(symbol? cls)
         (map? m)]}
  (let [^#?(:cljr Type :default Class) rcls (resolve cls)
        _ (assert (class? rcls) {:cls cls :resolved rcls})
        rsym (symbol (#?(:cljr .FullName :default .getName) rcls))
        {:keys [fields] :as info} (get defexpr-info rsym)
        _ (assert info (str "No info for expr " cls))
        fset (into #{} (map keyword) fields)
        real-fields (select-keys m fset)
        extmap (not-empty (apply dissoc m fset))]
    `(new ~rsym
          ~@(map (comp real-fields keyword) fields)
          ~@(when extmap
              [nil extmap]))))

(defmacro update-expr [e cls & cases]
  {:pre [(symbol? cls)
         (every? vector? cases)]}
  (let [^#?(:cljr Type :default Class) rcls (resolve cls)
        _ (assert (class? rcls) {:cls cls :resolved rcls})
        rsym (symbol (#?(:cljr .FullName :default .getName) rcls))
        {:keys [fields] :as info} (get defexpr-info rsym)
        _ (assert info (str "No info for expr " cls))
        ks (map first cases)
        fset (into #{} (map keyword) fields)
        _ (assert (every? simple-keyword? ks))
        _ (assert (apply distinct? ks))
        kw->case (zipmap (map first cases)
                         (map rest cases))
        g (with-meta (gensym 'e)
                     {:tag rsym})
        lookup-extmap (list '.-__extmap g)
        lookup-kw (fn [k]
                    {:pre [(simple-keyword? k)]}
                    (if (fset k)
                      ;; normal field
                      (list (symbol (str ".-" (name k))) g)
                      ;; in extmap
                      (list `get lookup-extmap k)))
        lcases (into {}
                     (map (fn [k]
                            (let [c (kw->case k)
                                  _ (assert (seq c))]
                              [k [;; local
                                  (gensym (symbol (name k)))
                                  ;; rhs
                                  (list* (first c)
                                         (lookup-kw k)
                                         (rest c))]])))
                     ks)]
    `(let [~g ~e
           ~@(mapcat lcases ks)]
       (new ~rsym
            ~@(sequence (comp
                          (map keyword)
                          (map (fn [kw]
                                 (if-let [[_ [lhs]] (find lcases kw)]
                                   lhs
                                   (lookup-kw kw)))))
                        fields)
            ~(list '.-__meta g)
            ~(let [extcases (apply dissoc lcases fset)]
               (if (seq extcases)
                 `(assoc ~lookup-extmap ~@(mapcat (fn [[k [local]]]
                                                    [k local])
                                                  extcases))
                 lookup-extmap))))))

(defn ^:private f->fs [f] #(mapv f %))
(defn ^:private f->maybe-f [f] #(some-> % f))

(defn mark-top-level
  [ast]
  ^::t/ignore
  ^{::t/unsafe-cast Expr}
  ; in ::config because an :unanalyzed node is still top-level
  ; once analyzed
  (assoc-in ast [::config :top-level] true))

(defn unmark-top-level
  [ast]
  ^::t/ignore
  ^{::t/unsafe-cast Expr}
  (update ast ::config dissoc :top-level))

(defn top-level?
  [ast]
  (boolean (get-in ast [::config :top-level])))

(defn mark-eval-top-level
  [ast]
  (assoc ast ::eval-gilardi? true))

(defn unmark-eval-top-level
  [ast]
  (dissoc ast ::eval-gilardi?))

(defn eval-top-level?
  [ast]
  (boolean (get ast ::eval-gilardi?)))

(defn unanalyzed-top-level
  [form env opts]
  (mark-top-level (unanalyzed form env opts)))

(defn inherit-top-level
  "Return new-expr with equivalent top-level status
  as old-expr."
  [new-expr old-expr]
  (cond-> new-expr
    (top-level? old-expr) mark-top-level
    (eval-top-level? old-expr) mark-eval-top-level))

(defn propagate-top-level
  "Propagate :top-level down :do nodes. Attach ::ana2/eval-gilardi? to
  root nodes that should be evaluated."
  [{:keys [op] :as ast}]
  (if (and (not= :unanalyzed op)
           (get-in ast [::config :top-level]))
    ; we know this root node is fully analyzed, so we can reliably predict
    ; whether to evaluate it under the Gilardi scenario.
    (case (:op ast)
      :do (ast/update-children ast mark-top-level)
      (mark-eval-top-level ast))
    ast))

(defn propagate-result
  "Propagate :result from :top-level :do nodes."
  [ast]
  {:pre [(:op ast)]}
  (cond-> ast
    (and (= :do (:op ast))
         (get-in ast [::config :top-level]))
    ^::t/ignore
    ^{::t/unsafe-cast Expr}
    (into (select-keys (:ret ast) [:result]))))

(defn eval-top-level
  "Evaluate `eval-top-level?` nodes and unanalyzed `top-level?` nodes.
  Otherwise, propagate result from children."
  [ast opts]
  {:pre [(:op ast)]}
  (if (or (eval-top-level? ast)
          (and (top-level? ast)
               (= :unanalyzed (:op ast))))
    (eval-ast ast opts)
    (propagate-result ast)))

(defn analyze-outer-root
  "Repeatedly call analyze-outer to a fixed point."
  [ast opts]
  (let [ast' (analyze-outer ast opts)]
    (if (identical? ast ast')
      ast'
      (recur ast' opts))))

(defn unanalyzed-in-env
  "Takes an env map and returns a function that analyzes a form in that env"
  [env opts]
  (fn [form] (unanalyzed form env opts)))

(declare ^:private update-withmetaexpr-children)

(defexpr WithMetaExpr [op form env meta expr top-level children]
  ast/IASTWalk
  (ast/children-of* [_] [meta expr])
  (ast/update-children* [this f]
    (update-withmetaexpr-children this f)))

(defn ^:private update-withmetaexpr-children [this f]
  (-> this
      (update-expr WithMetaExpr [:meta f] [:expr f])))

;; this node wraps non-quoted collections literals with metadata attached
;; to them, the metadata will be evaluated at run-time, not treated like a constant
(defn wrapping-meta
  [{:keys [form env] :as expr} opts]
  (let [meta (meta form)]
    (if (and (u/obj? form)
             (seq meta))
      (->
        {:op       :with-meta
         ::op      ::with-meta
         :env      env
         :form     form
         :meta     (unanalyzed meta (u/ctx env :ctx/expr) opts)
         :expr     (assoc-in expr [:env :context] :ctx/expr)
         :children [:meta :expr]}
        (create-expr WithMetaExpr))
      expr)))

(declare ^:private update-constexpr-children)

(defexpr ConstExpr [op form env type literal? val meta top-level children]
  ast/IASTWalk
  (ast/children-of* [_] (cond-> []
                          meta (conj meta)))
  (ast/update-children* [this f] (update-constexpr-children this f)))

(defn ^:private update-constexpr-children [this f]
  (-> this
      (update-expr ConstExpr
                   [:meta (f->maybe-f f)])))

(defn analyze-const
  [form env type opts]
  (let [type (or type (u/classify form))
        m (when (u/obj? form)
            (not-empty (meta form)))]
    (->
      {:op       :const
       ::op      ::const
       :env      env
       :type     type
       :literal? true
       :val      form
       :form     form
       :meta    (some-> m (analyze-const (u/ctx env :ctx/expr) :map opts)) ;; metadata on a constant literal will not be evaluated at
       :children (when m [:meta])}                                         ;; runtime, this is also true for metadata on quoted collection literals
      (create-expr ConstExpr))))

(declare ^:private update-vectorexpr-children)

(defexpr VectorExpr [op env items form children top-level]
  ast/IASTWalk
  (ast/children-of* [_] items)
  (ast/update-children* [this f] (update-vectorexpr-children this f)))

(defn ^:private update-vectorexpr-children [this f]
  (-> this
      (update-expr VectorExpr
                   [:items (f->fs f)])))

(defn analyze-vector
  [form env opts]
  (let [items-env (u/ctx env :ctx/expr)
        items (mapv (unanalyzed-in-env items-env opts) form)]
    (->
     {:op       :vector
      ::op      ::vector
      :env      env
      :items    items
      :form     form
      :children [:items]}
     (create-expr VectorExpr)
     (wrapping-meta opts))))

(declare ^:private update-mapexpr-children)

(defexpr MapExpr [op env keys vals form children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (into keys vals))
  (ast/update-children* [this f] (update-mapexpr-children this f)))

(defn ^:private update-mapexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr MapExpr [:keys fs] [:vals fs]))))

(defn analyze-map
  [form env opts]
  (let [kv-env (u/ctx env :ctx/expr)
        [keys vals] (reduce-kv (fn [[keys vals] k v]
                                 [(conj keys k) (conj vals v)])
                               [[] []] form)
        ks (mapv (unanalyzed-in-env kv-env opts) keys)
        vs (mapv (unanalyzed-in-env kv-env opts) vals)]
    (->
     {:op       :map
      ::op      ::map
      :env      env
      :keys     ks
      :vals     vs
      :form     form
      :children [:keys :vals]}
     (create-expr MapExpr)
     (wrapping-meta opts))))

(defexpr SetExpr [op env items form children top-level]
  ast/IASTWalk
  (ast/children-of* [_] items)
  (ast/update-children* [this f]
    (-> this
        (u/update-record-children :items f))))

(defn analyze-set
  [form env opts]
  (let [items-env (u/ctx env :ctx/expr)
        items (mapv (unanalyzed-in-env items-env opts) form)]
    (->
     {:op       :set
      ::op      ::set
      :env      env
      :items    items
      :form     form
      :children [:items]}
     (create-expr SetExpr)
     (wrapping-meta opts))))

(defrecord LocalExpr [op env form assignable? children top-level tag o-tag atom case-test]
  ast/IASTWalk
  (ast/children-of* [_]
    (assert (empty? children) children)
    [])
  (ast/update-children* [this f]
    (assert (empty? children) children)
    this))

(defexpr VarExpr [op env form assignable? var meta children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [])
  (ast/update-children* [this f] this))

(defexpr MaybeHostFormExpr [op env form class field children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [])
  (ast/update-children* [this f] this))

(defexpr MaybeClassExpr [op env form class children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [])
  (ast/update-children* [this f] this))

(defn analyze-symbol
  [sym env opts]
  (let [mform (macroexpand-1 sym env opts)] ;; t.a.j/macroexpand-1 macroexpands Class/Field into (. Class Field)
    (if (= mform sym)
      (into
        (if-let [{:keys [mutable children] :as local-binding} (-> env :locals sym)] ;; locals shadow globals
          (->
            (into local-binding
                  {:op          :local
                   ::op         ::local
                   :assignable? (boolean mutable)
                   ;; don't walk :init, but keep in AST
                   :children    (into [] (remove #(= :init %)) children)})
            map->LocalExpr)
          (if-let [var (let [v (resolve-sym sym env)]
                         (and (var? v) v))]
            (let [m (meta var)]
              (->
                {:op          :var
                 ::op         ::var
                 :assignable? (u/dynamic? var m) ;; we cannot statically determine if a Var is in a thread-local context
                 :var         var              ;; so checking whether it's dynamic or not is the most we can do
                 :meta        m}
                (create-expr VarExpr)))
            (if-let [maybe-class (namespace sym)] ;; e.g. js/foo.bar or Long/MAX_VALUE
              (let [maybe-class (symbol maybe-class)]
                (->
                  {:op    :maybe-host-form
                   ::op   ::maybe-host-form
                   :class maybe-class
                   :field (symbol (name sym))}
                  (create-expr MaybeHostFormExpr)))
              (->
                {:op    :maybe-class ;; e.g. java.lang.Integer or Long
                 ::op   ::maybe-class
                 :class mform}
                (create-expr MaybeClassExpr)))))
        ;; TODO inline this
        {:env  env
         :form mform})
      (-> (unanalyzed mform env opts)
        (update :raw-forms (fnil conj ()) sym)))))

(defn analyze-seq
  [form env {::keys [parse] :as opts}]
  ;(prn "analyze-seq" form)
  (let [op (first form)]
    (when (nil? op)
      (throw (ex-info "Can't call nil"
                      (merge {:form form}
                             (u/-source-info form env)))))
    (let [mform (macroexpand-1 form env opts)]
      (if (= form mform) ;; function/special-form invocation
        (parse mform env opts)
        (-> (unanalyzed mform env opts)
            (update :raw-forms (fnil conj ())
                    (vary-meta form assoc ::resolved-op (resolve-sym op env))))))))

(declare ^:private update-doexpr-children)

(defexpr DoExpr [op env form statements ret children top-level body?]
  ast/IASTWalk
  (ast/children-of* [_]
    (assert (vector? statements))
    (conj statements ret))
  (ast/update-children* [this f] (update-doexpr-children this f)))

(defn ^:private update-doexpr-children [this f]
  (let [fs #(mapv f %)]
    (-> this
        (update-expr DoExpr
                     [:statements fs]
                     [:ret f]))))

(defn parse-do
  [[_ & exprs :as form] env opts]
  (let [statements-env (u/ctx env :ctx/statement)
        [statements ret] (loop [statements [] [e & exprs] exprs]
                           (if (seq exprs)
                             (recur (conj statements e) exprs)
                             [statements e]))
        statements (mapv (unanalyzed-in-env statements-env opts) statements)
        ret (unanalyzed ret env opts)]
    (->
      {:op         :do
       ::op        ::do
       :env        env
       :form       form
       :statements statements
       :ret        ret
       :children   [:statements :ret]}
      (create-expr DoExpr))))

(declare ^:private update-ifexpr-children)

(defexpr IfExpr [op env form test then else children top-level tag o-tag]
  ast/IASTWalk
  (ast/children-of* [_] [test then else])
  (ast/update-children* [this f] (update-ifexpr-children this f)))

(defn ^:private update-ifexpr-children [this f]
  (-> this
      (update-expr IfExpr
                   [:test f]
                   [:then f]
                   [:else f])))

(defn parse-if
  [[_ test then else :as form] env opts]
  (let [formc (count form)]
    (when-not (or (= formc 3) (= formc 4))
      (throw (ex-info (str "Wrong number of args to if, had: " (dec (count form)))
                      (merge {:form form}
                             (u/-source-info form env))))))
  (let [test-expr (unanalyzed test (u/ctx env :ctx/expr) opts)
        then-expr (unanalyzed then env opts)
        else-expr (unanalyzed else env opts)]
    (->
      {:op       :if
       ::op      ::if
       :form     form
       :env      env
       :test     test-expr
       :then     then-expr
       :else     else-expr
       :children [:test :then :else]}
      (create-expr IfExpr))))

(declare ^:private update-newexpr-children)

(defexpr NewExpr [op env form class args children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (vec (cons class args)))
  (ast/update-children* [this f] (update-newexpr-children this f)))

(defn ^:private update-newexpr-children [this f]
  (let [fs #(mapv f %)]
    (-> this
        (update-expr NewExpr
                     [:class f]
                     [:args fs]))))

(defn parse-new
  [[_ class & args :as form] env opts]
  (when-not (>= (count form) 2)
    (throw (ex-info (str "Wrong number of args to new, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [args-env (u/ctx env :ctx/expr)
        args (mapv (unanalyzed-in-env args-env opts) args)]
    (->
      {:op          :new
       ::op         ::new
       :env         env
       :form        form
       :class       (analyze-form class (assoc env :locals {}) opts) ;; avoid shadowing
       :args        args
       :children    [:class :args]}
      (create-expr NewExpr))))

(declare ^:private update-quoteexpr-children)

(defexpr QuoteExpr [op env form expr literal? children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [expr])
  (ast/update-children* [this f] (update-quoteexpr-children this f)))

(defn ^:private update-quoteexpr-children [this f]
  (-> this
      (update-expr QuoteExpr
                   [:expr f])))

(defn parse-quote
  [[_ expr :as form] env opts]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to quote, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [const (analyze-const expr env nil opts)]
    (->
      {:op       :quote
       ::op      ::quote
       :expr     const
       :form     form
       :env      env
       :literal? true
       :children [:expr]}
      (create-expr QuoteExpr))))

(declare ^:private update-set!expr-children)

(defexpr Set!Expr [op env form target val children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [target val])
  (ast/update-children* [this f] (update-set!expr-children this f)))

(defn ^:private update-set!expr-children [this f]
  (-> this
      (update-expr Set!Expr
                   [:target f]
                   [:val f])))

(defn parse-set!
  [[_ target val :as form] env opts]
  (when-not (= 3 (count form))
    (throw (ex-info (str "Wrong number of args to set!, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [target (unanalyzed target (u/ctx env :ctx/expr) opts)
        val (unanalyzed val (u/ctx env :ctx/expr) opts)]
    (->
      {:op       :set!
       ::op      ::set!
       :env      env
       :form     form
       :target   target
       :val      val
       :children [:target :val]}
      (create-expr Set!Expr))))

(defn analyze-body [body env {::keys [parse] :as opts}]
  ;; :body is used by emit-form to remove the artificial 'do
  (assoc (parse (cons 'do body) env opts) :body? true))

(defn valid-binding-symbol? [s]
  (and (symbol? s)
       (not (namespace s))
       (not (re-find #"\." (name s)))))

(defn ^:private split-with' [pred coll]
  (loop [take [] drop coll]
    (if (seq drop)
      (let [[el & r] drop]
        (if (pred el)
          (recur (conj take el) r)
          [(seq take) drop]))
      [(seq take) ()])))

(declare ^:private update-tryexpr-children)

(defexpr TryExpr [op env form body catches finally children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (vec (concat [body] catches
                                     (when finally
                                       [finally]))))
  (ast/update-children* [this f] (update-tryexpr-children this f)))

(defn ^:private update-tryexpr-children [this f]
  (let [fs #(mapv f %)]
    (-> this
        (update-expr TryExpr
                     [:body f]
                     [:catches fs]
                     [:finally #(some-> % f)]))))

(declare parse-catch)
(defn parse-try
  [[_ & body :as form] env opts]
  (let [catch? (every-pred seq? #(= (first %) 'catch))
        finally? (every-pred seq? #(= (first %) 'finally))
        [body tail'] (split-with' (complement (some-fn catch? finally?)) body)
        [cblocks tail] (split-with' catch? tail')
        [[fblock & fbs :as fblocks] tail] (split-with' finally? tail)]
    (when-not (empty? tail)
      (throw (ex-info "Only catch or finally clause can follow catch in try expression"
                      (merge {:expr tail
                              :form form}
                             (u/-source-info form env)))))
    (when-not (empty? fbs)
      (throw (ex-info "Only one finally clause allowed in try expression"
                      (merge {:expr fblocks
                              :form form}
                             (u/-source-info form env)))))
    (let [env' (assoc env :in-try true)
          body (analyze-body body env' opts)
          cenv (u/ctx env' :ctx/expr)
          cblocks (mapv #(parse-catch % cenv opts) cblocks)
          fblock (when-not (empty? fblock)
                   (analyze-body (rest fblock) (u/ctx env :ctx/statement) opts))]
      (->
        {:op      :try
         ::op     ::try
         :env     env
         :form    form
         :body    body
         :catches cblocks
         :finally fblock
         :children (into [:body :catches]
                         (when fblock [:finally]))}
        (create-expr TryExpr)))))

(declare ^:private update-catchexpr-children)

(defexpr CatchExpr [op env form class local body children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [class local body])
  (ast/update-children* [this f] (update-catchexpr-children this f)))

(defn ^:private update-catchexpr-children [this f]
  (-> this
      (update-expr CatchExpr
                   [:class f]
                   [:local f]
                   [:body f])))

;; NOTE: the clojure.tools.analyzer.passes.jvm.annotate-tag pass (in tools.analyzer, not typed.clj.analyzer)
;; assumes that :atom is a non-nilable field. Since records 
(defexpr BindingExpr [op env form name init local arg-id variadic? children atom]
  ast/IASTWalk
  (ast/children-of* [_] (if (some #(= % :init) children)
                          [init]
                          []))
  (ast/update-children* [this f]
    (cond-> this
      (some #(= % :init) children)
      (u/update-record-child :init f))))

(defn parse-catch
  [[_ etype ename & body :as form] env opts]
  (when-not (valid-binding-symbol? ename)
    (throw (ex-info (str "Bad binding form: " ename)
                    (merge {:sym ename
                            :form form}
                           (u/-source-info form env)))))
  (let [env (dissoc env :in-try)
        local (-> {:op    :binding
                   ::op   ::binding
                   :env   env
                   :form  ename
                   :name  ename
                   :local :catch}
                  (create-expr BindingExpr))]
    (->
      {:op          :catch
       ::op         ::catch
       :class       (unanalyzed etype (assoc env :locals {}) opts)
       :local       local
       :env         env
       :form        form
       :body        (analyze-body body (assoc-in env [:locals ename] (u/dissoc-env local)) opts)
       :children    [:class :local :body]}
      (create-expr CatchExpr))))

(defexpr ThrowExpr [op env form exception children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [exception])
  (ast/update-children* [this f]
    (-> this
        (u/update-record-child :exception f))))

(defn parse-throw
  [[_ throw :as form] env opts]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to throw, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (->
    {:op        :throw
     ::op       ::throw
     :env       env
     :form      form
     :exception (unanalyzed throw (u/ctx env :ctx/expr) opts)
     :children  [:exception]}
    (create-expr ThrowExpr)))

(defn validate-bindings
  [[op bindings & _ :as form] env]
  (when-let [error-msg
             (cond
              (not (vector? bindings))
              (str op " requires a vector for its bindings, had: "
                   (#?(:cljs type :default class) bindings))

              (not (even? (count bindings)))
              (str op " requires an even number of forms in binding vector, had: "
                   (count bindings)))]
    (throw (ex-info error-msg
                    (merge {:form     form
                            :bindings bindings}
                           (u/-source-info form env))))))

(declare ^:private update-letfnexpr-children)

(defexpr LetFnExpr [op env form bindings body children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (conj bindings body))
  (ast/update-children* [this f] (update-letfnexpr-children this f)))

(defn ^:private update-letfnexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr LetFnExpr
                     [:bindings fs]
                     [:body f]))))

(defn parse-letfn*
  [[_ bindings & body :as form] env opts]
  (validate-bindings form env)
  (let [bindings (apply array-map bindings) ;; pick only one local with the same name, if more are present.
        fns      (keys bindings)]
    (when-some [[sym] (seq (remove valid-binding-symbol? fns))]
      (throw (ex-info (str "Bad binding form: " sym)
                      (into {:form form
                             :sym  sym}
                            (u/-source-info form env)))))
    (let [binds (reduce (fn [binds name]
                          (assoc binds name
                                 {:op    :binding
                                  ::op   ::binding
                                  :env   env
                                  :name  name
                                  :form  name
                                  :local :letfn}))
                        {} fns)
          e (update env :locals merge binds) ;; pre-seed locals
          binds (reduce-kv (fn [binds name bind]
                             (assoc binds name
                                    (assoc bind
                                           :init (unanalyzed (bindings name)
                                                             (u/ctx e :ctx/expr)
                                                             opts)
                                           :children [:init])))
                           {} binds)
          e (update env :locals merge (u/update-vals binds u/dissoc-env))
          body (analyze-body body e opts)]
      (->
        {:op       :letfn
         ::op      ::letfn
         :env      env
         :form     form
         :bindings (vec (vals binds)) ;; order is irrelevant
         :body     body
         :children [:bindings :body]}
        (create-expr LetFnExpr)))))

(defn analyze-let
  [[op bindings & body :as form] {:keys [context loop-id] :as env} opts]
  (validate-bindings form env)
  (let [loop? (= 'loop* op)]
    (loop [bindings bindings
           env (u/ctx env :ctx/expr)
           binds []]
      (if-some [[name init & bindings] (seq bindings)]
        (if (not (valid-binding-symbol? name))
          (throw (ex-info (str "Bad binding form: " name)
                          (into {:form form
                                 :sym  name}
                                (u/-source-info form env))))
          (let [init-expr (unanalyzed init env opts)
                bind-expr (->
                            {:op       :binding
                             ::op      ::binding
                             :env      env
                             :name     name
                             :init     init-expr
                             :form     name
                             :local    (if loop? :loop :let)
                             :children [:init]}
                            (create-expr BindingExpr))]
            (recur bindings
                   (assoc-in env [:locals name] (u/dissoc-env bind-expr))
                   (conj binds bind-expr))))
        (let [body-env (assoc env :context (if loop? :ctx/return context))
              body (analyze-body body (cond-> body-env
                                        loop? (assoc :loop-id loop-id
                                                     :loop-locals (count binds)))
                                 opts)]
          {:body     body
           :bindings binds
           :children [:bindings :body]})))))

(declare ^:private update-letexpr-children)

(defexpr LetExpr [op env form bindings body children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (conj bindings body))
  (ast/update-children* [this f] (update-letexpr-children this f)))

(defn ^:private update-letexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr LetExpr
                     [:bindings fs]
                     [:body f]))))

(defn parse-let*
  [form env opts]
  (let [{:keys [body bindings children]} (analyze-let form env opts)]
    (-> {:op   :let
         ::op  ::let
         :form form
         :env  env
         :body body
         :bindings bindings
         :children children}
        (create-expr LetExpr))))

(declare ^:private update-loopexpr-children)

(defexpr LoopExpr [op env form bindings body loop-id children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (conj bindings body))
  (ast/update-children* [this f] (update-loopexpr-children this f)))

(defn ^:private update-loopexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr LoopExpr
                     [:bindings fs]
                     [:body f]))))

(defn parse-loop*
  [form env opts]
  (let [loop-id (gensym "loop_") ;; can be used to find matching recur
        env (assoc env :loop-id loop-id)
        {:keys [body bindings children]} (analyze-let form env opts)]
    (-> {:op      :loop
         ::op     ::loop
         :form    form
         :env     env
         :loop-id loop-id
         :body body
         :bindings bindings
         :children children}
        (create-expr LoopExpr))))

(defexpr RecurExpr [op env form exprs loop-id children top-level]
  ast/IASTWalk
  (ast/children-of* [_] exprs)
  (ast/update-children* [this f]
    (-> this
        (u/update-record-children :exprs f))))

(defn parse-recur
  [[_ & exprs :as form] {:keys [context loop-locals loop-id]
                         :as env} opts]
  (when-let [error-msg
             (cond
              (not (isa? context :ctx/return))
              "Can only recur from tail position"

              (not (= (count exprs) loop-locals))
              (str "Mismatched argument count to recur, expected: " loop-locals
                   " args, had: " (count exprs)))]
    (throw (ex-info error-msg
                    (merge {:exprs exprs
                            :form  form}
                           (u/-source-info form env)))))

  (let [exprs (mapv (unanalyzed-in-env (u/ctx env :ctx/expr) opts) exprs)]
    (->
      {:op          :recur
       ::op         ::recur
       :env         env
       :form        form
       :exprs       exprs
       :loop-id     loop-id
       :children    [:exprs]}
      (create-expr RecurExpr))))

(declare ^:private update-fnmethodexpr-children)

(defexpr FnMethodExpr [op env form loop-id variadic? params fixed-arity body local children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (conj params body))
  (ast/update-children* [this f] (update-fnmethodexpr-children this f)))

(defn ^:private update-fnmethodexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr FnMethodExpr
                     [:params fs]
                     [:body f]))))

(defn analyze-fn-method [[params & body :as form] {:keys [locals local] :as env} opts]
  (when-not (vector? params)
    (throw (ex-info "Parameter declaration should be a vector"
                    (merge {:params params
                            :form   form}
                           (u/-source-info form env)
                           (u/-source-info params env)))))
  (when (not-every? valid-binding-symbol? params)
    (throw (ex-info (str "Params must be valid binding symbols, had: "
                         (mapv #?(:cljs type :default class) params))
                    (merge {:params params
                            :form   form}
                           (u/-source-info form env)
                           (u/-source-info params env))))) ;; more specific
  (let [variadic? (boolean (some '#{&} params))
        params-names (cond-> params
                       variadic? (-> pop pop (conj (peek params))))
        env (dissoc env :local)
        arity (count params-names)
        params-expr (mapv (fn [name id]
                            (->
                              {:env       env
                               :form      name
                               :name      name
                               :variadic? (and variadic?
                                               (= id (dec arity)))
                               :op        :binding
                               ::op       ::binding
                               :arg-id    id
                               :local     :arg}
                              (create-expr BindingExpr)))
                          params-names (range))
        fixed-arity (cond-> arity 
                      variadic? dec)
        loop-id (gensym "loop_")
        body-env (-> env
                     (update :locals merge (zipmap params-names (map u/dissoc-env params-expr)))
                     (assoc :context     :ctx/return
                            :loop-id     loop-id
                            :loop-locals (count params-expr)))
        body (analyze-body body body-env opts)]
    (when variadic?
      (let [x (drop-while #(not= % '&) params)]
        (when (contains? #{nil '&} (second x))
          (throw (ex-info "Invalid parameter list"
                          (-> {:params params
                               :form form}
                              (into (u/-source-info form env))
                              (into (u/-source-info params env))))))
        (when (not= 2 (count x))
          (throw (ex-info (str "Unexpected parameter: " (first (drop 2 x))
                               " after variadic parameter: " (second x))
                          (-> {:params params
                               :form form}
                              (into (u/-source-info form env))
                              (into (u/-source-info params env))))))))
      (->
        {:op          :fn-method
         ::op         ::fn-method
         :form        form
         :loop-id     loop-id
         :env         env
         :variadic?   variadic?
         :params      params-expr
         :fixed-arity fixed-arity
         :body        body
         :children    [:params :body]
         :local (some-> local u/dissoc-env)}
        (create-expr FnMethodExpr))))

(declare ^:private update-fnexpr-children)

(defexpr FnExpr [op env form variadic? max-fixed-arity methods once local children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (into (if local [local] [])
                              methods))
  (ast/update-children* [this f] (update-fnexpr-children this f)))

(defn ^:private update-fnexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr FnExpr
                     [:local #(some-> % f)]
                     [:methods fs]))))

(defn parse-fn*
  [[op & args :as form] env opts]
  (wrapping-meta
   (let [[n meths] (if (symbol? (first args))
                     [(first args) (next args)]
                     [nil (seq args)])
         name-expr (->
                     {:op    :binding
                      ::op   ::binding
                      :env   env
                      :form  n
                      :local :fn
                      :name  n}
                     (create-expr BindingExpr))
         e (cond-> env
             n (-> (assoc-in [:locals n] (u/dissoc-env name-expr))
                   (assoc :local name-expr)))
         once? (-> op meta :once boolean)
         menv (-> e 
                  (dissoc :in-try)
                  (assoc :once once?))
         ;;turn (fn [] ...) into (fn ([]...))
         meths (cond-> meths
                 (vector? (first meths)) list)
         methods-exprs (mapv #(analyze-fn-method % menv opts) meths)
         variadic (seq (filter :variadic? methods-exprs))
         variadic? (boolean variadic)
         fixed-arities (seq (sequence
                              (comp (remove :variadic?)
                                    (map :fixed-arity))
                              methods-exprs))
         max-fixed-arity (when fixed-arities (apply max fixed-arities))]
     (when (>= (count variadic) 2)
       (throw (ex-info "Can't have more than 1 variadic overload"
                       (into {:variadics (mapv :form variadic)
                              :form      form}
                             (u/-source-info form env)))))
     (when (not= (seq (distinct fixed-arities)) fixed-arities)
       (throw (ex-info "Can't have 2 or more overloads with the same arity"
                       (into {:form form}
                             (u/-source-info form env)))))
     (when (and variadic?
                (not-every? #(<= (:fixed-arity %)
                                 (:fixed-arity (first variadic)))
                            (remove :variadic? methods-exprs)))
       (throw (ex-info "Can't have fixed arity overload with more params than variadic overload"
                       (into {:form form}
                             (u/-source-info form env)))))
     (->
       {:op              :fn
        ::op             ::fn
        :env             env
        :form            form
        :variadic?       variadic?
        :max-fixed-arity max-fixed-arity
        :methods         methods-exprs
        :once            once?
        :children (conj (if n [:local] []) :methods)
        :local (when n name-expr)}
       (create-expr FnExpr)))
   opts))

(declare ^:private update-defexpr-children)

(defexpr DefExpr [op env form name var meta init doc children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (filterv identity [meta init]))
  (ast/update-children* [this f] (update-defexpr-children this f)))

(defn ^:private update-defexpr-children [this f]
  (let [maybe-f (f->maybe-f f)]
    (-> this
        (update-expr DefExpr
                     [:meta maybe-f]
                     [:init maybe-f]))))

(defn parse-def
  [[_ sym & expr :as form] {:keys [ns] :as env} opts]
  (when (not (symbol? sym))
    (throw (ex-info (str "First argument to def must be a symbol, had: " (#?(:cljs type :default class) sym))
                    (into {:form form}
                          (u/-source-info form env)))))
  (when (and (namespace sym)
             (not= *ns* (find-ns (symbol (namespace sym)))))
    (throw (ex-info "Cannot def namespace qualified symbol"
                    (into {:form form
                           :sym sym}
                          (u/-source-info form env)))))
  (let [pfn (fn
              ([])
              ([init]
                 {:init init})
              ([doc init]
                 {:pre [(string? doc)]}
                 {:init init :doc doc}))
        args (apply pfn expr)

        doc (or (:doc args) (-> sym meta :doc))
        arglists (when-let [arglists (:arglists (meta sym))]
                   (second arglists)) ;; drop quote

        sym (with-meta
              (symbol (name sym))
              (merge (meta sym)
                     (when arglists
                       {:arglists arglists})
                     (when doc
                       {:doc doc})
                     (u/-source-info form env)))

        var (create-var sym env) ;; interned var will have quoted arglists, replaced on evaluation

        meta (merge (meta sym)
                    (when arglists
                      {:arglists (list 'quote arglists)}))

        meta-expr (when meta (unanalyzed meta (u/ctx env :ctx/expr) opts)) ;; meta on def sym will be evaluated

        args (when-let [[_ init] (find args :init)]
               (assoc args :init (unanalyzed init (u/ctx env :ctx/expr) opts)))
        init? (:init args)
        children (cond-> [] 
                   meta (conj :meta)
                   init? (conj :init))]
    (->
      {:op   :def
       ::op  ::def
       :env  env
       :form form
       :name sym
       :var  var
       :meta meta-expr
       :children (not-empty children)
       :doc (:doc args)
       :init (:init args)}
      (create-expr DefExpr))))

(declare ^:private update-hostcallexpr-children)

(defexpr HostCallExpr [op env form method target args children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (into [target] args))
  (ast/update-children* [this f] (update-hostcallexpr-children this f)))

(defn ^:private update-hostcallexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr HostCallExpr
                     [:target f]
                     [:args fs]))))

(declare ^:private update-hostfieldexpr-children)

(defexpr HostFieldExpr [op env form field target assignable? children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [target])
  (ast/update-children* [this f] (update-hostfieldexpr-children this f)))

(defn ^:private update-hostfieldexpr-children [this f]
  (-> this
      (update-expr HostFieldExpr
                   [:target f])))

(declare ^:private update-hostinteropexpr-children)

(defexpr HostInteropExpr [op env form m-or-f target assignable? children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [target])
  (ast/update-children* [this f] (update-hostinteropexpr-children this f)))

(defn ^:private update-hostinteropexpr-children [this f]
  (-> this
      (update-expr HostInteropExpr
                   [:target f])))

(defn parse-dot
  [[_ target & [m-or-f & args] :as form] env opts]
  (when-not (>= (count form) 3)
    (throw (ex-info (str "Wrong number of args to ., had: " (dec (count form)))
                    (into {:form form}
                          (u/-source-info form env)))))
  (let [[m-or-f field?] (if (and (symbol? m-or-f)
                                 (= \- (first (name m-or-f))))
                          [(-> m-or-f name (subs 1) symbol) true]
                          [(if args (cons m-or-f args) m-or-f) false])
        target-expr (unanalyzed target (u/ctx env :ctx/expr) opts)
        call? (and (not field?) (seq? m-or-f))]

    (when (and call? (not (symbol? (first m-or-f))))
      (throw (ex-info (str "Method name must be a symbol, had: " (#?(:cljs type :default class) (first m-or-f)))
                      (into {:form   form
                             :method m-or-f}
                            (u/-source-info form env)))))
    (cond
      call?
      (->
        {:op       :host-call
         ::op      ::host-call
         :method   (symbol (name (first m-or-f)))
         :args     (mapv (unanalyzed-in-env (u/ctx env :ctx/expr) opts) (next m-or-f))
         :children [:target :args]
         ;; common fields
         :form   form
         :env    env
         :target target-expr}
        (create-expr HostCallExpr))

      field?
      (->
        {:op          :host-field
         ::op         ::host-field
         :assignable? true
         :field       (symbol (name m-or-f))
         :children    [:target]
         ;; common fields
         :form   form
         :env    env
         :target target-expr}
        (create-expr HostFieldExpr))

      :else
      (->
        {:op          :host-interop ;; either field access or no-args method call
         ::op         ::host-interop
         :assignable? true
         :m-or-f      (symbol (name m-or-f))
         :children    [:target]
         ;; common fields
         :form   form
         :env    env
         :target target-expr}
        (create-expr HostInteropExpr)))))

(declare ^:private update-invokeexpr-children)

(defexpr InvokeExpr [op env form fn args meta children top-level]
  ast/IASTWalk
  (ast/children-of* [_] (into [fn] args))
  (ast/update-children* [this f] (update-invokeexpr-children this f)))

(defn ^:private update-invokeexpr-children [this f]
  (let [fs (f->fs f)]
    (-> this
        (update-expr InvokeExpr
                     [:fn f]
                     [:args fs]))))

(defn parse-invoke
  [[f & args :as form] env opts]
  (let [fenv (u/ctx env :ctx/expr)
        fn-expr (unanalyzed f fenv opts)
        args-expr (mapv (unanalyzed-in-env fenv opts) args)
        m (meta form)]
    (->
      {:op   :invoke
       ::op  ::invoke
       :form form
       :env  env
       :fn   fn-expr
       :args args-expr
       :meta m ;; meta on invoke form will not be evaluated
       :children [:fn :args]}
      (create-expr InvokeExpr))))

(defexpr TheVarExpr [op env form var children top-level]
  ast/IASTWalk
  (ast/children-of* [_] [])
  (ast/update-children* [this f]
    this))

(defn parse-var
  [[_ var :as form] env opts]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to var, had: " (dec (count form)))
                    (into {:form form}
                          (u/-source-info form env)))))
  (if-let [var (resolve-sym var env)]
    (->
      {:op   :the-var
       ::op  ::the-var
       :env  env
       :form form
       :var  var}
      (create-expr TheVarExpr))
    (throw (ex-info (str "var not found: " var) {:var var}))))

(defn -parse
  "Takes a form and an env map and dispatches on the head of the form, that is
   a special form."
  [form env opts]
  ((case (first form)
     do      parse-do
     if      parse-if
     new     parse-new
     quote   parse-quote
     set!    parse-set!
     try     parse-try
     throw   parse-throw
     def     parse-def
     .       parse-dot
     let*    parse-let*
     letfn*  parse-letfn*
     loop*   parse-loop*
     recur   parse-recur
     fn*     parse-fn*
     var     parse-var
     #_:else parse-invoke)
   form env opts))

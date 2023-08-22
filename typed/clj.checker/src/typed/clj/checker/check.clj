;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.check
  (:require [typed.clojure :as t]
            [typed.cljc.checker.filter-ops :as fo]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.rules :as rules]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as cljset]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [typed.clj.analyzer :as jana2]
            [typed.clj.analyzer.passes.beta-reduce :as beta-reduce]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.clj.analyzer.utils :as jtau]
            [typed.clj.checker.analyze-clj :as ana-clj]
            [typed.clj.checker.array-ops :as arr-ops]
            [typed.clj.checker.assoc-utils :as assoc-u]
            [typed.clj.checker.check.deftype :as deftype]
            [typed.clj.checker.check.field :as field]
            [typed.clj.checker.check.host-interop :as host-interop]
            [typed.clj.checker.check.method :as method]
            [typed.clj.checker.check.reify :as reify]
            [typed.clj.checker.check.type-hints :as type-hints]
            [typed.clj.checker.constant-type :as constant-type]
            [typed.clj.checker.ctor-override-env :as ctor-override]
            [typed.clj.checker.mm-env :as mm]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.rclass-env :as rcls]
            [typed.clj.checker.reflect-utils :as reflect-u]
            [typed.clj.checker.subtype :as sub]
            [typed.clj.checker.tc-equiv :as equiv]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.env :as env]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.apply :as apply]
            [typed.cljc.checker.check.binding :as binding]
            [typed.cljc.checker.check.case :as case]
            [typed.cljc.checker.check.catch :as catch]
            [typed.cljc.checker.check.cli :as cli]
            [typed.cljc.checker.check.const :as const]
            [typed.cljc.checker.check.def :as def]
            [typed.cljc.checker.check.do :as do]
            [typed.cljc.checker.check.fn :as fn]
            [typed.cljc.checker.check.fn-method-utils :as fn-method-u]
            [typed.cljc.checker.check.fn-methods :as fn-methods]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.get :as get]
            [typed.cljc.checker.check.if :as if]
            [typed.cljc.checker.check.invoke :as invoke]
            [typed.cljc.checker.check.invoke-kw :as invoke-kw]
            [typed.cljc.checker.check.isa :as isa]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check.letfn :as letfn]
            [typed.cljc.checker.check.local :as local]
            [typed.cljc.checker.check.loop :as loop]
            [typed.cljc.checker.check.map :as map]
            [typed.cljc.checker.check.meta-ann :as meta-ann]
            [typed.cljc.checker.check.monitor :as monitor]
            [typed.cljc.checker.check.multi :as multi]
            [typed.cljc.checker.check.multi-utils :as multi-u]
            [typed.cljc.checker.check.nth :as nth]
            [typed.cljc.checker.check.nthnext :as nthnext]
            [typed.cljc.checker.check.print-env :as print-env]
            [typed.cljc.checker.check.quote :as quote]
            [typed.cljc.checker.check.recur :as recur]
            [typed.cljc.checker.check.set :as set]
            [typed.cljc.checker.check.set-bang :as set!]
            [typed.cljc.checker.check.special.cast :as cast]
            [typed.cljc.checker.check.special.fn :as special-fn]
            [typed.cljc.checker.check.special.loop :as special-loop]
            [typed.cljc.checker.check.throw :as throw]
            [typed.cljc.checker.check.try :as try]
            [typed.cljc.checker.check.unanalyzed :as unanalyzed]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.check.vector :as vec]
            [typed.cljc.checker.check.with-meta :as with-meta]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.datatype-ancestor-env :as ancest]
            [typed.cljc.checker.datatype-env :as dt-env]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.fold-rep :as fold]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.inst :as inst]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.ns-deps-utils :as ns-depsu]
            [typed.cljc.checker.ns-options :as ns-opts]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.open-result :as open-result]
            [typed.cljc.checker.path-rep :as pe]
            [typed.cljc.checker.protocol-env :as ptl-env]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.subst-obj :as subst-obj]
            [typed.cljc.checker.tvar-bnds :as tvar-bnds]
            [typed.cljc.checker.tvar-env :as tvar-env]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.update :as update]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.var-env :as var-env])
  (:import (clojure.lang IPersistentMap Seqable Var)))

(t/ann ^:no-check typed.clj.checker.parse-unparse/*unparse-type-in-ns* (t/U nil t/Sym))
(t/ann ^:no-check clojure.core.typed.util-vars/*already-checked* (t/U nil (t/Atom1 (t/Set t/Sym))))

;==========================================================
; # Type Checker
;
; The type checker is implemented here.

(declare check-top-level)

(defn check-ns1
  "Type checks an entire namespace."
  ([ns] (check-ns1 ns (jana2/empty-env)))
  ([ns env]
   (env/ensure (jana2/global-env)
     (let [^java.net.URL res (jtau/ns-url ns)]
       (assert res (str "Can't find " ns " in classpath"))
       (let [filename (str res)
             path     (.getPath res)]
         (binding [*ns*   *ns*
                   *file* filename]
           (with-open [rdr (io/reader res)]
             (let [pbr (readers/indexing-push-back-reader
                         (java.io.PushbackReader. rdr) 1 filename)
                   eof (Object.)
                   read-opts (cond-> {:eof eof :features #{:clj}}
                               (.endsWith filename "cljc") (assoc :read-cond :allow))]
               (loop []
                 (let [form (reader/read read-opts pbr)]
                   (when-not (identical? form eof)
                     (check-top-level form nil {:env (assoc env :ns (ns-name *ns*))})
                     (recur))))))))))))

(defn check-ns-and-deps [nsym] (cu/check-ns-and-deps nsym check-ns1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(defmulti -check
  "Type checks the given expression at an optional expected TCResult.
  Assumes expression has been passed to ana2/run-pre-passes.
  Dispatches on the operator of expr."
  (fn [expr expected]
    {:pre [((some-fn nil? r/TCResult?) expected)]}
    (:op expr)))

(declare check-expr)

(def ^:private *register-exts (delay (configs/register-clj-config-exts)))

(defn maybe-check-inlineable [{:keys [op form env] :as expr} expected]
  {:pre [(#{:unanalyzed} op)]}
  (when (seq? form)
    (let [v (-> (first form)
                (ana2/resolve-sym env))]
      (when (var? v)
        (let [m (meta v)]
          (when (and (:inline m)
                     (let [inline-arities-f (:inline-arities m)]
                       (or (not inline-arities-f)
                           (inline-arities-f (count (rest form))))))
            ;; TODO unit test (lack of) double expand/eval
            (let [expr-noinline (binding [ana2/macroexpand-1 (fn [form _] form)]
                                  ;; could pull out the ana2/unmark-top-level to here.
                                  ;; probably would avoid the need for ana2/unmark-eval-top-level.
                                  (ana2/analyze-outer expr))]
              (when (= :invoke (:op expr-noinline))
                (let [{cargs :args
                       res u/expr-type} (-> expr-noinline
                                            ;; defer eval to inlining -- would rather an bad type check than a botched eval
                                            ana2/unmark-top-level
                                            ana2/unmark-eval-top-level
                                            (check-expr expected))]
                  (-> expr
                      (assoc :form (with-meta (cons (first form)
                                                    ;; technically we just need the :tag of these
                                                    ;; args, could infer from checked expr-noinline.
                                                    (map emit-form/emit-form cargs))
                                              (meta form)))
                      ana2/analyze-outer-root
                      ana2/run-passes
                      (assoc u/expr-type res)))))))))))

(defn check-expr
  "Type checks expr at optional expected type. expr must not have a u/expr-type entry.
  
  The return expr will be fully expanded, analyzed, evaluated (via ana2/eval-top-level),
  with a u/expr-type entry giving the TCResult of the whole expression.

  As an exception, an :unanalyzed node may be returned from this function. It will have a :tag,
  and :result (if top-level). If not top-level, the node will be expanded and evaluated as part
  of its enclosing top-level expression."
  ([expr] (check-expr expr nil))
  ([expr expected]
  {:pre [(map? expr)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? (u/expr-type %))]}
  ;(prn "check-expr" op)
  ;(clojure.pprint/pprint (emit-form/emit-form expr))
  ;; to keep :post condition
  (loop [expr expr]
    (assert (not (u/expr-type expr))
            (str "Expression already has type information when passed to check-expr"))
    (let [; update namespace, as it might have changed when evaluating a previous
          ; subexpression during type checking
          {:keys [env] :as expr} (assoc-in expr [:env :ns] (ns-name *ns*))]
      (when vs/*trace-checker*
        (println (str "Checking line " (:line env) ":" (:column env) ":" (:file env)))
        (println (str "> " (binding [*print-level* (or *print-level* 10)
                                     *print-length* (or *print-length* 10)]
                             (pr-str (:form expr))))))
      (if (= :unanalyzed (:op expr))
        ;; Type checks the :unanalyzed expr at expected type.
        ;; The return expr will be fully expanded, analyzed, evaluated (if top-level),
        ;; with a u/expr-type entry for the TCResult of the entire expression."
        (let [;register typing rules (ie., implementations of -unanalyzed-top-level
              ; and -unanalyzed-special)
              _ @*register-exts]
          (or (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
                        vs/*current-expr* expr]
                (or (meta-ann/maybe-check-meta-ann expr expected)
                    (unanalyzed/-unanalyzed-special expr expected)
                    (maybe-check-inlineable expr expected)))
              (-> expr
                  ana2/analyze-outer
                  recur)))
        (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
                  vs/*current-expr* expr]
          (-> expr
              ana2/run-pre-passes
              (-check expected)
              ana2/run-post-passes
              ana2/eval-top-level)))))))

(defn check-top-level
  "Type check a top-level form at an expected type, returning a
  fully analyzed core.typed.analyzer AST node (ie., containing no :unanalyzed nodes)
  with a u/expr-type entry giving its TCResult type, and a :result entry
  holding its evaluation result."
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env] :as opts}]
   ;(prn "check-top-level" form)
   ;(prn "*ns*" *ns*)
   (with-bindings (dissoc (ana-clj/thread-bindings) #'*ns*) ; *ns* is managed by higher-level ops like check-ns1
     (binding [check/check-expr check-expr]
       (env/ensure (jana2/global-env)
         (-> form
             (ana2/unanalyzed-top-level (or env (jana2/empty-env)))
             (check-expr expected)))))))

(defmethod -check :const
  [expr expected]
  (const/check-const constant-type/constant-type false expr expected))

(defmethod -check :quote
  [expr expected]
  (quote/check-quote check-expr constant-type/constant-type expr expected))

(defmethod -check :map
  [expr expected]
  (map/check-map check-expr expr expected))

(defmethod -check :set
  [expr expected]
  (set/check-set check-expr expr expected))

(defmethod -check :vector
  [expr expected]
  (vec/check-vector check-expr expr expected))

(defn should-infer-vars? [expr]
  (-> (cu/expr-ns expr)
      find-ns
      meta
      :core.typed
      :experimental
      (contains? :infer-vars)))

(defn check-var [{:keys [var] :as expr} expected]
  {:pre [(var? var)]}
  (binding [vs/*current-expr* expr]
    (let [id (coerce/var->symbol var)
          _ (when-not (var-env/used-var? id)
              (var-env/add-used-var id))
          vsym id
          ut (var-env/get-untyped-var (cu/expr-ns expr) vsym)
          t (var-env/lookup-Var-nofail vsym)]
      ;(prn " annotation" t)
      ;(prn " untyped annotation" ut)
      (cond
        ;; we have an untyped annotation
        ut
        (if (cu/should-rewrite?)
          (assoc (cu/add-cast expr ut
                              {:positive (str "Annotation for " vsym)
                               :negative (str (cu/expr-ns expr))})
                 u/expr-type (below/maybe-check-below
                               (r/ret ut)
                               expected))
          (err/tc-delayed-error
            (str "Untyped var " id " found, but unable to rewrite to add contract"
            :return (assoc expr
                           u/expr-type (cu/error-ret expected)))))

        ;; we have a typed annotation
        t
        (assoc expr
               u/expr-type (below/maybe-check-below
                             (r/ret t)
                             expected))

        ;; :infer-vars are enabled for this namespace, this
        ;; var dereference is the dynamic type
        (or (should-infer-vars? expr)
            (impl/impl-case
              :clojure (= :unchecked 
                          (some-> vs/*check-config* deref :unannotated-var))
              :cljs nil))
        (do
          (println (str "Inferring " vsym " dereference as Unchecked"))
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret (r/-unchecked vsym))
                               expected)))
        (impl/impl-case
          :clojure (= :any (some-> vs/*check-config* deref :unannotated-var))
          :cljs nil)
        (do
          (println (str "Inferring " vsym " dereference as Any"))
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret r/-any)
                               expected)))


        :else
        (err/tc-delayed-error
          (str "Unannotated var " id)
          :return (assoc expr
                         u/expr-type (cu/error-ret expected)))))))

(defn set-erase-atoms [expr cred]
  {:pre [(u/expr-type cred)]}
  (let [_ (some-> expr ::with-meta/erase-atom (reset! true))
        _ (some-> expr ::replace-invoke-atom (reset! cred))]
    nil))

(defn ensure-within-beta-limit []
  (let [state vs/*beta-count*]
    (assert state)
    (if (< (:limit @state) (:count @state))
      (err/int-error
        (str "Exceeded the limit of symbolic beta reductions in a single form "
             "(" (:limit @state) ")"))
      (swap! state update :count inc))))

(defmethod -check :var
  [{:keys [var env] :as expr} expected]
  {:pre [(var? var)]}
  ;(prn " checking var" var)
  (or #_(when vs/*custom-expansions* 
        (when-let [args (::invoke-args expr)]
          (when-not expected
            (case (coerce/var->symbol var)
              ;; FIXME what if we break up final argument and it has an ann-form around it?
              ;; eg. (apply map [(ann-form identity [Any -> Any]))
              ;; eg. (apply map (ann-form [identity] (Seqable [Any -> Any])))
              ; moved to c.c.t.expand
              ;clojure.core/apply (when-let [red (beta-reduce/maybe-beta-reduce-apply
              ;                                    expr args
              ;                                    {:before-reduce ensure-within-beta-limit})]
              ;                     (let [cred (check-expr red (::invoke-expected expr))]
              ;                       (set-erase-atoms expr cred)
              ;                       cred))
              (let [vsym (ast-u/emit-form-fn expr)
                    form (with-meta (list* vsym (map ast-u/emit-form-fn args))
                                    (meta vsym))
                    mform (ana2/macroexpand-1 form env)]
                (when (not= form mform)
                  (ensure-within-beta-limit)
                  (let [cred (-> mform
                                 (ana2/analyze-form env)
                                 (update :raw-forms (fnil conj ())
                                         (vary-meta form assoc ::ana2/resolved-op (ana2/resolve-sym (first form) env)))
                                 ana2/run-passes
                                 (check-expr (::invoke-expected expr)))]
                    (set-erase-atoms expr cred)
                    cred)))))))
      (check-var expr expected)))

(defmethod -check :the-var
  [{:keys [^Var var env] :as expr} expected]
  {:pre [(var? var)]}
  (let [id (coerce/var->symbol var)
        macro? (.isMacro var)
        _ (when-not (or macro?
                        (var-env/used-var? id))
            (var-env/add-used-var id))
        t (var-env/lookup-Var-nofail id)
        t (cond
            t t
            macro? r/-any
            ;; :infer-vars are enabled for this namespace, this
            ;; var object is the dynamic type
            (should-infer-vars? expr) (r/-unchecked id)
            :else (err/tc-delayed-error (str "Unannotated var reference: " id)
                                        :form (ast-u/emit-form-fn expr)
                                        :return (r/TCError-maker)))]
    (assoc expr
           u/expr-type (binding [vs/*current-expr* expr]
                         (below/maybe-check-below
                           (r/ret (c/RClass-of Var [t t])
                                  (fo/-true-filter))
                           expected)))))

(defmulti -invoke-special (fn [{{:keys [form env] :as fexpr} :fn :as expr} expected] 
                            {:pre [(#{:invoke} (:op expr))
                                   (#{:unanalyzed} (:op fexpr))]
                             :post [((some-fn nil? symbol?) %)]}
                            (-> form
                                (ana2/resolve-sym env)
                                ana2/var->sym)))

(defmulti -invoke-apply (fn [{[{:keys [op form env] :as fexpr} :as args] :args :as expr} expected]
                          {:pre [(#{:invoke} (:op expr))]
                           :post [((some-fn nil? symbol?) %)]}
                          (when (seq args)
                            (assert (#{:unanalyzed} op))
                            (-> form
                                (ana2/resolve-sym env)
                                ana2/var->sym))))

(defn host-call-qname [{:keys [target] :as expr} _]
  {:pre [(#{:host-call} (:op expr))
         (#{:unanalyzed} (:op target))]
   :post [((some-fn nil?
                    (con/hvector-c? #{:static-call
                                      :instance-call}
                                    symbol?))
           %)]}
  (when ((some-fn symbol? class?) (:form target))
    (let [target (ana2/run-passes target)]
      (or (when-let [csym (and (= :const (:op target))
                               (= :class (:type target))
                               (:form target))]
            [:static-call (symbol
                            (name
                              (cond-> csym
                                (not (symbol? csym)) coerce/Class->symbol))
                            (str (:method expr)))])
          (when-let [tag (:tag target)]
            (let [tag (cond-> tag
                        (class? tag) coerce/Class->symbol)]
              (when (symbol? tag)
                (let [sym (symbol (str tag) (str (:method expr)))]
                  [:instance-call sym]))))))))

(defmulti -host-call-special #'host-call-qname)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups


; only handle special case that the first argument is literal class
(defmethod -invoke-special 'clojure.core/cast
  [{:keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when (#{2} (count args))
    (let [cargs (mapv check-expr args)
          ct (-> (first cargs) u/expr-type r/ret-t c/fully-resolve-type)]
      (when (and (r/Value? ct) (class? (:val ct)))
        (let [v-t (-> (check-expr (second args)) u/expr-type r/ret-t)
              t (c/In v-t (c/Un r/-nil (c/RClass-of-with-unknown-params (:val ct))))]
          (-> expr
              (update :fn check-expr)
              (assoc :args cargs
                     u/expr-type (below/maybe-check-below
                                   (r/ret t)
                                   expected))))))))

(defmethod -invoke-special 'clojure.core.typed/var>*
  [expr expected]
  {:post [(or (nil? %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when-not (#{1} (count (:args expr)))
    (err/int-error (str "Wrong number of arguments to clojure.core.typed/var>,"
                        " expected 1, given " (count (:args expr)))))
  (let [{[sym-expr :as args] :args fexpr :fn :as expr}
        (-> expr
            (update-in [:args 0] ana2/run-passes))
        sym (ast-u/quote-expr-val sym-expr)
        _ (assert (symbol? sym))
        t (var-env/lookup-Var-nofail sym)
        _ (when-not t
            (err/tc-delayed-error (str "Unannotated var: " sym)))]
    (-> expr
        ; var>* is internal, don't check
        #_(update :fn check-expr)
        (assoc u/expr-type (below/maybe-check-below
                             (r/ret (or t (r/TCError-maker)))
                             expected)))))

; ignore some keyword argument related intersections
(defmethod -invoke-special 'clojure.core/seq?
  [{:keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when (#{1} (count args))
    (let [{[ctarget] :args :as cexpr}
          (-> expr
              (update :fn check-expr)
              (update :args #(mapv check-expr %)))
          targett (-> ctarget u/expr-type r/ret-t c/fully-resolve-type)]
      (cond 
        ; records never extend ISeq
        ;; TODO move this to subtyping
        (r/Record? targett)
        (assoc cexpr
               u/expr-type (below/maybe-check-below
                             (r/ret r/-false (fo/-false-filter))
                             expected))))))

(defmethod -invoke-special 'clojure.core/extend
  [expr expected]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not ((every-pred odd? pos?) (count (:args expr)))
    (err/int-error (str "Wrong number of arguments to extend, expected at least one with an even "
                        "number of variable arguments, given " (count (:args expr)))))
  (let [{[catype & protos :as args] :args :as expr}
        (-> expr
            ;atype
            (update-in [:args 0] check-expr))
        expr (-> expr
                 ; don't check extend
                 ;(update :fn check-expr)
                 (assoc u/expr-type (below/maybe-check-below
                                      (r/ret r/-nil (fo/-false-filter))
                                      expected)))
        ; this is a Value type containing a java.lang.Class instance representing
        ; the type extending the protocol, or (Value nil) if extending to nil
        target-literal-class (r/ret-t (u/expr-type catype))]
    (cond
      (not (and (r/Value? target-literal-class)
                ((some-fn class? nil?) (:val target-literal-class))))
      (err/tc-delayed-error
        (str "Must provide a Class or nil as first argument to extend, "
             "got " (pr-str (prs/unparse-type target-literal-class)))
        :return expr)

      (and expected (not (sub/subtype? r/-any (r/ret-t expected))))
      (do (cu/expected-error r/-any expected)
          expr)
      :else
      (let [; this is the actual core.typed type of the thing extending the protocol
            target-type (let [v (:val target-literal-class)]
                          (if (nil? v)
                            r/-nil
                            (c/RClass-of-with-unknown-params v)))
            ; build expected types for each method map
            extends (for [[prcl-expr mmap-expr] (partition 2 protos)]
                      (let [prcl-expr (ana2/run-pre-passes (ana2/analyze-outer-root prcl-expr))
                            protocol (do (when-not (= :var (:op prcl-expr))
                                           (err/int-error "Must reference protocol directly with var in extend"))
                                         (ptl-env/resolve-protocol (coerce/var->symbol (:var prcl-expr))))
                            _ (when-not (r/Protocol? protocol)
                                (err/int-error (str "Expecting Protocol type, found " protocol)))
                            expected-mmap (c/make-HMap ;get all combinations
                                                       :optional
                                                       (into {}
                                                             (map
                                                               (fn [[msym mtype]]
                                                                 [(r/-val (keyword (name msym))) 
                                                                  (cu/extend-method-expected target-type mtype)]))
                                                             (:methods protocol)))]
                        {:expected-hmap expected-mmap
                         :prcl-expr prcl-expr
                         :mmap-expr mmap-expr}))
            cargs (into [catype]
                        (mapcat
                          (fn [{:keys [mmap-expr expected-hmap prcl-expr]}]
                            (let [cprcl-expr (check-expr prcl-expr)
                                  cmmap-expr (check-expr mmap-expr (r/ret expected-hmap))]
                              [cprcl-expr cmmap-expr])))
                        extends)
            _ (assert (== (count cargs)
                          (count args)))]
        (assoc expr
               :args cargs)))))

;into-array>
;
; Usage: (into-array> javat cljt coll)
;        (into-array> cljt coll)
(defmethod -invoke-special 'clojure.core.typed/into-array>*
  [{:keys [args] :as expr} expected]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{2 3 4} (count args)) 
    (err/int-error "Wrong number of args to into-array>*"))
  (let [has-java-syn? (#{3 4} (count args))
        [javat-syn cljt-syn coll-expr]
        (cond 
          (= 3 (count args)) args
          (= 4 (count args)) (next args) ;handle temporary hacky case
          :else (cons nil args))

        javat-syn (some-> javat-syn ana2/run-passes)
        cljt-syn (some-> cljt-syn ana2/run-passes)
        javat (let [syn (or (when has-java-syn? (ast-u/quote-expr-val javat-syn))  ; generalise javat-syn if provided, otherwise cljt-syn
                            (ast-u/quote-expr-val cljt-syn))
                    c (-> 
                        (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                          (prs/parse-type syn))
                        arr-ops/Type->array-member-Class)]
                (assert (class? c))
                c)
        cljt (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
               (prs/parse-type (ast-u/quote-expr-val cljt-syn)))
        ccoll (check-expr coll-expr (r/ret (c/Un r/-nil (c/RClass-of Seqable [cljt]))))]
    (-> expr
        ; into-array>* is internal, don't check it
        #_(update :fn check-expr)
        ; the coll is always last
        (assoc :args (-> args pop (conj ccoll))
               u/expr-type (below/maybe-check-below
                             (r/ret (r/PrimitiveArray-maker javat cljt cljt))
                             expected)))))

;not
(defmethod -invoke-special 'clojure.core/not
  [{:keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (when (#{1} (count args)) 
    (let [[ctarget :as cargs] (mapv check-expr args)
          {fs+ :then fs- :else} (-> ctarget u/expr-type r/ret-f)]
      (assoc expr
             :args cargs
             u/expr-type (below/maybe-check-below
                           (r/ret (prs/parse-type 'boolean) 
                                  ;flip filters
                                  (fo/-FS fs- fs+)
                                  obj/-empty)
                           expected)))))

;to-array
(defmethod -invoke-special 'clojure.core/to-array
  [{:keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (when (#{1} (count args)) 
    (let [[ctarget :as cargs] (mapv check-expr args)
          targett (-> ctarget u/expr-type r/ret-t)]
      ;(prn "to-array" targett)
      (cond
        ;; handle seq-to-map-for-destructuring expansion introduced in Clojure 1.11
        (sub/subtype? targett r/-any-kw-args-seq)
        (let [res (reduce (fn [t union-t]
                            (if-some [intersection-ts
                                      (seq (keep #(do (assert ((some-fn r/KwArgsSeq? r/CountRange?)
                                                               %)
                                                              (print-str "TODO" (class %)))
                                                      (when (r/KwArgsSeq? %)
                                                        (c/KwArgsSeq->KwArgsArray %)))
                                                 (c/flatten-intersections [union-t])))]
                              (c/Un t (apply c/In intersection-ts))
                              t))
                          (c/Un)
                          (c/flatten-unions [targett]))
              _ (assert (not (sub/subtype? res (c/Un)))
                        targett)]
          (assoc expr
                 :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret res)
                               expected)))))))

;get
(defmethod -invoke-special 'clojure.core/get
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (get/invoke-get check-expr expr expected))

(defmethod -host-call-special '[:static-call clojure.lang.RT/get]
  [{:keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (get/invoke-get check-expr expr expected))

;FIXME should be the same as (apply hash-map ..) in invoke-apply
(defmethod -host-call-special '[:static-call clojure.lang.PersistentHashMap/create]
  [expr expected]
  {:pre [(#{:host-call} (:op expr))
         (every? (comp #{:unanalyzed} :op) (:args expr))]
   :post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (-> % :target u/expr-type r/TCResult?)))]}
  (when (#{1} (count (:args expr)))
    (binding [vs/*current-expr* expr]
      (let [{[target] :args :as expr} (-> expr
                                          (update :args #(mapv check-expr %)))
            targett (-> target u/expr-type r/ret-t c/fully-resolve-type)]
        (cond
          (and (sub/subtype? targett (c/Un r/-nil r/-any-kw-args-seq))
               (not (sub/subtype? targett r/-nil)))
          (let [res (reduce (fn [t union-t]
                              (if-some [intersection-ts
                                        (seq (keep #(do (assert ((some-fn r/KwArgsSeq? r/Nil? r/CountRange?)
                                                                 %)
                                                                (print-str "TODO" (class %)))
                                                        (when (r/KwArgsSeq? %)
                                                          (when (-> % :kw-args-regex :maybe-trailing-nilable-non-empty-map?)
                                                            (err/tc-delayed-error
                                                              (str "Cannot pass KwArgsSeq to clojure.lang.PersistentHashMap/create "
                                                                   "when :maybe-trailing-nilable-non-empty-map? is true.")))
                                                          (c/KwArgsSeq->HMap %)))
                                                   (c/flatten-intersections [union-t])))]
                                (c/Un t (apply c/In intersection-ts))
                                t))
                            (c/Un)
                            (c/flatten-unions [targett]))
                _ (assert (not (sub/subtype? res (c/Un)))
                          targett)]
            (-> expr
                (update :target check-expr)
                (assoc u/expr-type (below/maybe-check-below
                                     (r/ret res)
                                     expected))))
          (r/HeterogeneousSeq? targett)
          (let [res (reduce (fn [t [kt vt]]
                              {:pre [(r/HeterogeneousMap? t)]}
                              (if (= (c/Un) vt)
                                ;preserve bottom
                                (reduced vt)
                                (assoc-in t [:types kt] vt)))
                            (c/-complete-hmap {})
                            (:types targett))]
            (-> expr
                (update :target check-expr)
                (assoc u/expr-type (below/maybe-check-below
                                     (r/ret res)
                                     expected)))))))))

(defmethod -host-call-special '[:static-call clojure.lang.PersistentArrayMap/createAsIfByAssoc]
  [expr expected]
  {:pre [(#{:host-call} (:op expr))
         (every? (comp #{:unanalyzed} :op) (:args expr))]
   :post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (-> % :target u/expr-type r/TCResult?)))]}
  (when (#{1} (count (:args expr)))
    (binding [vs/*current-expr* expr]
      (let [{[target] :args :as expr} (-> expr
                                          (update :args #(mapv check-expr %)))
            targett (-> target u/expr-type r/ret-t c/fully-resolve-type)]
        (cond
          ;; handle seq-to-map-for-destructuring expansion, which always passes
          ;; the result of to-array
          (r/KwArgsArray? targett)
          (-> expr
              (update :target check-expr)
              (assoc u/expr-type (below/maybe-check-below
                                   (r/ret (c/KwArgsArray->HMap targett)
                                          (fo/-true-filter))
                                   expected))))))))

(defmethod -check :prim-invoke
  [expr expected]
  (-> expr
      (assoc :op :invoke)
      check-expr 
      (assoc :op :prim-invoke)))

(defmethod -check :keyword-invoke
  [{kw :keyword :keys [target] :as expr} expected]
  {:pre [(and (#{:const} (:op kw))
              (keyword? (:val kw)))]
   :post [(r/TCResult? (u/expr-type %))]}
  (let [ckw (check-expr kw)
        ctarget (check-expr target)]
    (assoc expr
           :keyword ckw
           :target ctarget
           u/expr-type (invoke-kw/invoke-keyword
                         expr
                         (u/expr-type ckw)
                         (u/expr-type ctarget)
                         nil
                         expected))))

;; TODO refactor into own file
(defn protocol-invoke [check-fn {:keys [protocol-fn target args] :as expr} expected]
  (let [cprotocol-fn (check-fn protocol-fn)
        ctarget (check-fn target)
        cargs (mapv check-fn args)
        ftype (u/expr-type cprotocol-fn)
        argtys (map u/expr-type (cons ctarget cargs))
        actual (funapp/check-funapp cprotocol-fn (cons ctarget cargs) ftype argtys expected)]
    (assoc expr
           :target ctarget
           :protocol-fn cprotocol-fn
           :args cargs
           u/expr-type actual)))

(defmethod -check :protocol-invoke ; protocol methods
  [expr expected]
  (protocol-invoke check-expr expr expected))

;binding
;FIXME use `check-normal-def`
;FIXME record checked-var-def info
(defmethod -invoke-special 'clojure.core/push-thread-bindings
  [{[bindings-expr :as args] :args :as expr} expected]
  {:post [((every-pred vector? #(= 1 (count %))) (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (when-not (= 1 (count args))
    (err/int-error (str "push-thread-bindings expected one argument, given " (count args))))
  (let [bindings-expr (ana2/run-pre-passes (ana2/analyze-outer-root bindings-expr))
        bindings-expr (cond-> bindings-expr 
                        (#{:invoke} (-> bindings-expr :op))
                        (update :fn ana2/run-passes))
        ; only support (push-thread-bindings (hash-map ~@[var bnd ...]))
        ; like `binding`s expansion
        _ (when-not (and (#{:invoke} (-> bindings-expr :op))
                         (#{#'hash-map} (-> bindings-expr :fn :var))
                         (even? (count (-> bindings-expr :args))))
            (err/nyi-error (str "Can only check push-thread-bindings with a well-formed call to hash-map as first argument"
                                " (like bindings expansion)")))
        new-bindings-exprs (partition 2 (-> bindings-expr :args))
        cargs
        [(assoc bindings-expr
                :args
                (into []
                      (mapcat (fn [[var-expr bnd-expr]]
                                (let [{:keys [op var] :as var-expr} (ana2/run-pre-passes (ana2/analyze-outer-root var-expr))]
                                  (when-not (#{:the-var} op)
                                    (err/int-error (str "push-thread-bindings must have var literals for keys")))
                                  (let [expected (var-env/type-of (coerce/var->symbol var))
                                        cvar-expr (check-expr var-expr)
                                        cexpr (check-expr bnd-expr (r/ret expected))
                                        actual (-> cexpr u/expr-type r/ret-t)]
                                    (when (not (sub/subtype? actual expected))
                                      (err/tc-delayed-error (str "Expected binding for "
                                                                 (coerce/var->symbol var)
                                                                 " to be: " (prs/unparse-type expected)
                                                                 ", Actual: " (prs/unparse-type actual))))
                                    [cvar-expr cexpr]))))
                      new-bindings-exprs))]]
    (-> expr
        ; push-thread-bindings is unannotated
        #_(update :fn check-expr)
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret r/-nil)
                             expected)))))

(defn typing-rule-opts [expr]
  {:post [(map? %)]}
  (let [opts (:form (nth (:statements expr) 2))]
    (assert (and (seq? opts)
                 (= 2 (count opts))
                 (#{'quote} (first opts)))
            (str "Options of typing rule must be a quoted map literal, "
                 "found: " (pr-str opts)))
    ; (quote {...})
    (second opts)))

(def typing-rule-expr-kw :ret)

(defn invoke-typing-rule
  [vsym {:keys [env] :as expr} expected]
  ;(prn "invoke-typing-rule" vsym)
  (let [unparse-type-verbose #(binding [vs/*verbose-types* false]
                                (prs/unparse-type %))
        subtype? (fn [s t]
                   (let [s (prs/parse-type s)
                         t (prs/parse-type t)]
                     (sub/subtype? s t)))
        solve (fn [t q]
                {:pre [(map? t)
                       (contains? t :type)]
                 :post [((some-fn nil? map?) %)]}
                (let [;; atm only support query = (All [x+] [in :-> out])
                      query (prs/parse-type q)
                      _ (assert (r/Poly? query))
                      names (c/Poly-fresh-symbols* query)
                      bbnds (c/Poly-bbnds* names query)
                      body (c/Poly-body* names query)
                      _ (assert (r/FnIntersection? body))
                      _ (assert (= 1 (count (:types body))))
                      arity (first (:types body))
                      _ (assert (r/Function? arity))
                      _ (assert (= 1 (count (:dom arity))))
                      _ (assert (not-any? #(% arity) [:rest :drest :kws :prest :pdot]))
                      _ (assert (= (fo/-simple-filter) (:fl (:rng arity))))
                      _ (assert (= obj/-empty (:o (:rng arity))))

                      lhs (prs/parse-type (:type t))
                      rhs (first (:dom arity))
                      out (:t (:rng arity))
                      substitution (cgen/handle-failure
                                     (cgen/infer
                                       (zipmap names bbnds)
                                       {}
                                       [lhs]
                                       [rhs]
                                       out))]
                  (when substitution
                    {:type (unparse-type-verbose
                             (subst/subst-all substitution out))})))
        #_#_
        solve-subtype (fn [vs f]
                        {:pre [(apply distinct? vs)
                               (every? symbol? vs)]}
                        (let [gvs (map gensym vs)
                              gvs->vs (zipmap gvs vs)
                              syns (apply f gvs)
                              [lhs rhs] (tvar-env/with-extended-tvars gvs
                                          (mapv prs/parse-type syns))
                              substitution
                              (cgen/handle-failure
                                (cgen/infer
                                  (zipmap gvs (repeat r/no-bounds))
                                  {}
                                  [lhs]
                                  [rhs]
                                  r/-any))]
                          (when substitution
                            (into {}
                                  (comp (filter (every-pred (comp (set gvs) key)
                                                            (comp crep/t-subst? val)))
                                        (map (fn [[k v]]
                                               [(gvs->vs k)
                                                (unparse-type-verbose (:type v))])))
                                  substitution))))
        with-updated-locals (fn [locals f]
                              (let [locals (into {}
                                                 (map (fn [[k v]]
                                                        [(prs/uniquify-local k)
                                                         (prs/parse-type v)]))
                                                 locals)]
                                (lex/with-locals locals
                                  (f))))
        rule-args {:vsym vsym
                   :opts (typing-rule-opts expr)
                   :expr (typing-rule-expr-kw expr)
                   :locals (:locals env)
                   :expected (some-> expected cu/TCResult->map)
                   ;:uniquify-local prs/uniquify-local
                   :with-updated-locals with-updated-locals
                   :maybe-check-expected (fn [actual expected]
                                           {:pre [(map? actual)
                                                  ((some-fn nil? map?) expected)]
                                            :post [(map? %)]}
                                           (->
                                             (below/maybe-check-below
                                               (cu/map->TCResult actual)
                                               (cu/maybe-map->TCResult expected))
                                             cu/TCResult->map))
                   :check (fn check-fn
                            ([expr] (check-fn expr nil))
                            ([expr expected]
                             {:pre [((some-fn nil? map?) expected)]}
                             (let [ret (some-> expected cu/map->TCResult)
                                   cexpr (check-expr expr ret)]
                               (assoc cexpr ::rules/expr-type (cu/TCResult->map (u/expr-type cexpr))))))
                   ;:solve-subtype solve-subtype
                   :solve solve
                   :subtype? subtype?
                   :emit-form ast-u/emit-form-fn
                   :abbreviate-type (fn [t]
                                      (let [m (prs/parse-type t)]
                                        (binding [vs/*verbose-types* false]
                                          (prs/unparse-type m))))
                   :delayed-error (fn [s opts]
                                    (let [opts (-> opts
                                                   (update :expected cu/maybe-map->TCResult)
                                                   (cond->
                                                     (contains? opts :actual)
                                                     (update :actual prs/parse-type)))]
                                      (apply err/tc-delayed-error s (apply concat opts))))
                   :expected-error (fn [s t opts]
                                     (let [s (prs/parse-type s)
                                           t (cu/map->TCResult t)
                                           opts (-> opts
                                                    (update :expected cu/map->TCResult))]
                                       (apply cu/expected-error s t (apply concat opts))))
                   :internal-error (fn [s opts]
                                     ;; TODO args
                                     (let [opts (-> opts
                                                    (update :expected cu/maybe-map->TCResult))]
                                       (apply err/int-error s (apply concat opts))))}

        {out-expr-type ::rules/expr-type :as cexpr} (rules/typing-rule rule-args)
        out-tcresult (cu/map->TCResult out-expr-type)]
    (-> expr
        (assoc u/expr-type out-tcresult
               typing-rule-expr-kw (-> cexpr
                                       (dissoc ::rules/expr-type)
                                       (assoc u/expr-type out-tcresult))))))

;=
(defmethod -invoke-special 'clojure.core/= 
  [{:keys [args] :as expr} expected]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update :fn check-expr)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected)))))

;not=
(defmethod -invoke-special 'clojure.core/not=
  [{:keys [args] :as expr} expected]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update :fn check-expr)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv :not= (map u/expr-type cargs) expected)))))

;identical
(defmethod -host-call-special '[:static-call clojure.lang.Util/identical]
  [expr expected]
  {:pre [(#{:host-call} (:op expr))]
   :post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [{:keys [args] :as expr} (-> expr
                                    (update :target check-expr)
                                    (update :args #(mapv check-expr %)))]
    (assoc expr
           u/expr-type (equiv/tc-equiv :identical? (map u/expr-type args) expected))))

;equiv
(defmethod -host-call-special '[:static-call clojure.lang.Util/equiv]
  [expr expected]
  {:pre [(#{:host-call} (:op expr))]
   :post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [{:keys [args] :as expr} (-> expr
                                    (update :target check-expr)
                                    (update :args #(mapv check-expr %)))]
    (assoc expr
           u/expr-type (equiv/tc-equiv := (map u/expr-type args) expected))))

;isa? (2 arity is special)
(defmethod -invoke-special 'clojure.core/isa?
  [{:keys [args] :as expr} expected]
  (when (#{2} (count args))
    (let [[cchild-expr cparent-expr :as cargs] (mapv check-expr args)]
      (-> expr
          (update :fn check-expr)
          (assoc :args cargs
                 u/expr-type (isa/tc-isa? (u/expr-type cchild-expr)
                                          (u/expr-type cparent-expr)
                                          expected))))))

;apply
(defmethod -invoke-special 'clojure.core/apply
  [{:keys [args env] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (apply/maybe-check-apply check-expr -invoke-apply expr expected))

(defonce ^:dynamic *inst-ctor-types* nil)
(t/tc-ignore
(set-validator! #'*inst-ctor-types* (some-fn nil? (con/every-c? r/Type?)))
)

;TODO this should be a special :do op
;manual instantiation for calls to polymorphic constructors
(defmethod -invoke-special 'clojure.core.typed/inst-poly-ctor
  [expr expected]
  {:pre [(#{2} (count (:args expr)))]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [{[ctor-expr targs-exprs] :args :as expr} (-> expr
                                                     (update-in [:args 1] ana2/run-passes))
        targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                (mapv prs/parse-type (ast-u/quote-expr-val targs-exprs)))
        cexpr (binding [*inst-ctor-types* targs]
                (check-expr ctor-expr))]
    (-> expr 
        (assoc-in [:args 0] cexpr)
        (assoc u/expr-type (u/expr-type cexpr)))))

;debug printing
(defmethod -invoke-special 'clojure.core.typed/print-env
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (#{1} (count (:args expr)))
    (err/int-error (str "Wrong arguments to print-env, Expected 1, found " (count (:args expr)))))
  (let [{[debug-string :as args] :args :as expr} (-> expr
                                                     (update-in [:args 0] ana2/run-passes))]
    (when-not (= :const (:op debug-string))
      (err/int-error "Must pass print-env a string literal"))
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    (prs/with-unparse-ns (cu/expr-ns expr)
      (print-env/print-env*))
    ;DO NOT REMOVE
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret r/-nil (fo/-false-filter) obj/-empty)
                         expected))))

;filter printing
(defmethod -invoke-special 'clojure.core.typed/print-filterset
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (#{2} (count (:args expr)))
    (err/int-error (str "Wrong arguments to print-filterset. Expected 2, found " (count (:args expr)))))
  (let [{[debug-string form :as args] :args :as expr} (-> expr
                                                          (update-in [:args 0] ana2/run-passes))
        _ (when-not (= :const (:op debug-string)) 
            (err/int-error "Must pass print-filterset a string literal as the first argument."))
        cform (check-expr form expected)
        cargs (assoc args 1 cform)
        t (u/expr-type cform)]
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    ;(prn (:fl t))
    (prs/with-unparse-ns (cu/expr-ns expr)
      (if (fl/FilterSet? (:fl t))
        (do (pprint/pprint (prs/unparse-filter-set (:fl t)))
            (flush))
        (prn (:fl t)))
      (prn (prs/unparse-object (:o t))))
    ;DO NOT REMOVE
    (assoc expr
           :args cargs
           u/expr-type t)))

;seq
(defmethod -invoke-special 'clojure.core/seq
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (or (nthnext/check-seq check-expr expr expected)
      (invoke/normal-invoke expr fexpr args expected)))

;make vector
(defmethod -invoke-special 'clojure.core/vector
  [{:keys [args] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update :fn check-expr)
        (assoc 
          :args cargs
          u/expr-type (below/maybe-check-below
                        (r/ret (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                                        :filters (mapv (comp r/ret-f u/expr-type) cargs)
                                        :objects (mapv (comp r/ret-o u/expr-type) cargs)))
                        expected)))))

;(apply concat hmap)
(defmethod -invoke-apply 'clojure.core/concat
  [{[_concat-fn_ & args] :args :as expr} expected]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (let [cargs (mapv check-expr args) ;FIXME possible repeated check-expr
        tmap (when (#{1} (count cargs))
               (c/fully-resolve-type (r/ret-t (u/expr-type (last cargs)))))]
    (binding [vs/*current-expr* expr]
      (when (r/HeterogeneousMap? tmap)
        (let [r (c/HMap->KwArgsSeq tmap)]
          (-> expr
              (update :fn check-expr)
              (assoc u/expr-type (below/maybe-check-below
                                   (r/ret r (fo/-true-filter))
                                   expected))))))))

;apply hash-map
(defmethod -invoke-apply 'clojure.core/hash-map
  [{[fn-expr & args] :args :as expr} expected]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (let [cargs (mapv check-expr args)]
    (cond
      (and (#{1} (count cargs))
           (r/KwArgsSeq? (u/expr-type (last cargs))))
      (-> expr
          (update :fn check-expr)
          ;; FIXME add annotation for hash-map to check fn-expr
          (assoc :args (vec (concat [(ana2/run-passes fn-expr)] cargs))
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/KwArgsSeq->HMap (-> (u/expr-type (last cargs)) r/ret-t)))
                               expected)))

      (and (seq cargs)
           (r/HSequential?  (r/ret-t (u/expr-type (last cargs))))
           ;; every key must be a Value
           (let [kvs (vec
                       (concat (map (comp r/ret-t u/expr-type) (butlast cargs))
                               (mapcat vector (:types (r/ret-t (u/expr-type (last cargs)))))))]
             (and (even? (count kvs))
                  (every? r/Value? (keys (apply hash-map kvs))))))
      (-> expr
          (update :fn check-expr)
          ;; FIXME add annotation for hash-map to check fn-expr
          (assoc :args (vec (concat [(ana2/run-passes fn-expr)] cargs))
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/-complete-hmap
                                        (apply hash-map (concat (map (comp r/ret-t u/expr-type) (butlast cargs))
                                                                (mapcat vector (:types (r/ret-t (u/expr-type (last cargs)))))))))
                               expected))))))


;nth
(defmethod -host-call-special '[:static-call clojure.lang.RT/nth]
  [{:keys [args] :as expr} expected]
  {:pre [(#{:host-call :invoke} (:op expr))
         (every? (every-pred (comp #{:unanalyzed} :op)
                             (complement u/expr-type))
                 args)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nth/invoke-nth expr expected))

(defmethod -invoke-special 'clojure.core/nth
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nth/invoke-nth expr expected))

;nthnext
(defmethod -invoke-special 'clojure.core/nthnext
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nthnext/check-nthnext check-expr expr expected))

;next
(defmethod -invoke-special 'clojure.core/next
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nthnext/check-next check-expr expr expected))

;rest
(defmethod -invoke-special 'clojure.core/rest
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nthnext/check-rest check-expr expr expected))

;keeping because it might be handy later
#_
(defn first-result [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Result?) %)]}
  (let [ftype (fn ftype [t]
                {:pre [(r/Type? t)]
                 :post [((some-fn nil? r/Result?) %)]}
                (let [t (c/fully-resolve-type t)]
                  (cond
                    (r/Union? t) (let [ts (mapv ftype (:types t))]
                                   (when (every? identity ts)
                                     (apply c/union-Results ts)))
                    (r/Intersection? t) (when-let [ts (seq (keep ftype (:types t)))]
                                          (apply c/intersect-Results ts))
                    (r/Nil? t) (r/make-Result r/-nil (fo/-false-filter))
                    (r/HSequential? t) (cond
                                         (seq (:types t))
                                         (r/make-Result (first (:types t))
                                                        (first (:fs t))
                                                        (first (:objects t)))

                                         (:rest t) (r/make-Result (c/Un r/-nil (:rest t)))
                                         (:drest t) (r/make-Result r/-any)

                                         (empty? (:types t)) (r/make-Result (r/ret r/-nil (fo/-false-filter)))))))]
    (ftype (nthnext/seq-type t))))

;first
#_
(defmethod -invoke-special 'clojure.core/first
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (when-not (= 1 (count args))
    (err/int-error (str "'first' accepts 1 argument, found "
                        (count args))))
  #_
  (when vs/*custom-expansions*
    (let [[coll :as cargs] (mapv check-expr args)
          ct (r/ret-t (u/expr-type coll))
          fres (first-result ct)]
      (when fres
        (assoc expr
               :args cargs
               u/expr-type (r/Result->TCResult fres)))))
  nil)

;assoc
(defmethod -invoke-special 'clojure.core/assoc
  [{:keys [args] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [[target & keyvals] args

        _ (when-not (<= 3 (count args))
            (err/int-error (str "assoc accepts at least 3 arguments, found "
                                     (count args))))
        _ (when-not (even? (count keyvals))
            (err/int-error "assoc accepts an even number of keyvals"))

        ctarget (check-expr target)
        targetun (-> ctarget u/expr-type r/ret-t)
        ckeyvals (mapv check-expr keyvals)
        keypair-types (partition 2 (map u/expr-type ckeyvals))
        cargs (into [ctarget] ckeyvals)]
    (if-let [new-hmaps (apply assoc-u/assoc-type-pairs targetun keypair-types)]
      (-> expr
        (update :fn check-expr)
        (assoc
          :args cargs
          u/expr-type (below/maybe-check-below
                        (r/ret new-hmaps
                               (fo/-true-filter)) ;assoc never returns nil
                        expected)))
      
      ;; to do: improve this error message
      (err/tc-delayed-error (str "A call to assoc failed to type check with target expression of type:\n\t" (prs/unparse-type targetun)
                                 "\nand key/value pairs of types: \n\t"
                                 (str/join " " (map (comp pr-str prs/unparse-type :t u/expr-type) ckeyvals)))
                            ;; first argument is to blame, gather any blame information from there
                            :expected (u/expr-type ctarget)
                            :return (-> expr
                                        (update :fn check-expr)
                                        (assoc
                                          :args cargs
                                          u/expr-type (cu/error-ret expected)))))))

(defmethod -invoke-special 'clojure.core/dissoc
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [_ (when-not (seq args)
            (err/int-error (str "dissoc takes at least one argument, given: " (count args))))
        ;FIXME possible repeated type checking
        [ctarget & cdissoc-args :as cargs] (mapv check-expr args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cdissoc-args)]
    (when-let [new-t (assoc-u/dissoc-keys ttarget targs)]
      (-> expr
          (update :fn check-expr)
          (assoc
            :args cargs
            u/expr-type (below/maybe-check-below
                          (r/ret new-t)
                          expected))))))

; merge
(defmethod -invoke-special 'clojure.core/merge
  [{fexpr :fn :keys [args] :as expr} expected]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (let [;FIXME possible repeated type checking
        cargs (mapv check-expr args)
        targs (map u/expr-type cargs)]
    (when-some [merged (apply assoc-u/merge-types targs)]
      (-> expr
          (update :fn check-expr)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret merged)
                               expected))))))

;conj
(defmethod -invoke-special 'clojure.core/conj
  [{fexpr :fn :keys [args] :as expr} expected]
  (let [;FIXME possible repeated type checking
        [ctarget & cconj-args :as cargs] (mapv check-expr args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cconj-args)]
    (when-let [conjed (apply assoc-u/conj-types ttarget targs)]
      (-> expr
          (update :fn check-expr)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret conjed
                                      (fo/-true-filter) ; conj never returns nil
                                      obj/-empty)
                               expected))))))

(comment
  (method-expected-type (prs/parse-type '[Any -> Any])
                        (prs/parse-type '(Value :op))
                        (prs/parse-type '(Value :if)))
  ;=> ['{:if Any} -> Any]
  )

; cli
;TODO add cargs to result
(defmethod -invoke-special 'clojure.tools.cli/cli
  [{[args-expr & specs-exprs] :args :keys [env] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (binding [vs/*current-env* env]
    (let [args-expected-ty (prs/parse-type `(t/Seqable t/Str))
          cargs-expr (binding [vs/*current-env* (:env args-expr)]
                       (check-expr args-expr))
          _ (when-not (sub/subtype? 
                        (-> cargs-expr u/expr-type r/ret-t)
                        args-expected-ty)
              (binding [vs/*current-env* (:env args-expr)]
                (cu/expected-error (-> cargs-expr u/expr-type r/ret-t) (r/ret args-expected-ty))))
          spec-map-ty (reduce (fn [t spec-expr]
                                (if-let [[keyt valt] (cli/parse-cli-spec check-expr spec-expr)]
                                  (-> t
                                    (assoc-in [:types keyt] valt))
                                  ; resort to a general type
                                  (do
                                    ;(prn "cli: giving up because of" (ast-u/emit-form-fn spec-expr)
                                         ;"\n" spec-expr)
                                    (reduced 
                                      (c/RClass-of IPersistentMap [(c/RClass-of clojure.lang.Keyword) r/-any])))))
                              (c/-complete-hmap {})
                              specs-exprs)

          actual (r/-hvec [spec-map-ty 
                           (prs/parse-type `(t/Seqable t/Str))
                           (prs/parse-type `t/Str)])
          _ (when expected
              (when-not (sub/subtype? actual (r/ret-t expected))
                (cu/expected-error 
                  actual expected)))
          cargs (vec (cons cargs-expr specs-exprs))]
      (-> expr
          (update :fn check-expr)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret actual)
                               expected))))))

(defmethod -invoke-special 'typed.cljc.checker.check.utils/special-typed-expression
  [expr expected]
  (let [_ (assert (= 1 (count (:args expr))))
        {[type-expr] :args :keys [env] :as expr} (-> expr
                                                     (update-in [:args 0] ana2/run-passes))
        _ (assert (= :quote (:op type-expr)))
        _ (assert (= :const (-> type-expr :expr :op))
                  (-> type-expr :expr :op))
        t (prs/parse-type (-> type-expr :expr :val))]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret t)
                         expected))))

; FIXME this needs a line number from somewhere!
(defmethod -host-call-special '[:instance-call clojure.lang.MultiFn/addMethod]
  [expr expected]
  {:pre [(every? (every-pred (complement u/expr-type)
                             (comp #{:unanalyzed} :op))
                 (cons (:target expr) (:args expr)))]
   :post [#_(every? :post-done (cons (:target %) (:args %)))]}
  (when-not (= 2 (count (:args expr)))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn/addMethod"))
  (let [{[dispatch-val-expr _] :args target :target :keys [env] :as expr}
        (cond-> expr
          (-> expr :target :form symbol?) (update :target ana2/run-passes))
        _ (when-not (#{:var} (:op target))
            (err/int-error "Must call addMethod with a literal var"))
        var (:var target)
        _ (assert (var? var))
        mmsym (coerce/var->symbol var)
        expr (assoc expr
                    u/expr-type (binding [vs/*current-expr* expr]
                                  (below/maybe-check-below
                                    (r/ret (c/RClass-of clojure.lang.MultiFn))
                                    expected)))
        default? (cu/default-defmethod? var (ast-u/emit-form-fn dispatch-val-expr))
        unannotated-def (some-> vs/*check-config* deref :unannotated-def)]
    (cond
      (and (= :unchecked unannotated-def)
           (not (var-env/lookup-Var-nofail mmsym)))
      (-> expr
          (update :args #(mapv ana2/run-passes %)))

      ;skip if warn-on-unannotated-vars is in effect
      (or (and (ns-opts/warn-on-unannotated-vars? (cu/expr-ns expr))
               (not (var-env/lookup-Var-nofail mmsym)))
          (not (var-env/check-var? mmsym)))
      (do (u/tc-warning (str "Not checking defmethod " mmsym " with dispatch value: " 
                             (pr-str (ast-u/emit-form-fn dispatch-val-expr))))
          (-> expr
              (update :args #(mapv ana2/run-passes %))))
      :else
      (let [{[dispatch-val-expr method-expr] :args :as expr}
            (-> expr
                (update :args #(-> %
                                   (update 0 check-expr)
                                   (update 1 (comp ana2/run-pre-passes ana2/analyze-outer-root)))))
            _ (assert (#{:var} (:op target)))
            _ (when-not (#{:fn} (:op method-expr))
                (err/int-error (str "Method must be a fn")))
            dispatch-type (mm/multimethod-dispatch-type mmsym)]
        (if-not dispatch-type
          (binding [vs/*current-env* env]
            (err/tc-delayed-error (str "Multimethod requires dispatch type: " mmsym
                                       "\n\nHint: defmulti must be checked before its defmethods")
                                  :return (-> expr
                                              (update-in [:args 1] ana2/run-passes))))
          (let [method-expected (var-env/type-of mmsym)
                cmethod-expr 
                (binding [multi-u/*current-mm* 
                          (when-not default?
                            {:dispatch-fn-type dispatch-type
                             :dispatch-val-ret (u/expr-type dispatch-val-expr)})]
                  (check-expr method-expr (r/ret method-expected)))]
            (-> expr
                (assoc-in [:args 1] cmethod-expr))))))))

(defmethod -invoke-special :default [expr expected])
(defmethod -host-call-special :default [expr expected])

;;TODO attach new :args etc.
;;convert apply to normal function application
(defmethod -invoke-apply :default [expr expected])

(defmethod -check :invoke
  [{fexpr :fn :keys [args env] :as expr} expected]
  {:post [(r/TCResult? (u/expr-type %))]}
  (invoke/check-invoke check-expr -invoke-special expr expected))

(defmacro prepare-check-fn [env expr & body]
  `(let [env# ~env
         expr# ~expr]
     (binding [vs/*current-env* (if (:line env#) env# vs/*current-env*)
               vs/*current-expr* expr#]
       ~@body)))

(defmethod -check :fn
  [{:keys [env] :as expr} expected]
  {:pre [((some-fn nil? r/TCResult?) expected)]
   :post [(-> % u/expr-type r/TCResult?)
          (or (not= :fn (:op %))
              (vector? (:methods %)))]}
  ;(prn "check :fn" expected)
  (or #_(when vs/*custom-expansions*
        ;; try to beta-expand
        (when-not (:local expr) ;; no recursive functions
          (when-let [args (::invoke-args expr)]
            (if expected
              ; expand ((ann-form (fn* [params*] body) [P* :-> R]) args*)
              ; to     (ann-form body[(ann-form args* P*)/params*] R)
              (let [[t :as ts] (fn-methods/function-types (:t expected))]
                (when (= 1 (count ts))
                  (let [[fin inst-frees bnds poly?] (cu/unwrap-poly (first ts))]
                    (when-not poly?
                      (when (r/FnIntersection? fin)
                        (when-let [matching-method (beta-reduce/find-matching-method expr (count args))]
                          (let [[ft :as relevant-fn-types] (keep #(fn-methods/expected-for-method matching-method % (:methods expr))
                                                                 (:types fin))]
                            (when (= 1 (count relevant-fn-types))
                              (let [{:keys [dom rng]} ft]
                                (when (not-any? #(% ft) [:drest :kws :prest :pdot])
                                  (assert ((if (:rest ft) <= =) (count dom) (count args)))
                                  (when-let [red (beta-reduce/maybe-beta-reduce-fn
                                                   expr
                                                   (mapv (fn [t a]
                                                           (binding [vs/*verbose-types* true]
                                                             (-> `(t/ann-form ~(ast-u/emit-form-fn a) ~(prs/unparse-type t))
                                                                 (ana2/analyze-form env)
                                                                 ana2/run-passes)))
                                                         (concat dom (repeat (:rest ft)))
                                                         args)
                                                   {:before-reduce ensure-within-beta-limit})]
                                    (let [cred (check-expr red (below/maybe-check-below
                                                                 ;; TODO subst arguments in object in result
                                                                 (r/Result->TCResult rng)
                                                                 (::invoke-expected expr)))]
                                      (set-erase-atoms expr cred)
                                      cred))))))))))))
              ; expand ((fn* [params*] body) args*)
              ; to     body[args*/params*]
              (when-let [red (beta-reduce/maybe-beta-reduce-fn expr args
                                                               {:before-reduce ensure-within-beta-limit})]
                (let [cred (check-expr red (::invoke-expected expr))]
                  (set-erase-atoms expr cred)
                  cred))))))
      (prepare-check-fn env expr
        (if (and expected
                 (not (r/wild? (r/ret-t expected))))
          (fn/check-fn expr expected)
          (special-fn/check-core-fn-no-expected expr)))))

;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form :clojure.core.typed/cast
  [{[_ _ {{tsyn :type} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (cast/check-cast check-expr expr expected))

(defmethod internal-special-form :clojure.core.typed/loop
  [{[_ _ {{tsyns :ann} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (special-loop/check-special-loop check-expr expr expected))

(defmethod internal-special-form :default
  [expr expected]
  (binding [vs/*current-expr* expr]
    (invoke-typing-rule (coerce/kw->symbol (u/internal-dispatch-val expr)) expr expected)))

(defmethod -check :do
  [expr expected]
  {:post [(r/TCResult? (u/expr-type %))
          (vector? (:statements %))]}
  (do/check-do check-expr internal-special-form expr expected))

(defmethod -check :monitor-enter
  [expr expected]
  (monitor/check-monitor check-expr expr expected))

(defmethod -check :monitor-exit
  [expr expected]
  (monitor/check-monitor check-expr expr expected))

(defmethod -check :local
  [expr expected]
  (local/check-local expr expected))

(defmethod -check :host-interop
  [expr expected]
  (host-interop/check-host-interop check-expr expr expected))

(defmethod -check :host-call
  [expr expected]
  (host-interop/check-host-call check-expr -host-call-special expr expected))

(defmethod -check :host-field
  [expr expected]
  (host-interop/check-host-interop check-expr expr expected))

(defmethod -check :maybe-host-form
  [expr expected]
  (host-interop/check-maybe-host-form check-expr expr expected))

(defmethod -check :maybe-class
  [expr expected]
  (let [expr (ana2/run-post-passes expr)]
    (if (= :maybe-class (:op expr))
      (err/tc-delayed-error (str "Unresolved host interop: " (:form expr)
                                 "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            :return (assoc expr u/expr-type (or expected r/-error)))
      (check-expr expr expected))))

(defmethod -invoke-special 'clojure.core/instance?
  [{[cls-expr :as args] :args :as expr} expected]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [((some-fn nil?
                    (comp r/TCResult? u/expr-type))
           %)]}
  (when-not (#{2} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/instance?,"
                        " expected 2, given " (count (:args expr)))))
  (when-let [cls (when (symbol? (:form cls-expr))
                   (let [cls (ana2/resolve-sym (:form cls-expr)
                                               (:env cls-expr))]
                     (when (class? cls)
                       cls)))]
    (let [{[cls-expr cexpr] :args :as expr}
          (-> expr
              (update :args #(vec (map check-expr % [(r/ret (c/RClass-of Class))
                                                     nil]))))
          inst-of (c/RClass-of-with-unknown-params cls)
          expr-tr (u/expr-type cexpr)]
      (assoc expr
             u/expr-type (below/maybe-check-below
                           (r/ret (c/Un r/-true r/-false)
                                  (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                          (fo/-not-filter-at inst-of (r/ret-o expr-tr))))
                           expected)))))

(defmethod -invoke-special 'clojure.core/satisfies?
  [{[cls-expr :as args] :args :as expr} expected]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [((some-fn nil?
                    (comp r/TCResult? u/expr-type))
           %)]}
  (when-not (#{2} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/satisfies?,"
                        " expected 2, given " (count (:args expr)))))
  (when-some [v (when (symbol? (:form cls-expr))
                  (let [v (ana2/resolve-sym (:form cls-expr)
                                            (:env cls-expr))]
                    (when (var? v)
                      v)))]
    (let [{[_ cexpr] :args :as expr}
          (-> expr
              (update-in [:args 1] check-expr))
          inst-of (c/Protocol-with-unknown-params (symbol v))
          expr-tr (u/expr-type cexpr)]
      (assoc expr
             u/expr-type (below/maybe-check-below
                           (r/ret (c/Un r/-true r/-false)
                                  (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                          (if (:extend-via-metadata @v)
                                            fl/-top ;; satisfies? does not rule out metadata extension https://clojure.atlassian.net/browse/CLJ-2426
                                            (fo/-not-filter-at inst-of (r/ret-o expr-tr)))))
                           expected)))))

(defmethod -check :instance?
  [{cls :class the-expr :target :as expr} expected]
  ;(assert nil ":instance? node not used")
  (let [inst-of (c/RClass-of-with-unknown-params cls)
        cexpr (check-expr the-expr)
        expr-tr (u/expr-type cexpr)]
    (assoc expr
           :target cexpr
           u/expr-type (below/maybe-check-below
                         (r/ret (c/Un r/-true r/-false)
                                (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                        (fo/-not-filter-at inst-of (r/ret-o expr-tr))))
                         expected))))

(defmulti -new-special (fn [{{:keys [form env] :as cls-expr} :class :as expr} expected]
                         {:pre [(#{:maybe-class} (:op cls-expr))]
                          :post [((some-fn nil? symbol?) %)]}
                         (let [cls (ana2/resolve-sym form
                                                     ; `new` ignores locals
                                                     (assoc env :locals {}))]
                           (when (class? cls)
                             (coerce/Class->symbol cls)))))

;; TODO share this logic with a macro rule for `clojure.core/defmulti`
(defmethod -new-special 'clojure.lang.MultiFn
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (== 4 (count (:args expr)))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn constructor"))
  (when-not expected
    (err/int-error "Expected type needed for defmulti"))
  (let [expected-t (r/ret-t expected)
        expected-d (multi/expected-dispatch-type expected-t)
        {[nme-expr cdispatch-expr default-expr hierarchy-expr] :args :as expr}
        (-> expr
            (update :class check-expr)
            ;name
            (update-in [:args 0] check-expr)
            ;dispatch-expr
            (update-in [:args 1] check-expr)
            ;default
            (update-in [:args 2] check-expr)
            ;hierarchy
            (update-in [:args 3] check-expr))
        _ (when-not (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
            (err/int-error "Multimethod hierarchy cannot be customised"))
        _ (when-not (= (:val default-expr) :default)
            (err/int-error "Non :default default dispatch value NYI"))
        mm-name (:val nme-expr)
        _ (when-not (string? mm-name)
            (err/int-error "MultiFn name must be a literal string"))
        inferred-dispatch-t (c/fully-resolve-type (r/ret-t (u/expr-type cdispatch-expr)))
        _ (when ((some-fn r/Union? r/Intersection?) inferred-dispatch-t)
            (err/nyi-error "defmulti dispatch function inferred as union or intersection"))
        ;_ (prn "inferred-dispatch-t" inferred-dispatch-t)
        ;_ (prn "expected-d" expected-d)
        resolved-dispatch-t (cond-> inferred-dispatch-t
                              (r/SymbolicClosure? inferred-dispatch-t) (-> (sub/check-symbolic-closure expected-d)
                                                                           u/expr-type
                                                                           r/ret-t))
        ;_ (prn "resolved-dispatch-t" resolved-dispatch-t)
        mm-qual (symbol (str (cu/expr-ns expr)) mm-name)
        _ (mm/add-multimethod-dispatch-type mm-qual resolved-dispatch-t)]
    (-> expr
        (assoc u/expr-type (below/maybe-check-below
                             (r/ret (c/In #_(c/RClass-of clojure.lang.MultiFn) 
                                          expected-t))
                             expected)))))

(defmethod -new-special :default [expr expected])

(defmethod -check :new
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  ;(prn ":new" (mapv (juxt :op :tag) (cons (:class expr) (:args expr))))
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (or (-new-special expr expected)
        (let [inst-types *inst-ctor-types*
              expr (-> expr
                       (update :class check-expr)
                       (update :args #(binding [*inst-ctor-types* nil]
                                        (mapv check-expr %)))
                       ;delegate eval to check-expr
                       ana2/run-post-passes)
              ;; call when we're convinced there's no way to rewrite this AST node
              ;; in a non-reflective way.
              give-up (fn [expr]
                        (let [clssym (-> expr
                                         ast-u/new-op-class 
                                         coerce/Class->symbol)]
                          (err/tc-delayed-error (str "Unresolved constructor invocation " 
                                                     (type-hints/suggest-type-hints 
                                                       nil 
                                                       nil 
                                                       (map (comp r/ret-t u/expr-type) (:args expr))
                                                       :constructor-call clssym)
                                                     ".\n\nHint: add type hints")
                                                :form (ast-u/emit-form-fn expr)
                                                :return (assoc expr
                                                               u/expr-type (cu/error-ret expected)))))
              ;; returns the function type for this constructor, or nil if
              ;; it is reflective.
              ctor-fn (fn [expr]
                        (when (:validated? expr)
                          (let [clssym (-> expr
                                           ast-u/new-op-class 
                                           coerce/Class->symbol)]
                            (or (ctor-override/get-constructor-override clssym)
                                (and (dt-env/get-datatype clssym)
                                     (cu/DataType-ctor-type clssym))
                                (when-let [ctor (cu/NewExpr->Ctor expr)]
                                  (cu/Constructor->Function ctor))))))
              ;; check a non-reflective constructor
              check-validated (fn [expr]
                                (let [ifn (-> (ctor-fn expr)
                                              (cond-> inst-types
                                                (inst/manual-inst inst-types {}))
                                              r/ret)
                                      ;_ (prn "Expected constructor" (prs/unparse-type (r/ret-t ifn)))
                                      res-type (funapp/check-funapp expr (:args expr) ifn (map u/expr-type (:args expr)) expected)]
                                  (assoc expr
                                         u/expr-type res-type)))]
          ;; try to rewrite, otherwise error on reflection
          (cond
            (:validated? expr) (check-validated expr)

            (cu/should-rewrite?) (let [expr (update expr :args #(mapv host-interop/add-type-hints %))
                                       rexpr (host-interop/try-resolve-reflection expr)]
                                   ;; rexpr can only be :new
                                   (case (:op rexpr)
                                     (:new) (if (:validated? rexpr)
                                              (check-validated rexpr)
                                              (give-up rexpr))))
            :else (give-up expr))))))

(defmethod -check :throw
  [expr expected]
  (throw/check-throw check-expr expr expected (r/ret (c/RClass-of Throwable))))

(defmethod -check :recur
  [{args :exprs :keys [env] :as expr} expected]
  {:post [(vector? (:exprs %))]}
  (recur/check-recur args env expr expected check-expr))

(defmethod -check :binding
  [{:keys [init] :as expr} expected]
  (binding/check-binding check-expr expr expected))

(defmethod -check :loop
  [{binding-inits :bindings :keys [body] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (loop/check-loop check-expr expr expected))

(defmethod -check :let
  [{bindings :bindings :keys [body] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let/check-let check-expr expr expected))

(defmethod -check :letfn
  [{bindings :bindings :keys [body] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (letfn/check-letfn bindings body expr expected check-expr))

(defmethod -check :with-meta
  [expr expected]
  (with-meta/check-with-meta check-expr expr expected))

(defmethod -check :if
  [{:keys [test then else] :as expr} expected]
  (if/check-if check-expr expr expected))

(defmethod -check :def
  [{:keys [var env] :as expr} expected]
  (let [prs-ns (cu/expr-ns expr)
        mvar (meta var)
        qsym (coerce/var->symbol var)]
    ; annotation side effect
    ;; TODO convert to type provider
    (when-let [[_ tsyn] (find mvar :ann)]
      (let [ann-type (binding [vs/*current-env* env
                               prs/*parse-type-in-ns* prs-ns]
                       (prs/parse-type tsyn))]
        (var-env/add-var-type qsym ann-type)))
    (when (:no-check mvar)
      (var-env/add-nocheck-var qsym))
    (def/check-def check-expr expr expected)))

(defmethod -check :deftype
  [expr expected]
  (deftype/check-deftype expr expected))

(defmethod -check :reify
  [expr expected]
  (reify/check-reify expr expected))

(defmethod -check :import
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-nil)
                       expected)))

(defmethod -check :case-test
  [{:keys [test] :as expr} expected]
  (let [ctest (check-expr test expected)]
    (assoc expr
           :test ctest
           u/expr-type (u/expr-type ctest))))

(defmethod -check :case
  [{target :test :keys [tests thens default] :as expr} expected]
  {:post [((every-pred vector?
                       (con/every-c? (every-pred
                                       (comp #{:case-test} :op)
                                       :test)))
           (:tests %))
          ((every-pred vector?
                       (con/every-c? (every-pred
                                       (comp #{:case-then} :op)
                                       :then)))
           (:thens %))
          (-> % u/expr-type r/TCResult?)]}
  ; tests have no duplicates
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [ctarget (check-expr target)
          target-ret (u/expr-type ctarget)
          _ (assert (r/TCResult? target-ret))
          ctests (mapv check-expr tests)
          tests-rets (map u/expr-type ctests)
          ; Can we derive extra information from 'failed'
          ; tests? Delegate to check-case-thens for future enhancements.
          cthens (case/check-case-thens check-expr target-ret tests-rets thens expected)
          cdefault (let [flag+ (volatile! true)
                         neg-tst-fl (let [val-ts (map (comp c/fully-resolve-type r/ret-t) tests-rets)]
                                      (if (every? r/Value? val-ts)
                                        (fo/-not-filter-at (apply c/Un val-ts)
                                                           (r/ret-o target-ret))
                                        fl/-top))
                         env-default (update/env+ (lex/lexical-env) [neg-tst-fl] flag+)
                         _ (when-not @flag+
                             ;; FIXME should we ignore this branch?
                             (u/tc-warning "Local became bottom when checking case default"))]
                     ;(prn "neg-tst-fl" neg-tst-fl)
                     ;(prn "env-default" env-default)
                     (var-env/with-lexical-env env-default
                       (check-expr default expected)))
          ;; FIXME this is a duplicated expected test, already done able
          case-result (let [type (apply c/Un (map (comp :t u/expr-type) (cons cdefault cthens)))
                            ; TODO
                            filter (fo/-FS fl/-top fl/-top)
                            ; TODO
                            object obj/-empty]
                        (below/maybe-check-below
                          (r/ret type filter object)
                          expected))]
      (assoc expr
             :test ctarget
             :tests ctests
             :thens cthens
             :default cdefault
             u/expr-type case-result))))

(defmethod -check :catch
  [expr expected]
  (catch/check-catch check-expr expr expected))

(defmethod -check :try
  [expr expected]
  {:post [(vector? (:catches %))
          (-> % u/expr-type r/TCResult?)]}
  (try/check-try check-expr expr expected))

(defmethod -check :set!
  [expr expected]
  (set!/check-set! check-expr expr expected))

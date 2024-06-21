;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.analyze-clj
  (:refer-clojure :exclude [macroexpand-1 get-method eval delay])
  (:require [clojure.core :as core]
            [typed.clojure :as t]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-utils-platform-specific :as plat-con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.expand :as expand]
            [clojure.core.typed.rules :as rules]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.util-vars :as vs]
            [clojure.pprint :as pp]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [typed.clj.analyzer :as jana2]
            [typed.clj.analyzer :as taj]
            [typed.clj.analyzer.passes.beta-reduce :as beta-reduce]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.clj.analyzer.passes.validate :as validate]
            [typed.clj.analyzer.utils :as taj-utils]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as env]
            [typed.cljc.analyzer.passes :as passes]
            [typed.cljc.analyzer.utils :as ta-utils]
            [typed.cljc.checker.utils :as u]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])
  (:import [clojure.lang Compiler$LocalBinding]))

; Updated for Clojure 1.8
;  https://github.com/clojure/clojure/commit/7f79ac9ee85fe305e4d9cbb76badf3a8bad24ea0
(t/ann ^:no-check typed-macros (t/Map t/Any t/Any))
(def typed-macros
  {;; add positional information for destructured bindings
   #'clojure.core/loop
   (fn [&form &env bindings & body]
     (@#'clojure.core/assert-args
       (vector? bindings) "a vector for its binding"
       (even? (count bindings)) "an even number of forms in binding vector")
     (let [db (destructure bindings)]
       (if (= db bindings)
         `(loop* ~bindings ~@body)
         (let [vs (take-nth 2 (drop 1 bindings))
               bs (take-nth 2 bindings)
               gs (map (fn [b] (if (symbol? b) 
                                 b 
                                 ;; preserve positional metadata
                                 (with-meta 
                                   (gensym) 
                                   (meta b))))
                       bs)
               bfs (reduce (fn [ret [b v g]]
                             (if (symbol? b)
                               (conj ret g v)
                               (conj ret g v b g)))
                           [] (map vector bs vs gs))]
           `(let ~bfs
              (loop* ~(vec (interleave gs gs))
                     (let ~(vec (interleave bs gs))
                       ~@body)))))))})

(defn custom-expansion-opts [env opts]
  (let [analyze (fn [form & [env1]]
                  (ana2/run-passes (ana2/analyze-form form (or env1 env)) opts))]
    {:internal-error (fn [s & [aopts]]
                       ;; TODO aopts (line numbers, blame form etc.)
                       (let [;; can't access check.utils ns from here (circular deps)
                             #_#_aopts (update aopts :expected cu/maybe-map->TCResult)]
                         (err/int-error s aopts opts)))
     :splice-seqable-form (fn [form & [env1]]
                            (some->> (beta-reduce/splice-seqable-expr (analyze form (or env1 env)))
                                     (mapv (fn [e]
                                             (let [form (emit-form/emit-form (:expr e) opts)]
                                               (-> e
                                                   (dissoc :expr)
                                                   (assoc :form form)))))))
     :analyze-env env
     :analyze analyze
     :emit-form #(emit-form/emit-form % opts)}))

(t/ann ^:no-check typed-macro-lookup [t/Any t/Any t/Any :-> t/Any])
(defn typed-macro-lookup [var env {::vs/keys [custom-expansions] :as opts}]
  {:post [(ifn? %)]}
  (or (when custom-expansions
        (let [vsym (coerce/var->symbol var)]
          (when (expand/custom-expansion? vsym)
            (fn [form locals & _args_]
              (expand/expand-macro form 
                                   (merge (custom-expansion-opts env opts)
                                          {:vsym vsym
                                           :locals locals}))))))
      (get typed-macros var)
      var))

;; copied from tools.analyzer.jvm to insert `*typed-macros*`
(t/ann ^:no-check macroexpand-1 [t/Any (t/Map t/Any t/Any) -> t/Any])
(defn macroexpand-1
  "If form represents a macro form or an inlinable function, returns its expansion,
   else returns form."
  ;([form] (macroexpand-1 form (taj/empty-env)))
  ([form env {::vs/keys [custom-expansions] :as opts}]
   ;(prn "macroexpand-1" form)
   (env/ensure (jana2/global-env)
     (cond
       (seq? form)
       (let [[op & args] form]
         (if (taj/specials op)
           form
           (let [v (ana2/resolve-sym op env)
                 m (meta v)
                 local? (-> env :locals (get op))
                 macro? (and (not local?) (:macro m)) ;; locals shadow macros
                 inline-arities-f (:inline-arities m)
                 ;; disable :inline with custom expansions to avoid arity errors
                 ;; in symbolic execution.
                 inline? (if custom-expansions
                           (when (and (not local?) (var? v))
                             (let [vsym (coerce/var->symbol v)]
                               (when (expand/custom-inline? vsym)
                                 (fn [& _args_]
                                   (expand/expand-inline form
                                                         (merge (custom-expansion-opts env opts)
                                                                {:vsym vsym}))))))
                           (and (not local?)
                                (or (not inline-arities-f)
                                    (inline-arities-f (count args)))
                                (:inline m)))
                 t (:tag m)]
             (cond

               macro?
               (let [locals (cond->> (:locals env)
                              ;; fake Compiler.java's locals so tools.analyzer doesn't think there's a recursive go macro,
                              ;; and because typed.clj.analyzer AST's are incompatible with tools.analyzer's.
                              ('#{clojure.core.async/go
                                  typed.lib.clojure.core.async/go}
                                (symbol v))
                              (into {}
                                    (map (fn [[sym e]]
                                           {:pre [(symbol? sym)]}
                                           (let [tag (-> e :tag)]
                                             [sym (Compiler$LocalBinding.
                                                    0
                                                    sym
                                                    tag
                                                    nil ;; init
                                                    false ;; isArg
                                                    nil ;; clearPathRoot
                                                    )])))))
                     res (apply (typed-macro-lookup v env opts) form locals (rest form))] ; (m &form &env & args)
                 (if (ta-utils/obj? res)
                   (vary-meta res merge (meta form))
                   res))

               inline?
               (let [res (apply inline? args)]
                 (if (ta-utils/obj? res)
                   (vary-meta res merge
                              (and t {:tag t})
                              (meta form))
                   res))

               :else
               (jana2/desugar-host-expr form env)))))

       (symbol? form)
       (jana2/desugar-symbol form env opts)

       :else
       form))))

(t/ann ^:no-check special-form? [t/Any :-> t/Any])
(defn special-form? [mform]
  (and (seq? mform)
       (= 'do (first mform))
       (or (= (second mform) spec/special-form)
           (= (second mform) :clojure.core.typed/special-collect))))

(declare eval-ast)
;; reflect-validated from eastwood
;========================
(t/ann ^:no-check reflect-validated [(t/Map t/Any t/Any) :-> t/Any])
(defmulti reflect-validated 
  {:pass-info {:walk :any :depends #{#'validate/validate}}}
  :op)

(t/ann ^:no-check arg-type-str [(t/Seqable (t/U nil Class)) :-> t/Str])
(defn arg-type-str [arg-types]
  (str/join ", "
            (map #(if (nil? %) "nil" (.getName ^Class %)) arg-types)))

(defn get-ctor [ast]
  (let [cls (:val (:class ast))
        arg-type-vec (mapv :tag (:args ast))
        arg-type-arr (into-array Class arg-type-vec)]
;;    (println (format "dbgx: get-ctor cls=%s arg-types=%s"
;;                     cls (arg-type-str arg-type-vec)))
    (try
      (.getConstructor ^Class cls arg-type-arr)
      (catch NoSuchMethodException e
        (try
          (.getDeclaredConstructor ^Class cls arg-type-arr)
          (catch NoSuchMethodException e
            {:class cls, :arg-types arg-type-vec}))))))

(defn get-field [ast]
  (let [cls (:class ast)
        fld-name (name (:field ast))]
    (try
      (.getField ^Class cls fld-name)
      (catch NoSuchFieldException e
        (try
          (.getDeclaredField ^Class cls fld-name)
          (catch NoSuchFieldException e
            {:class cls, :field-name fld-name}))))))

(defn get-method [ast]
  (let [cls (:class ast)
        method-name (name (:method ast))
        arg-type-vec (mapv :tag (:args ast))
        arg-type-arr (into-array Class arg-type-vec)]
;;    (println (format "dbgx: get-method cls=%s method=%s arg-types=%s"
;;                     cls method-name (arg-type-str arg-type-vec)))
    (when (some nil? arg-type-vec)
      (println (format "Error: Bad arg-type nil for method named %s for class %s, full arg type list (%s).  ast pprinted below for debugging tools.analyzer:"
                       method-name
                       (.getName ^Class cls)
                       (arg-type-str arg-type-vec)))
      #_(util/pprint-ast-node ast))
    (try
      (.getMethod ^Class cls method-name arg-type-arr)
      (catch NoSuchMethodException e
        (try
          (.getDeclaredMethod ^Class cls method-name arg-type-arr)
          (catch NoSuchMethodException e
            {:class cls, :method-name method-name,
             :arg-types arg-type-vec}))))))


(defn void-method? [^java.lang.reflect.Method m]
  (let [ret-val (.getGenericReturnType m)]
    (= ret-val Void/TYPE)))

(defmethod reflect-validated :default [ast] ast)

(defmethod reflect-validated :new [ast]
  (if (:validated? ast)
    (assoc ast :reflected-ctor (@#'reflect/constructor->map (get-ctor ast)))
    ast))

(defmethod reflect-validated :instance-field [ast]
  (assoc ast :reflected-field (@#'reflect/field->map (get-field ast))))

(defmethod reflect-validated :instance-call [ast]
  (if (:validated? ast)
    (assoc ast :reflected-method (@#'reflect/method->map (get-method ast)))
    ast))

(defmethod reflect-validated :static-field [ast]
  (assoc ast :reflected-field (@#'reflect/field->map (get-field ast))))

(defmethod reflect-validated :static-call [ast]
  (if (:validated? ast)
    (assoc ast :reflected-method (@#'reflect/method->map (get-method ast)))
    ast))
;========================

(declare scheduled-passes-for-custom-expansions)

;; (All [x ...] [-> '{(Var x) x ...})])
(defn thread-bindings [opt {::vs/keys [check-config delayed-errors custom-expansions] :as opts}]
  (let [ns (the-ns (or (-> opt :env :ns)
                       *ns*))
        side-effects? (case (:check-form-eval check-config)
                        (:never :before) false
                        (:after nil) true)
        eval-ast (if side-effects? jana2/eval-ast2 (fn [ast _] ast))]
    (-> (jana2/default-thread-bindings {:ns (ns-name ns)})
        (cond->
          ;; reify* also imports a class name, but it's gensym'd.
          (not side-effects?) (assoc #'jana2/*parse-deftype-with-existing-class* true
                                     #'ana2/create-var (fn [sym {:keys [ns]}]
                                                         (or (find-var
                                                               (symbol (-> ns ns-name name)
                                                                       (name sym)))
                                                             (err/int-error
                                                               (format "Could not find var %s in namespace %s"
                                                                       sym (ns-name ns))
                                                               opts)))))
        (assoc #'ana2/eval-ast (fn [ast opts]
                                 (let [; don't evaluate a form if there are delayed type errors
                                       throw-this (atom nil)
                                       _ (swap! delayed-errors
                                                (fn [delayed]
                                                  {:pre [(vector? delayed)]
                                                   :post [(vector? %)]}
                                                  (if (seq delayed)
                                                    ; take the last type error to throw
                                                    (do (reset! throw-this (peek delayed))
                                                        (pop delayed))
                                                    delayed)))
                                       _ (when-some [e @throw-this]
                                           (throw e))
                                       ]
                                   (eval-ast ast opts)))
               #'ana2/macroexpand-1 macroexpand-1
               #'ana2/scheduled-passes (if custom-expansions
                                         @scheduled-passes-for-custom-expansions
                                         @jana2/scheduled-default-passes)))))

(defn will-custom-expand? [form env {::vs/keys [custom-expansions] :as opts}]
  (boolean
    (when custom-expansions
      (when (seq? form)
        (let [[op & args] form]
          (when-not (taj/specials op)
            (let [v (ana2/resolve-sym op env)
                  m (meta v)
                  local? (-> env :locals (get op))
                  macro? (and (not local?) (:macro m)) ;; locals shadow macros
                  vsym (when (var? v)
                         (coerce/var->symbol v))]
              (or (when (and (not local?) vsym)
                    (expand/custom-inline? vsym))
                  (when (and macro? vsym)
                    (expand/custom-expansion? vsym))))))))))

(comment
  (clojure.pprint/pprint
    (passes/schedule (conj jana2/default-passes
                          #'beta-reduce/push-invoke
                          )
                    {:debug? true}))
  )

(def scheduled-passes-for-custom-expansions
  (delay
    (passes/schedule (conj jana2/default-passes
                          ;#'beta-reduce/push-invoke
                          ))))

;; bindings is an atom that records any side effects during macroexpansion. Useful
;; for nREPL middleware.
(defn analyze1 [form env {:keys [bindings-atom analyze-bindings-fn] :as opt} opts]
  {:pre [((some-fn nil? plat-con/atom?) bindings-atom)
         ((some-fn nil? ifn?) analyze-bindings-fn)]}
  (u/trace (str "Analyze1 form " *file* " " form)
           opts)
  (let [old-bindings (or (some-> bindings-atom deref) {})
        analyze-fn (fn [form env opt]
                     (let [env (assoc env :ns (ns-name *ns*))
                           opt (-> opt
                                   (dissoc :bindings-atom)
                                   (assoc-in [:bindings #'*ns*] *ns*))]
                       (jana2/analyze form env (into opts opt))))]
    (with-bindings old-bindings
      ;(prn "analyze1 namespace" *ns*)
      (let [ana (jana2/analyze+eval 
                  form (or env (taj/empty-env))
                  (->
                    (merge-with merge (into opts opt)
                                {:bindings (if analyze-bindings-fn
                                             (analyze-bindings-fn)
                                             (thread-bindings opt opts))
                                 :special-form? special-form?})
                    (assoc
                      ;; if this is a typed special form like an ann-form, don't treat like
                      ;; a top-level do.
                      :additional-gilardi-condition 
                      (fn [mform env]
                        (not (special-form? mform)))
                      :stop-gildardi-check 
                      ;; cut off custom expansions to preserve :original-form's
                      (fn [mform env]
                        (will-custom-expand? mform env opts))
                      ;; propagate inner types to outer `do`
                      :annotate-do
                      (fn [a _ ret]
                        ;; could be nil if ret is unanalyzed
                        (assoc a u/expr-type (u/expr-type ret)))
                      ;; don't propagate expected type to `do` statements
                      :statement-opts-fn
                      (fn [opt]
                        (dissoc opt :expected))
                      :analyze-fn analyze-fn)))]
        ;; only record vars that were already bound
        (when bindings-atom
          (reset! bindings-atom (select-keys (get-thread-bindings) (keys old-bindings))))
        ana))))

#_
(defn ast-for-form
  "Returns an AST node for the form"
  ([form] (ast-for-form form {}))
  ([form opt]
   (analyze1 form (taj/empty-env) opt)))

; eval might already be monkey-patched, eval' avoids infinite looping
(defn eval' [frm]
  (. clojure.lang.Compiler (eval frm)))

(defn eval-ast [ast {::vs/keys [custom-expansions] :as opts}]
  ;; based on jvm/analyze+eval
  (let [; FIXME don't allow mixing of runtime inference and custom expansions,
        ; since we want to evaluate the modified AST in runtime inference.
        frm (if custom-expansions
              (:original-form opts)
              (emit-form/emit-form ast opts))
        ;_ (prn "form" frm)
        #_#_
        _ (binding [;*print-meta* true
                    ;*print-dup* true
                    ;*print-length* 6
                    ;*print-level* 6
                    ]
            (prn "form")
            (pp/pprint frm))
        ;; TODO support `handle-evaluation-exception`
        result (eval' frm)]  ;; eval the emitted form rather than directly the form to avoid double macroexpansion
    (merge ast {:result result})))


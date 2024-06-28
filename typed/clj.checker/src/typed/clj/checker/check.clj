;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.check
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.clojure :as t]
            [typed.cljc.checker.check.cache :as cache]
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
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
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
            [typed.cljc.checker.check-impl :refer [-check]]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.apply :as apply]
            [typed.cljc.checker.check.case :as case]
            [typed.cljc.checker.check.cli :as cli]
            [typed.cljc.checker.check.def :as def]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.get :as get]
            [typed.cljc.checker.check.invoke :as invoke]
            [typed.cljc.checker.check.invoke-kw :as invoke-kw]
            [typed.cljc.checker.check.isa :as isa]
            [typed.cljc.checker.check.meta-ann :as meta-ann]
            [typed.cljc.checker.check.monitor :as monitor]
            [typed.cljc.checker.check.multi :as multi]
            [typed.cljc.checker.check.multi-utils :as-alias multi-u]
            [typed.cljc.checker.check.nth :as nth]
            [typed.cljc.checker.check.nthnext :as nthnext]
            [typed.cljc.checker.check.print-env :as print-env]
            [typed.cljc.checker.check.special.cast :as cast]
            [typed.cljc.checker.check.special.loop :as special-loop]
            [typed.cljc.checker.check.unanalyzed :as unanalyzed]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.check.with-meta :as with-meta]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.cs-rep :as crep]
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
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.runtime.env :as cenv]
            [typed.cljc.runtime.env-utils :as env-utils])
  (:import (clojure.lang IPersistentMap Var)))

;==========================================================
; # Type Checker
;
; The type checker is implemented here.

(declare check-top-level)

(defn ignored-macro-call? [form env opts]
  (and #_(seq? form)
       (when-some [^Var v (ana2/resolve-sym (first form) env opts)]
         (and (.isMacro v)
              (-> v meta ::t/ignore)))))

(def defn-vars `#{defn defn-})
(def defn-var-names (into #{} (map name) defn-vars))

(def ignored-def-vars `#{defmacro declare})
(def ignored-def-var-names (into #{} (map name) ignored-def-vars))

(defn ignored-def? [form env opts]
  (and #_(seq? form)
       (let [[sym vsym] form]
         (and (symbol? sym)
              (symbol? vsym)
              (let [nme (name sym)]
                (if (ignored-def-var-names nme)
                  (ignored-def-vars (ana2/var->sym (ana2/resolve-sym sym env opts) opts))
                  ;;TODO needs to interact with the cache so a check is forced when ^:no-check removed
                  (and (or (= 'def sym)
                           (and (defn-var-names nme)
                                (defn-vars (ana2/var->sym (ana2/resolve-sym sym env opts) opts))))
                       (when-some [^Var v (ana2/resolve-sym vsym env opts)]
                         (when (var? v)
                           (or (and (.isMacro v)
                                    (-> v meta ::t/ignore))
                               (let [qvsym (symbol v)]
                                 (and (var-env/lookup-Var-nofail qvsym opts)
                                      (not (var-env/check-var? (cenv/checker opts) qvsym))))))))))))))

(defn skip-form? [form env opts]
  (and (seq? form)
       (or (-> form meta ::t/ignore)
           (ignored-macro-call? form env opts)
           (ignored-def? form env opts))))

(defn check-ns1
  "Type checks an entire namespace."
  ([ns env {::vs/keys [delayed-errors check-config
                       ^java.util.concurrent.ExecutorService check-threadpool]
            ::cenv/keys [checker] :as opts}]
   (let [opts (env/ensure opts (jana2/global-env))
         env (or env (jana2/empty-env ns))
         ^java.net.URL res (jtau/ns-url ns)
         _ (assert res (str "Can't find " ns " in classpath"))
         slurped (slurp (io/reader res))]
     (when-not (cache/ns-check-cached? checker ns slurped)
       (let [filename (str res)
             {:keys [check-form-eval]} check-config]
         (binding [*ns*   (if (= :never check-form-eval)
                            (the-ns ns) ;; assumes ns == clojure.core/ns and ns is the same throughout file
                            *ns*)
                   *file* filename]
           (let [forms-info (with-open [rdr (io/reader res)]
                              (let [pbr (readers/source-logging-push-back-reader
                                          (java.io.PushbackReader. rdr) 1 filename)
                                    eof (Object.)
                                    read-opts (cond-> {:eof eof :features #{:clj}}
                                                (.endsWith filename "cljc") (assoc :read-cond :allow))]
                                (loop [ns-form-str nil
                                       forms-info []]
                                  (let [[form sform] (reader/read+string read-opts pbr)]
                                    (if (identical? form eof)
                                      forms-info
                                      (recur (or ns-form-str
                                                 (when (and (seq? form) (= 'ns (first form)))
                                                   sform))
                                             (conj forms-info {:ns-form-str ns-form-str :sform sform :form form})))))))

                 ns-form-str (some :ns-form-str forms-info)
                 _ (assert delayed-errors)
                 bndings {#'*ns* *ns*}
                 exs (map (fn [{:keys [form sform]}]
                            (fn []
                              (let [delayed-errors (err/-init-delayed-errors)
                                    opts (-> opts
                                             (assoc ::vs/delayed-errors delayed-errors)
                                             ;; force types to reparse to detect dependencies in per-form cache
                                             ;; might affect TypeFn variance inference
                                             (assoc ::env-utils/type-cache (atom {})))
                                    ex (volatile! nil)
                                    chk (fn []
                                          (try (check-top-level form nil {:env (assoc env :ns (ns-name *ns*))
                                                                          :top-level-form-string sform
                                                                          :ns-form-string ns-form-str}
                                                                opts)
                                               (catch Throwable e (vreset! ex e))))
                                    out (with-bindings bndings
                                          (if check-threadpool
                                            (with-out-str
                                              (chk))
                                            (do (chk) nil)))]
                                (-> (if-let [ex @ex]
                                      (if (-> ex ex-data :type-error)
                                        {:errors (conj @delayed-errors ex)}
                                        {:ex ex})
                                      {:errors @delayed-errors})
                                    (assoc :out out)))))
                          forms-info)
                 results (if check-threadpool
                           (mapv (fn [^java.util.concurrent.Future future]
                                   (try (.get future)
                                        (catch java.util.concurrent.ExecutionException e
                                          (throw (or (.getCause e) e)))))
                                 (.invokeAll check-threadpool (or (seq exs) ())))
                           (mapv #(%) exs))
                 _ (swap! delayed-errors into
                          (into [] (mapcat (fn [{:keys [ex errors out]}]
                                             (some-> out str/trim not-empty println)
                                             (some-> ex throw)
                                             errors))
                                results))]
             (cache/remove-stale-cache-entries ns ns-form-str (map :sform forms-info) slurped opts))))))))

(defn check-ns-and-deps [nsym opts] (cu/check-ns-and-deps nsym check-ns1 opts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(def ^:private *register-exts (delay (configs/register-clj-config-exts)))

(defn maybe-check-inlineable [{:keys [op form env] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(= :unanalyzed op)]}
  (when (seq? form)
    (let [v (-> (first form)
                (ana2/resolve-sym env opts))]
      (when (var? v)
        (let [m (meta v)]
          (when (and (:inline m)
                     (let [inline-arities-f (:inline-arities m)]
                       (or (not inline-arities-f)
                           (inline-arities-f (count (rest form))))))
            ;; TODO unit test (lack of) double expand/eval
            (let [expr-noinline 
                  ;; could pull out the ana2/unmark-top-level to here.
                  ;; probably would avoid the need for ana2/unmark-eval-top-level.
                  (ana2/analyze-outer expr (assoc opts ::ana2/macroexpand-1 (fn [form _ _] form)))]
              (when (= :invoke (:op expr-noinline))
                (let [{cargs :args
                       res u/expr-type} (-> expr-noinline
                                            ;; defer eval to inlining -- would rather an bad type check than a botched eval
                                            ana2/unmark-top-level
                                            ana2/unmark-eval-top-level
                                            (check-expr expected opts))]
                  (-> expr
                      (assoc :form (with-meta (cons (first form)
                                                    ;; technically we just need the :tag of these
                                                    ;; args, could infer from checked expr-noinline.
                                                    (map #(emit-form/emit-form % opts) cargs))
                                              (meta form)))
                      (ana2/analyze-outer-root opts)
                      (ana2/run-passes opts)
                      (assoc u/expr-type res)))))))))))



(defn should-infer-vars? [expr opts]
  (-> (cu/expr-ns expr opts)
      find-ns
      meta
      :core.typed
      :experimental
      (contains? :infer-vars)))

(defn check-var [{:keys [var] :as expr} expected {::vs/keys [check-config] :as opts}]
  {:pre [(var? var)]}
  (let [opts (assoc opts ::vs/current-expr expr)
        checker (cenv/checker opts)
        id (coerce/var->symbol var)
        _ (when-not (var-env/used-var? checker id)
            (var-env/add-used-var checker id))
        vsym id
        ut (var-env/get-untyped-var checker (cu/expr-ns expr opts) vsym opts)
        t (var-env/lookup-Var-nofail vsym opts)]
    ;(prn " annotation" t)
    ;(prn " untyped annotation" ut)
    (cond
      ;; we have an untyped annotation
      ut
      (if (cu/should-rewrite? opts)
        (assoc (cu/add-cast expr ut
                            {:positive (str "Annotation for " vsym)
                             :negative (str (cu/expr-ns expr opts))}
                            opts)
               u/expr-type (below/maybe-check-below
                             (r/ret ut)
                             expected
                             opts))
        (err/tc-delayed-error
          (str "Untyped var " id " found, but unable to rewrite to add contract")
          {:return (assoc expr
                          u/expr-type (cu/error-ret expected))}
          opts))

      ;; we have a typed annotation
      t
      (assoc expr
             u/expr-type (below/maybe-check-below
                           (r/ret t)
                           expected
                           opts))

      ;; :infer-vars are enabled for this namespace, this
      ;; var dereference is the dynamic type
      (or (should-infer-vars? expr opts)
          (impl/impl-case opts
            :clojure (= :unchecked (:unannotated-var check-config))
            :cljs nil))
      (do
        (println (str "Inferring " vsym " dereference as Unchecked"))
        (assoc expr
               u/expr-type (below/maybe-check-below
                             (r/ret (r/-unchecked vsym))
                             expected
                             opts)))
      (impl/impl-case opts
        :clojure (= :any (:unannotated-var check-config))
        :cljs nil)
      (do
        (println (str "Inferring " vsym " dereference as Any"))
        (assoc expr
               u/expr-type (below/maybe-check-below
                             (r/ret r/-any)
                             expected
                             opts)))


      :else
      (err/tc-delayed-error
        (str "Unannotated var " id)
        {:return (assoc expr
                        u/expr-type (cu/error-ret expected))}
        opts))))

(defn set-erase-atoms [expr cred]
  {:pre [(u/expr-type cred)]}
  (let [_ (some-> expr ::with-meta/erase-atom (reset! true))
        _ (some-> expr ::replace-invoke-atom (reset! cred))]
    nil))

(defn check-the-var
  [{:keys [^Var var env] :as expr} expected opts]
  {:pre [(var? var)]}
  (impl/assert-clojure opts)
  (let [checker (cenv/checker opts)
        id (coerce/var->symbol var)
        macro? (.isMacro var)
        _ (when-not (or macro?
                        (var-env/used-var? checker id))
            (var-env/add-used-var checker id))
        t (var-env/lookup-Var-nofail id opts)
        t (cond
            t t
            macro? r/-any
            ;; :infer-vars are enabled for this namespace, this
            ;; var object is the dynamic type
            (should-infer-vars? expr opts) (r/-unchecked id)
            :else (err/tc-delayed-error (str "Unannotated var reference: " id)
                                        {:form (ast-u/emit-form-fn expr opts)
                                         :return (r/TCError-maker)}
                                        opts))]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret (c/-name `t/Var t)
                                (fo/-true-filter))
                         expected
                         (assoc opts ::vs/current-expr expr)))))

(defmulti -invoke-special (fn [{{:keys [form env] :as fexpr} :fn :as expr} expected opts]
                            {:pre [(= :invoke (:op expr))
                                   (= :unanalyzed (:op fexpr))]
                             :post [((some-fn nil? symbol?) %)]}
                            (-> form
                                (ana2/resolve-sym env opts)
                                (ana2/var->sym opts))))

(defmulti -invoke-apply (fn [{[{:keys [op form env] :as fexpr} :as args] :args :as expr} expected opts]
                          {:pre [(= :invoke (:op expr))]
                           :post [((some-fn nil? symbol?) %)]}
                          (when (seq args)
                            (assert (= :unanalyzed op))
                            (-> form
                                (ana2/resolve-sym env opts)
                                (ana2/var->sym opts)))))

(defn host-call-qname [{:keys [target] :as expr} _expected opts]
  {:pre [(= :host-call (:op expr))
         (= :unanalyzed (:op target))]
   :post [((some-fn nil?
                    (con/hvector-c? #{:static-call
                                      :instance-call}
                                    symbol?))
           %)]}
  (when ((some-fn symbol? class?) (:form target))
    (let [target (ana2/run-passes target opts)]
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
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when (= 2 (count args))
    (let [cargs (mapv #(check-expr % nil opts) args)
          ct (-> (first cargs) u/expr-type r/ret-t (c/fully-resolve-type opts))]
      (when (and (r/Value? ct) (class? (:val ct)))
        (let [v-t (-> (check-expr (second args) nil opts) u/expr-type r/ret-t)
              t (c/In [v-t (c/Un [r/-nil (c/RClass-of-with-unknown-params (:val ct) opts)]
                                 opts)]
                      opts)]
          (-> expr
              (update :fn check-expr nil opts)
              (assoc :args cargs
                     u/expr-type (below/maybe-check-below
                                   (r/ret t)
                                   expected
                                   opts))))))))

(defmethod -invoke-special 'clojure.core.typed/var>*
  [expr expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when-not (= 1 (count (:args expr)))
    (err/int-error (str "Wrong number of arguments to clojure.core.typed/var>,"
                        " expected 1, given " (count (:args expr)))
                   opts))
  (let [{[sym-expr :as args] :args fexpr :fn :as expr}
        (-> expr
            (update-in [:args 0] ana2/run-passes opts))
        sym (ast-u/quote-expr-val sym-expr)
        _ (assert (symbol? sym))
        t (var-env/lookup-Var-nofail sym opts)
        _ (when-not t
            (err/tc-delayed-error (str "Unannotated var: " sym) opts))]
    (-> expr
        ; var>* is internal, don't check
        #_(update :fn check-expr nil opts)
        (assoc u/expr-type (below/maybe-check-below
                             (r/ret (or t (r/TCError-maker)))
                             expected
                             opts)))))

; ignore some keyword argument related intersections
(defmethod -invoke-special 'clojure.core/seq?
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when (= 1 (count args))
    (let [{[ctarget] :args :as cexpr}
          (-> expr
              (update :fn check-expr nil opts)
              (update :args #(mapv (fn [t] (check-expr t nil opts)) %)))
          targett (-> ctarget u/expr-type r/ret-t (c/fully-resolve-type opts))]
      (cond 
        ; records never extend ISeq
        ;; TODO move this to subtyping
        (r/Record? targett)
        (assoc cexpr
               u/expr-type (below/maybe-check-below
                             (r/ret r/-false (fo/-false-filter))
                             expected
                             opts))))))

(defmethod -invoke-special 'clojure.core/extend
  [expr expected {::check/keys [check-expr] :as opts}]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not ((every-pred odd? pos?) (count (:args expr)))
    (err/int-error (str "Wrong number of arguments to extend, expected at least one with an even "
                        "number of variable arguments, given " (count (:args expr)))
                   opts))
  (let [checker (cenv/checker opts)
        {[catype & protos :as args] :args :as expr}
        (-> expr
            ;atype
            (update-in [:args 0] check-expr nil opts))
        expr (-> expr
                 ; don't check extend
                 ;(update :fn check-expr nil opts)
                 (assoc u/expr-type (below/maybe-check-below
                                      (r/ret r/-nil (fo/-false-filter))
                                      expected
                                      opts)))
        ; this is a Value type containing a java.lang.Class instance representing
        ; the type extending the protocol, or (Value nil) if extending to nil
        target-literal-class (r/ret-t (u/expr-type catype))]
    (cond
      (not (and (r/Value? target-literal-class)
                ((some-fn class? nil?) (:val target-literal-class))))
      (err/tc-delayed-error
        (str "Must provide a Class or nil as first argument to extend, "
             "got " (pr-str (prs/unparse-type target-literal-class opts)))
        {:return expr}
        opts)

      (and expected (not (sub/subtype? r/-any (r/ret-t expected) opts)))
      (do (cu/expected-error r/-any expected opts)
          expr)
      :else
      (let [; this is the actual core.typed type of the thing extending the protocol
            target-type (let [v (:val target-literal-class)]
                          (if (nil? v)
                            r/-nil
                            (c/RClass-of-with-unknown-params v opts)))
            ; build expected types for each method map
            extends (for [[prcl-expr mmap-expr] (partition 2 protos)]
                      (let [prcl-expr (-> prcl-expr (ana2/analyze-outer-root opts) (ana2/run-pre-passes opts))
                            protocol (do (when-not (= :var (:op prcl-expr))
                                           (err/int-error "Must reference protocol directly with var in extend" opts))
                                         (ptl-env/resolve-protocol checker (coerce/var->symbol (:var prcl-expr)) opts))
                            _ (when-not (r/Protocol? protocol)
                                (err/int-error (str "Expecting Protocol type, found " protocol) opts))
                            expected-mmap (c/make-HMap opts
                                                       ;get all combinations
                                                       {:optional
                                                        (into {}
                                                              (map
                                                                (fn [[msym mtype]]
                                                                  [(r/-val (keyword (name msym))) 
                                                                   (cu/extend-method-expected target-type mtype opts)]))
                                                              (:methods protocol))})]
                        {:expected-hmap expected-mmap
                         :prcl-expr prcl-expr
                         :mmap-expr mmap-expr}))
            cargs (into [catype]
                        (mapcat
                          (fn [{:keys [mmap-expr expected-hmap prcl-expr]}]
                            (let [cprcl-expr (check-expr prcl-expr nil opts)
                                  cmmap-expr (check-expr mmap-expr (r/ret expected-hmap) opts)]
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
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{2 3 4} (count args)) 
    (err/int-error "Wrong number of args to into-array>*" opts))
  (let [has-java-syn? (<= 3 (count args) 4)
        [javat-syn cljt-syn coll-expr]
        (cond 
          (= 3 (count args)) args
          (= 4 (count args)) (next args) ;handle temporary hacky case
          :else (cons nil args))
        opts (assoc opts ::prs/parse-type-in-ns (cu/expr-ns expr opts))
        javat-syn (some-> javat-syn (ana2/run-passes opts))
        cljt-syn (some-> cljt-syn (ana2/run-passes opts))
        javat (let [syn (or (when has-java-syn? (ast-u/quote-expr-val javat-syn))  ; generalise javat-syn if provided, otherwise cljt-syn
                            (ast-u/quote-expr-val cljt-syn))
                    c (-> 
                        (prs/parse-type syn opts)
                        (arr-ops/Type->array-member-Class opts))]
                (assert (class? c))
                c)
        cljt (prs/parse-type (ast-u/quote-expr-val cljt-syn) opts)
        ccoll (check-expr coll-expr (r/ret (c/Un [r/-nil (c/-name `t/Seqable cljt)]
                                                 opts))
                          opts)]
    (-> expr
        ; into-array>* is internal, don't check it
        #_(update :fn check-expr nil opts)
        ; the coll is always last
        (assoc :args (-> args pop (conj ccoll))
               u/expr-type (below/maybe-check-below
                             (r/ret (r/PrimitiveArray-maker javat cljt cljt))
                             expected
                             opts)))))

;not
(defmethod -invoke-special 'clojure.core/not
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (when (= 1 (count args)) 
    (let [[ctarget :as cargs] (mapv #(check-expr % nil opts) args)
          {fs+ :then fs- :else} (-> ctarget u/expr-type r/ret-f)]
      (assoc expr
             :args cargs
             u/expr-type (below/maybe-check-below
                           (r/ret (prs/parse-type 'boolean opts) 
                                  ;flip filters
                                  (fo/-FS fs- fs+)
                                  obj/-empty)
                           expected
                           opts)))))

;to-array
(defmethod -invoke-special 'clojure.core/to-array
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (when (= 1 (count args)) 
    (let [[ctarget :as cargs] (mapv #(check-expr % nil opts) args)
          targett (-> ctarget u/expr-type r/ret-t)]
      ;(prn "to-array" targett)
      (cond
        ;; handle seq-to-map-for-destructuring expansion introduced in Clojure 1.11
        (sub/subtype? targett r/-any-kw-args-seq opts)
        (let [res (reduce (fn [t union-t]
                            (if-some [intersection-ts
                                      (seq (keep #(do (assert ((some-fn r/KwArgsSeq? r/CountRange?)
                                                               %)
                                                              (print-str "TODO" (class %)))
                                                      (when (r/KwArgsSeq? %)
                                                        (c/KwArgsSeq->KwArgsArray %)))
                                                 (c/flatten-intersections [union-t])))]
                              (c/Un [t (c/In intersection-ts opts)]
                                    opts)
                              t))
                          (r/Bottom)
                          (c/flatten-unions [targett]))
              _ (assert (not (sub/subtype? res (r/Bottom) opts))
                        targett)]
          (assoc expr
                 :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret res)
                               expected
                               opts)))))))

;get
(defmethod -invoke-special 'clojure.core/get
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (get/invoke-get expr expected opts))

(defmethod -host-call-special '[:static-call clojure.lang.RT/get]
  [{:keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (get/invoke-get expr expected opts))

;FIXME should be the same as (apply hash-map ..) in invoke-apply
(defmethod -host-call-special '[:static-call clojure.lang.PersistentHashMap/create]
  [expr expected {::check/keys [check-expr] :as opts}]
  {:pre [(= :host-call (:op expr))
         (every? (comp #{:unanalyzed} :op) (:args expr))]
   :post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (-> % :target u/expr-type r/TCResult?)))]}
  (when (= 1 (count (:args expr)))
    (let [opts (assoc opts ::vs/current-expr expr)
          {[target] :args :as expr} (-> expr
                                        (update :args #(mapv (fn [t] (check-expr t nil opts)) %)))
          targett (-> target u/expr-type r/ret-t (c/fully-resolve-type opts))]
      (cond
        (and (sub/subtype? targett (c/Un [r/-nil r/-any-kw-args-seq] opts) opts)
             (not (sub/subtype? targett r/-nil opts)))
        (let [res (reduce (fn [t union-t]
                            (if-some [intersection-ts
                                      (seq (keep #(do (assert ((some-fn r/KwArgsSeq? r/Nil? r/CountRange?)
                                                               %)
                                                              (print-str "TODO" (class %)))
                                                      (when (r/KwArgsSeq? %)
                                                        (when (-> % :kw-args-regex :maybe-trailing-nilable-non-empty-map?)
                                                          (err/tc-delayed-error
                                                            (str "Cannot pass KwArgsSeq to clojure.lang.PersistentHashMap/create "
                                                                 "when :maybe-trailing-nilable-non-empty-map? is true.")
                                                            opts))
                                                        (c/KwArgsSeq->HMap % opts)))
                                                 (c/flatten-intersections [union-t])))]
                              (c/Un [t (c/In intersection-ts opts)] opts)
                              t))
                          (r/Bottom)
                          (c/flatten-unions [targett]))
              _ (assert (not (sub/subtype? res (r/Bottom) opts))
                        targett)]
          (-> expr
              (update :target check-expr nil opts)
              (assoc u/expr-type (below/maybe-check-below
                                   (r/ret res)
                                   expected
                                   opts))))
        (r/HeterogeneousSeq? targett)
        (let [res (reduce (fn [t [kt vt]]
                            {:pre [(r/HeterogeneousMap? t)]}
                            (if (r/Bottom? vt)
                              ;preserve bottom
                              (reduced vt)
                              (assoc-in t [:types kt] vt)))
                          (c/-complete-hmap {} opts)
                          (:types targett))]
          (-> expr
              (update :target check-expr nil opts)
              (assoc u/expr-type (below/maybe-check-below
                                   (r/ret res)
                                   expected
                                   opts))))))))

(defmethod -host-call-special '[:static-call clojure.lang.PersistentArrayMap/createAsIfByAssoc]
  [expr expected {::check/keys [check-expr] :as opts}]
  {:pre [(= :host-call (:op expr))
         (every? (comp #{:unanalyzed} :op) (:args expr))]
   :post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (-> % :target u/expr-type r/TCResult?)))]}
  (when (= 1 (count (:args expr)))
    (let [opts (assoc opts ::vs/current-expr expr)
          {[target] :args :as expr} (-> expr
                                        (update :args #(mapv (fn [t] (check-expr t nil opts)) %)))
          targett (-> target u/expr-type r/ret-t (c/fully-resolve-type opts))]
      (cond
        ;; handle seq-to-map-for-destructuring expansion, which always passes
        ;; the result of to-array
        (r/KwArgsArray? targett)
        (-> expr
            (update :target check-expr nil opts)
            (assoc u/expr-type (below/maybe-check-below
                                 (r/ret (c/KwArgsArray->HMap targett opts)
                                        (fo/-true-filter))
                                 expected
                                 opts)))))))

(defmethod -check ::jana2/prim-invoke
  [expr expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (assoc :op :invoke
             ::op ::ana2/invoke)
      (check-expr expected opts)
      (assoc :op :prim-invoke
             ::op ::jana2/prim-invoke)))

;;TODO when cu/should-rewrite?, add type hints for ILookupThunk
(defmethod -check ::ana2/keyword-invoke
  [{kw :keyword :keys [target] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(and (= :const (:op kw))
              (keyword? (:val kw)))]
   :post [(r/TCResult? (u/expr-type %))]}
  (impl/assert-clojure opts)
  (let [ckw (check-expr kw nil opts)
        ctarget (check-expr target nil opts)]
    (assoc expr
           :keyword ckw
           :target ctarget
           u/expr-type (invoke-kw/invoke-keyword
                         expr
                         (u/expr-type ckw)
                         (u/expr-type ctarget)
                         nil
                         expected
                         opts))))

;; TODO refactor into own file
(defn protocol-invoke [{:keys [protocol-fn target args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [cprotocol-fn (check-expr protocol-fn nil opts)
        ctarget (check-expr target nil opts)
        cargs (mapv #(check-expr % nil opts) args)
        ftype (u/expr-type cprotocol-fn)
        argtys (map u/expr-type (cons ctarget cargs))
        actual (funapp/check-funapp cprotocol-fn (cons ctarget cargs) ftype argtys expected {} opts)]
    (assoc expr
           :target ctarget
           :protocol-fn cprotocol-fn
           :args cargs
           u/expr-type actual)))

(defmethod -check ::ana2/protocol-invoke ; protocol methods
  [expr expected opts]
  (impl/assert-clojure opts)
  (protocol-invoke expr expected opts))

;binding
;FIXME use `check-normal-def`
;FIXME record checked-var-def info
(defmethod -invoke-special 'clojure.core/push-thread-bindings
  [{[bindings-expr :as args] :args :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [((every-pred vector? #(= 1 (count %))) (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (when-not (= 1 (count args))
    (err/int-error (str "push-thread-bindings expected one argument, given " (count args)) opts))
  (let [bindings-expr (-> bindings-expr (ana2/analyze-outer-root opts) (ana2/run-pre-passes opts))
        bindings-expr (cond-> bindings-expr 
                        (= :invoke (-> bindings-expr :op))
                        (update :fn ana2/run-passes opts))
        ; only support (push-thread-bindings (hash-map ~@[var bnd ...]))
        ; like `binding`s expansion
        _ (when-not (and (= :invoke (-> bindings-expr :op))
                         (= #'hash-map (-> bindings-expr :fn :var))
                         (even? (count (-> bindings-expr :args))))
            (err/nyi-error (str "Can only check push-thread-bindings with a well-formed call to hash-map as first argument"
                                " (like bindings expansion)")
                           opts))
        new-bindings-exprs (partition 2 (-> bindings-expr :args))
        cargs
        [(assoc bindings-expr
                :args
                (into []
                      (mapcat (fn [[var-expr bnd-expr]]
                                (let [{:keys [op var] :as var-expr} (-> var-expr (ana2/analyze-outer-root opts) (ana2/run-pre-passes opts))]
                                  (when-not (= :the-var op)
                                    (err/int-error (str "push-thread-bindings must have var literals for keys") opts))
                                  (let [expected (var-env/type-of (coerce/var->symbol var) opts)
                                        cvar-expr (check-expr var-expr nil opts)
                                        cexpr (check-expr bnd-expr (r/ret expected) opts)
                                        actual (-> cexpr u/expr-type r/ret-t)]
                                    (when-not (sub/subtype? actual expected opts)
                                      (err/tc-delayed-error (str "Expected binding for "
                                                                 (coerce/var->symbol var)
                                                                 " to be: " (prs/unparse-type expected opts)
                                                                 ", Actual: " (prs/unparse-type actual opts))
                                                            opts))
                                    [cvar-expr cexpr]))))
                      new-bindings-exprs))]]
    (-> expr
        ; push-thread-bindings is unannotated
        #_(update :fn check-expr nil opts)
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret r/-nil)
                             expected
                             opts)))))

(defn typing-rule-opts [expr]
  {:post [(map? %)]}
  (let [opts (:form (nth (:statements expr) 2))]
    (assert (and (seq? opts)
                 (= 2 (count opts))
                 (= 'quote (first opts)))
            (str "Options of typing rule must be a quoted map literal, "
                 "found: " (pr-str opts)
                 " in " (pr-str (:form expr))))
    ; (quote {...})
    (second opts)))

(def typing-rule-expr-kw :ret)

(defn invoke-typing-rule
  [vsym {:keys [env] :as expr} expected {::check/keys [check-expr] :as opts}]
  ;(prn "invoke-typing-rule" vsym)
  (let [unparse-type-verbose #(prs/unparse-type % (assoc opts ::vs/verbose-types false))
        subtype? (fn [s t]
                   (let [s (prs/parse-type s opts)
                         t (prs/parse-type t opts)]
                     (sub/subtype? s t opts)))
        solve (fn [t q]
                {:pre [(map? t)
                       (contains? t :type)]
                 :post [((some-fn nil? map?) %)]}
                (let [;; atm only support query = (All [x+] [in :-> out])
                      query (prs/parse-type q opts)
                      _ (assert (r/Poly? query))
                      names (c/Poly-fresh-symbols* query)
                      bbnds (c/Poly-bbnds* names query opts)
                      body (c/Poly-body* names query opts)
                      _ (assert (r/FnIntersection? body))
                      _ (assert (= 1 (count (:types body))))
                      arity (first (:types body))
                      _ (assert (r/Function? arity))
                      _ (assert (= 1 (count (:dom arity))))
                      _ (assert (= :fixed (:kind arity)))
                      _ (assert (= (fo/-simple-filter) (:fl (:rng arity))))
                      _ (assert (= obj/-empty (:o (:rng arity))))

                      lhs (prs/parse-type (:type t) opts)
                      rhs (first (:dom arity))
                      out (:t (:rng arity))
                      substitution (cgen/handle-failure
                                     (cgen/infer
                                       (zipmap names bbnds)
                                       {}
                                       [lhs]
                                       [rhs]
                                       out
                                       opts))]
                  (when substitution
                    {:type (unparse-type-verbose
                             (subst/subst-all substitution out opts))})))
        #_#_
        solve-subtype (fn [vs f]
                        {:pre [(apply distinct? vs)
                               (every? symbol? vs)]}
                        (let [gvs (map gensym vs)
                              gvs->vs (zipmap gvs vs)
                              syns (apply f gvs)
                              [lhs rhs] (mapv #(prs/parse-type % (tvar-env/with-extended-tvars opts gvs)) syns)
                              substitution
                              (cgen/handle-failure
                                (cgen/infer
                                  (zipmap gvs (repeat r/no-bounds))
                                  {}
                                  [lhs]
                                  [rhs]
                                  r/-any
                                  opts))]
                          (when substitution
                            (into {}
                                  (comp (filter (every-pred (comp (set gvs) key)
                                                            (comp crep/t-subst? val)))
                                        (map (fn [[k v]]
                                               [(gvs->vs k)
                                                (unparse-type-verbose (:type v))])))
                                  substitution))))
        rule-args {:vsym vsym
                   :opts (typing-rule-opts expr)
                   :expr (typing-rule-expr-kw expr)
                   :locals (:locals env)
                   :expected (some-> expected (cu/TCResult->map opts))
                   ;:uniquify-local prs/uniquify-local
                   :maybe-check-expected (fn [actual expected]
                                           {:pre [(map? actual)
                                                  ((some-fn nil? map?) expected)]
                                            :post [(map? %)]}
                                           (->
                                             (below/maybe-check-below
                                               (cu/map->TCResult actual)
                                               (cu/maybe-map->TCResult expected)
                                               opts)
                                             (cu/TCResult->map opts)))
                   :check (fn check-fn
                            ([expr] (check-fn expr nil))
                            ([expr expected]
                             {:pre [((some-fn nil? map?) expected)]}
                             (let [ret (some-> expected cu/map->TCResult)
                                   cexpr (check-expr expr ret opts)]
                               (assoc cexpr ::rules/expr-type (cu/TCResult->map (u/expr-type cexpr) opts)))))
                   ;:solve-subtype solve-subtype
                   :solve solve
                   :subtype? subtype?
                   :emit-form #(ast-u/emit-form-fn % opts)
                   :abbreviate-type (fn [t]
                                      (let [m (prs/parse-type t opts)]
                                        (prs/unparse-type m (assoc opts ::vs/verbose-types false))))
                   :delayed-error (fn [s opt]
                                    (let [opt (-> opt
                                                  (update :expected cu/maybe-map->TCResult)
                                                  (cond->
                                                    (contains? opt :actual)
                                                    (update :actual prs/parse-type opt)))]
                                      (err/tc-delayed-error s opt opts)))
                   :expected-error (fn [s t opt]
                                     (let [s (prs/parse-type s opts)
                                           t (cu/map->TCResult t)
                                           opt (update opt :expected cu/map->TCResult)]
                                       (cu/expected-error s t opt opts)))
                   :internal-error (fn [s opt]
                                     ;; TODO args
                                     (let [opt (update opt :expected cu/maybe-map->TCResult)]
                                       (err/int-error s opt opts)))}

        {out-expr-type ::rules/expr-type :as cexpr} (rules/typing-rule rule-args)
        out-tcresult (cu/map->TCResult out-expr-type)]
    (-> expr
        (assoc u/expr-type out-tcresult
               typing-rule-expr-kw (-> cexpr
                                       (dissoc ::rules/expr-type)
                                       (assoc u/expr-type out-tcresult))))))

;=
(defmethod -invoke-special 'clojure.core/=
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv #(check-expr % nil opts) args)]
    (-> expr
        (update :fn check-expr nil opts)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected opts)))))

;not=
(defmethod -invoke-special 'clojure.core/not=
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv #(check-expr % nil opts) args)]
    (-> expr
        (update :fn check-expr nil opts)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv :not= (map u/expr-type cargs) expected opts)))))

;identical
(defmethod -host-call-special '[:static-call clojure.lang.Util/identical]
  [expr expected {::check/keys [check-expr] :as opts}]
  {:pre [(= :host-call (:op expr))]
   :post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [{:keys [args] :as expr} (-> expr
                                    (update :target check-expr nil opts)
                                    (update :args #(mapv (fn [e] (check-expr e nil opts)) %)))]
    (assoc expr
           u/expr-type (equiv/tc-equiv :identical? (map u/expr-type args) expected opts))))

;equiv
(defmethod -host-call-special '[:static-call clojure.lang.Util/equiv]
  [expr expected {::check/keys [check-expr] :as opts}]
  {:pre [(= :host-call (:op expr))]
   :post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [{:keys [args] :as expr} (-> expr
                                    (update :target check-expr nil opts)
                                    (update :args #(mapv (fn [e] (check-expr e nil opts)) %)))]
    (assoc expr
           u/expr-type (equiv/tc-equiv := (map u/expr-type args) expected opts))))

;isa? (2 arity is special)
(defmethod -invoke-special 'clojure.core/isa?
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (when (= 2 (count args))
    (let [[cchild-expr cparent-expr :as cargs] (mapv #(check-expr % nil opts) args)]
      (-> expr
          (update :fn check-expr nil opts)
          (assoc :args cargs
                 u/expr-type (isa/tc-isa? (u/expr-type cchild-expr)
                                          (u/expr-type cparent-expr)
                                          expected
                                          opts))))))

;apply
(defmethod -invoke-special 'clojure.core/apply
  [{:keys [args env] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (apply/maybe-check-apply -invoke-apply expr expected opts))

;TODO this should be a special :do op
;manual instantiation for calls to polymorphic constructors
(defmethod -invoke-special 'clojure.core.typed/inst-poly-ctor
  [expr expected {::check/keys [check-expr] :as opts}]
  {:pre [(= 2 (count (:args expr)))]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [{[ctor-expr targs-exprs] :args :as expr} (-> expr
                                                     (update-in [:args 1] ana2/run-passes opts))
        opts (assoc opts ::prs/parse-type-in-ns (cu/expr-ns expr opts))
        targs (mapv #(prs/parse-type % opts) (ast-u/quote-expr-val targs-exprs))
        cexpr (check-expr ctor-expr nil (assoc opts ::inst-ctor-types targs))]
    (-> expr 
        (assoc-in [:args 0] cexpr)
        (assoc u/expr-type (u/expr-type cexpr)))))

;debug printing
(defmethod -invoke-special 'clojure.core.typed/print-env
  [expr expected opts]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (= 1 (count (:args expr)))
    (err/int-error (str "Wrong arguments to print-env, Expected 1, found " (count (:args expr))) opts))
  (let [{[debug-string :as args] :args :as expr} (-> expr
                                                     (update-in [:args 0] ana2/run-passes opts))]
    (when-not (= :const (:op debug-string))
      (err/int-error "Must pass print-env a string literal" opts))
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    (print-env/print-env* (prs/with-unparse-ns opts (cu/expr-ns expr opts)))
    ;DO NOT REMOVE
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret r/-nil (fo/-false-filter) obj/-empty)
                         expected
                         opts))))

;filter printing
(defmethod -invoke-special 'clojure.core.typed/print-filterset
  [expr expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (= 2 (count (:args expr)))
    (err/int-error (str "Wrong arguments to print-filterset. Expected 2, found " (count (:args expr))) opts))
  (let [{[debug-string form :as args] :args :as expr} (-> expr
                                                          (update-in [:args 0] ana2/run-passes opts))
        _ (when-not (= :const (:op debug-string)) 
            (err/int-error "Must pass print-filterset a string literal as the first argument." opts))
        cform (check-expr form expected opts)
        cargs (assoc args 1 cform)
        t (u/expr-type cform)]
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    ;(prn (:fl t))
    (let [opts (prs/with-unparse-ns opts (cu/expr-ns expr opts))] 
      (if (fl/FilterSet? (:fl t))
        (do (pprint/pprint (prs/unparse-filter-set (:fl t) opts))
            (flush))
        (prn (:fl t)))
      (prn (prs/unparse-object (:o t) opts)))
    ;DO NOT REMOVE
    (assoc expr
           :args cargs
           u/expr-type t)))

;seq
(defmethod -invoke-special 'clojure.core/seq
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (or (nthnext/check-seq expr expected opts)
      (invoke/normal-invoke expr fexpr args expected {} opts)))

;requiring-resolve
(defn check-requiring-resolve
  [{fexpr :fn :keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  ;; TODO guide the user with better error messages if any of these conditions fail.
  ;; currently complains that requiring-resolve is not annotated.
  (when (= 1 (count args))
    (let [cargs (mapv #(check-expr % nil opts) args)
          t (-> cargs first u/expr-type :t)]
      (when-some [sym (when (r/Value? t)
                        (let [v (:val t)]
                          (when (qualified-symbol? v)
                            v)))]
          ;; assumes there are never namespace aliases that shadow namespaces
          (when (var? (try (requiring-resolve sym)
                           (catch java.io.FileNotFoundException _)))
            (if (var-env/lookup-Var-nofail sym opts)
              (-> expr
                  (update :fn check-expr nil opts)
                  (assoc 
                    :args cargs
                    u/expr-type (below/maybe-check-below
                                  (r/ret (c/-name `t/Var (r/-type-of sym)))
                                  expected
                                  opts)))
              (do (println (str "WARNING: cannot check requiring-resolve call because "
                                sym " is unannotated"))
                  nil)))))))

(defmethod -invoke-special 'clojure.core/requiring-resolve [expr expected opts] (check-requiring-resolve expr expected opts))
(defmethod -invoke-special 'io.github.frenchy64.fully-satisfies.requiring-resolve/requiring-resolve [expr expected opts] (check-requiring-resolve expr expected opts))

;make vector
(defmethod -invoke-special 'clojure.core/vector
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [cargs (mapv #(check-expr % nil opts) args)]
    (-> expr
        (update :fn check-expr nil opts)
        (assoc 
          :args cargs
          u/expr-type (below/maybe-check-below
                        (r/ret (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                                        {:filters (mapv (comp r/ret-f u/expr-type) cargs)
                                         :objects (mapv (comp r/ret-o u/expr-type) cargs)}
                                        opts))
                        expected
                        opts)))))

;(apply concat hmap)
(defmethod -invoke-apply 'clojure.core/concat
  [{[_concat-fn_ & args] :args :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (let [cargs (mapv #(check-expr % nil opts) args) ;FIXME possible repeated check-expr
        tmap (when (= 1 (count cargs))
               (c/fully-resolve-type (r/ret-t (u/expr-type (last cargs))) opts))]
    (let [opts (assoc opts ::vs/current-expr expr)]
      (when (r/HeterogeneousMap? tmap)
        (let [r (c/HMap->KwArgsSeq tmap)]
          (-> expr
              (update :fn check-expr nil opts)
              (assoc u/expr-type (below/maybe-check-below
                                   (r/ret r (fo/-true-filter))
                                   expected
                                   opts))))))))

;apply hash-map
(defmethod -invoke-apply 'clojure.core/hash-map
  [{[fn-expr & args] :args :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (let [cargs (mapv #(check-expr % nil opts) args)
        nargs (count cargs)]
    (cond
      (and (= 1 nargs)
           (r/KwArgsSeq? (u/expr-type (peek cargs))))
      (-> expr
          (update :fn check-expr nil opts)
          ;; FIXME add annotation for hash-map to check fn-expr
          (assoc :args (into [(ana2/run-passes fn-expr opts)] cargs)
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/KwArgsSeq->HMap (-> cargs peek u/expr-type r/ret-t) opts))
                               expected
                               opts)))

      (and (pos? nargs)
           (let [hseq (r/ret-t (u/expr-type (peek cargs)))]
             (when (and (r/HSequential? hseq)
                        (not ((some-fn :rest :drest :repeat) hseq)))
               ;; every key must be a Value
               (let [kvs (into (mapv (comp r/ret-t u/expr-type) (pop cargs))
                               (mapcat vector)
                               (:types hseq))]
                 (and (even? (count kvs))
                      (every? (comp r/Value? first) (partition 2 kvs)))))))
      (-> expr
          (update :fn check-expr nil opts)
          ;; FIXME add annotation for hash-map to check fn-expr
          (assoc :args (into [(ana2/run-passes fn-expr opts)] cargs)
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/-complete-hmap
                                        (apply hash-map (concat (map (comp r/ret-t u/expr-type) (pop cargs))
                                                                (mapcat vector (:types (r/ret-t (u/expr-type (peek cargs)))))))
                                        opts))
                               expected
                               opts))))))


;nth
(defmethod -host-call-special '[:static-call clojure.lang.RT/nth]
  [{:keys [args] :as expr} expected opts]
  {:pre [(#{:host-call :invoke} (:op expr))
         (every? (every-pred (comp #{:unanalyzed} :op)
                             (complement u/expr-type))
                 args)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nth/invoke-nth expr expected opts))

(defmethod -invoke-special 'clojure.core/nth
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nth/invoke-nth expr expected opts))

;nthnext
(defmethod -invoke-special 'clojure.core/nthnext
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nthnext/check-nthnext expr expected opts))

;next
(defmethod -invoke-special 'clojure.core/next
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nthnext/check-next expr expected opts))

;rest
(defmethod -invoke-special 'clojure.core/rest
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (nthnext/check-rest expr expected opts))

;keeping because it might be handy later
#_
(defn first-result [t opts]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Result?) %)]}
  (let [ftype (fn ftype [t]
                {:pre [(r/Type? t)]
                 :post [((some-fn nil? r/Result?) %)]}
                (let [t (c/fully-resolve-type t opts)]
                  (cond
                    (r/Union? t) (let [ts (mapv ftype (:types t))]
                                   (when (every? identity ts)
                                     #_ ;; what is this?
                                     (apply c/union-Results ts)))
                    (r/Intersection? t) (when-let [ts (seq (keep ftype (:types t)))]
                                          #_ ;; what is this?
                                          (apply c/intersect-Results ts))
                    (r/Nil? t) (r/make-Result r/-nil (fo/-false-filter))
                    (r/HSequential? t) (cond
                                         (seq (:types t))
                                         (r/make-Result (first (:types t))
                                                        (first (:fs t))
                                                        (first (:objects t)))

                                         (:rest t) (r/make-Result (c/Un [r/-nil (:rest t)] opts))
                                         (:drest t) (r/make-Result r/-any)

                                         (empty? (:types t)) (r/make-Result (r/ret r/-nil (fo/-false-filter)))))))]
    (ftype (nthnext/seq-type t opts))))

;first
#_
(defmethod -invoke-special 'clojure.core/first
  [{fexpr :fn :keys [args] :as expr} expected opts]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (when-not (= 1 (count args))
    (err/int-error (str "'first' accepts 1 argument, found "
                        (count args))
                   opts))
  #_
  (when (::vs/custom-expansions opts)
    (let [[coll :as cargs] (mapv #(check-expr % nil opts) args)
          ct (r/ret-t (u/expr-type coll))
          fres (first-result ct)]
      (when fres
        (assoc expr
               :args cargs
               u/expr-type (r/Result->TCResult fres)))))
  nil)

;assoc
(defmethod -invoke-special 'clojure.core/assoc
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [[target & keyvals] args

        _ (when-not (<= 3 (count args))
            (err/int-error (str "assoc accepts at least 3 arguments, found "
                                     (count args))
                           opts))
        _ (when-not (even? (count keyvals))
            (err/int-error "assoc accepts an even number of keyvals" opts))

        ctarget (check-expr target nil opts)
        targetun (-> ctarget u/expr-type r/ret-t)
        ckeyvals (mapv #(check-expr % nil opts) keyvals)
        keypair-types (partition 2 (map u/expr-type ckeyvals))
        cargs (into [ctarget] ckeyvals)]
    (if-let [new-hmaps (assoc-u/assoc-type-pairs targetun keypair-types opts)]
      (-> expr
        (update :fn check-expr nil opts)
        (assoc
          :args cargs
          u/expr-type (below/maybe-check-below
                        (r/ret new-hmaps
                               (fo/-true-filter)) ;assoc never returns nil
                        expected
                        opts)))
      
      ;; to do: improve this error message
      (err/tc-delayed-error (str "A call to assoc failed to type check with target expression of type:\n\t" (prs/unparse-type targetun opts)
                                 "\nand key/value pairs of types: \n\t"
                                 (str/join " " (map (comp pr-str #(prs/unparse-type % opts) :t u/expr-type) ckeyvals)))
                            ;; first argument is to blame, gather any blame information from there
                            {:expected (u/expr-type ctarget)
                             :return (-> expr
                                         (update :fn check-expr nil opts)
                                         (assoc
                                           :args cargs
                                           u/expr-type (cu/error-ret expected)))}
                            opts))))

(defmethod -invoke-special 'clojure.core/dissoc
  [{fexpr :fn :keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (let [_ (when-not (seq args)
            (err/int-error (str "dissoc takes at least one argument, given: " (count args)) opts))
        ;FIXME possible repeated type checking
        [ctarget & cdissoc-args :as cargs] (mapv #(check-expr % nil opts) args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cdissoc-args)]
    (when-let [new-t (assoc-u/dissoc-keys ttarget targs opts)]
      (-> expr
          (update :fn check-expr nil opts)
          (assoc
            :args cargs
            u/expr-type (below/maybe-check-below
                          (r/ret new-t)
                          expected
                          opts))))))

; merge
(defmethod -invoke-special 'clojure.core/merge
  [{fexpr :fn :keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (let [;FIXME possible repeated type checking
        cargs (mapv #(check-expr % nil opts) args)
        targs (map u/expr-type cargs)]
    (when-some [merged (apply assoc-u/merge-types opts targs)]
      (-> expr
          (update :fn check-expr nil opts)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret merged)
                               expected
                               opts))))))

;conj
(defmethod -invoke-special 'clojure.core/conj
  [{fexpr :fn :keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [;FIXME possible repeated type checking
        [ctarget & cconj-args :as cargs] (mapv #(check-expr % nil opts) args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cconj-args)]
    (when-let [conjed (apply assoc-u/conj-types opts ttarget targs)]
      (-> expr
          (update :fn check-expr nil opts)
          (assoc :args cargs
                 u/expr-type (below/maybe-check-below
                               (r/ret conjed
                                      (fo/-true-filter) ; conj never returns nil
                                      obj/-empty)
                               expected
                               opts))))))

(comment
  (method-expected-type (prs/parse-type '[Any -> Any])
                        (prs/parse-type '(Value :op))
                        (prs/parse-type '(Value :if)))
  ;=> ['{:if Any} -> Any]
  )

; cli
;TODO add cargs to result
(defmethod -invoke-special 'clojure.tools.cli/cli
  [{[args-expr & specs-exprs] :args :keys [env] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [opts (assoc opts ::vs/current-env env)
        args-expected-ty (prs/parse-type `(t/Seqable t/Str) opts)
        cargs-expr (check-expr args-expr nil (assoc opts ::vs/current-env (:env args-expr)))
        _ (when-not (sub/subtype? 
                      (-> cargs-expr u/expr-type r/ret-t)
                      args-expected-ty
                      opts)
            (cu/expected-error (-> cargs-expr u/expr-type r/ret-t)
                               (r/ret args-expected-ty)
                               (assoc opts ::vs/current-env (:env args-expr))))
        spec-map-ty (reduce (fn [t spec-expr]
                              (if-let [[keyt valt] (cli/parse-cli-spec spec-expr opts)]
                                (-> t
                                  (assoc-in [:types keyt] valt))
                                ; resort to a general type
                                (do
                                  ;(prn "cli: giving up because of" (ast-u/emit-form-fn spec-expr opts)
                                       ;"\n" spec-expr)
                                  (reduced 
                                    (c/RClass-of IPersistentMap [(c/RClass-of clojure.lang.Keyword opts) r/-any] opts)))))
                            (c/-complete-hmap {} opts)
                            specs-exprs)

        actual (r/-hvec [spec-map-ty 
                         (prs/parse-type `(t/Seqable t/Str) opts)
                         (prs/parse-type `t/Str opts)]
                        {}
                        opts)
        _ (when expected
            (when-not (sub/subtype? actual (r/ret-t expected) opts)
              (cu/expected-error 
                actual expected opts)))
        cargs (vec (cons cargs-expr specs-exprs))]
    (-> expr
        (update :fn check-expr nil opts)
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             (r/ret actual)
                             expected
                             opts)))))

(defmethod -invoke-special 'typed.cljc.checker.check.utils/special-typed-expression
  [expr expected opts]
  (let [_ (assert (= 1 (count (:args expr))))
        {[type-expr] :args :keys [env] :as expr} (-> expr
                                                     (update-in [:args 0] ana2/run-passes opts))
        _ (assert (= :quote (:op type-expr)))
        _ (assert (= :const (-> type-expr :expr :op))
                  (-> type-expr :expr :op))
        t (prs/parse-type (-> type-expr :expr :val) opts)]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (r/ret t)
                         expected
                         opts))))

; FIXME this needs a line number from somewhere!
(defmethod -host-call-special '[:instance-call clojure.lang.MultiFn/addMethod]
  [expr expected {::vs/keys [check-config]
                  ::check/keys [check-expr] :as opts}]
  {:pre [(every? (every-pred (complement u/expr-type)
                             (comp #{:unanalyzed} :op))
                 (cons (:target expr) (:args expr)))]
   :post [#_(every? :post-done (cons (:target %) (:args %)))]}
  (when-not (= 2 (count (:args expr)))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn/addMethod" opts))
  (let [checker (cenv/checker opts)
        {[dispatch-val-expr _] :args target :target :keys [env] :as expr}
        (cond-> expr
          (-> expr :target :form symbol?) (update :target ana2/run-passes opts))
        _ (when-not (= :var (:op target))
            (err/int-error "Must call addMethod with a literal var" opts))
        var (:var target)
        _ (assert (var? var))
        mmsym (coerce/var->symbol var)
        expr (assoc expr
                    u/expr-type (below/maybe-check-below
                                  (r/ret (c/RClass-of clojure.lang.MultiFn opts))
                                  expected
                                  (assoc opts ::vs/current-expr expr)))
        default? (cu/default-defmethod? var (ast-u/emit-form-fn dispatch-val-expr opts))
        unannotated-def (:unannotated-def check-config)]
    (cond
      (and (= :unchecked unannotated-def)
           (not (var-env/lookup-Var-nofail mmsym opts)))
      (-> expr
          (update :args #(mapv (fn [e] (ana2/run-passes e opts)) %)))

      ;skip if warn-on-unannotated-vars is in effect
      (or (and (ns-opts/warn-on-unannotated-vars? checker (cu/expr-ns expr opts))
               (not (var-env/lookup-Var-nofail mmsym opts)))
          (not (var-env/check-var? checker mmsym)))
      (do (u/tc-warning (str "Not checking defmethod " mmsym " with dispatch value: " 
                             (pr-str (ast-u/emit-form-fn dispatch-val-expr opts)))
                        opts)
          (-> expr
              (update :args #(mapv (fn [e] (ana2/run-passes e opts)) %))))
      :else
      (let [{[dispatch-val-expr method-expr] :args :as expr}
            (-> expr
                (update :args #(-> %
                                   (update 0 check-expr nil opts)
                                   (update 1 (fn [e] (-> e (ana2/analyze-outer-root opts) (ana2/run-pre-passes opts)))))))
            _ (assert (= :var (:op target)))
            _ (when-not (= :fn (:op method-expr))
                (err/int-error (str "Method must be a fn") opts))
            dispatch-type (mm/multimethod-dispatch-type mmsym opts)]
        (if-not dispatch-type
          (err/tc-delayed-error (str "Multimethod requires dispatch type: " mmsym
                                     "\n\nHint: defmulti must be checked before its defmethods")
                                {:return (-> expr
                                             (update-in [:args 1] ana2/run-passes opts))}
                                (assoc opts ::vs/current-env env))
          (let [method-expected (var-env/type-of mmsym opts)
                cmethod-expr 
                (let [opts (assoc opts ::multi-u/current-mm (when-not default?
                                                              {:dispatch-fn-type dispatch-type
                                                               :dispatch-val-ret (u/expr-type dispatch-val-expr)}))]
                  (check-expr method-expr (r/ret method-expected) opts))]
            (-> expr
                (assoc-in [:args 1] cmethod-expr))))))))

(defmethod -invoke-special :default [expr expected opts])
(defmethod -host-call-special :default [expr expected opts])

;;TODO attach new :args etc.
;;convert apply to normal function application
(defmethod -invoke-apply :default [expr expected opts])



;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form :clojure.core.typed/cast [expr expected opts] (cast/check-cast expr expected opts))
(defmethod internal-special-form :clojure.core.typed/loop [expr expected opts] (special-loop/check-special-loop expr expected opts))

(defmethod internal-special-form :default
  [expr expected opts]
  (invoke-typing-rule (coerce/kw->symbol (u/internal-dispatch-val expr)) expr expected (assoc opts ::vs/current-expr expr)))

(defmethod -check ::jana2/monitor-enter [expr expected opts] (monitor/check-monitor expr expected opts))
(defmethod -check ::jana2/monitor-exit  [expr expected opts] (monitor/check-monitor expr expected opts))

(defmethod -check ::ana2/host-interop
  [expr expected opts]
  (impl/assert-clojure opts)
  (host-interop/check-host-interop expr expected opts))

(defmethod -check ::ana2/host-call
  [expr expected opts]
  (impl/assert-clojure opts)
  (host-interop/check-host-call -host-call-special expr expected opts))

(defmethod -check ::ana2/host-field
  [expr expected opts]
  (impl/assert-clojure opts)
  (host-interop/check-host-interop expr expected opts))

(defmethod -check ::ana2/maybe-host-form
  [expr expected opts]
  (impl/assert-clojure opts)
  (host-interop/check-maybe-host-form expr expected opts))

(defmethod -check ::ana2/maybe-class
  [expr expected {::check/keys [check-expr] :as opts}]
  (impl/assert-clojure opts)
  (let [expr (ana2/run-post-passes expr opts)]
    (if (= :maybe-class (:op expr))
      (err/tc-delayed-error (str "Unresolved host interop: " (:form expr)
                                 "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            {:return (assoc expr u/expr-type (or expected r/-error))}
                            opts)
      (check-expr expr expected opts))))

(defmethod -invoke-special 'clojure.core/instance?
  [{[cls-expr :as args] :args :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [((some-fn nil?
                    (comp r/TCResult? u/expr-type))
           %)]}
  (when-not (= 2 (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/instance?,"
                        " expected 2, given " (count (:args expr)))
                   opts))
  (when-let [cls (when (symbol? (:form cls-expr))
                   (let [cls (ana2/resolve-sym (:form cls-expr)
                                               (:env cls-expr)
                                               opts)]
                     (when (class? cls)
                       cls)))]
    (let [{[cls-expr cexpr] :args :as expr}
          (-> expr
              (update :args #(mapv (fn [e t] (check-expr e t opts))
                                   %
                                   [(r/ret (c/RClass-of Class opts))
                                    nil])))
          inst-of (c/RClass-of-with-unknown-params cls opts)
          expr-tr (u/expr-type cexpr)]
      (assoc expr
             u/expr-type (below/maybe-check-below
                           (r/ret (c/Un [r/-true r/-false] opts)
                                  (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                          (fo/-not-filter-at inst-of (r/ret-o expr-tr))))
                           expected
                           opts)))))

(defmethod -invoke-special 'clojure.core/satisfies?
  [{[cls-expr :as args] :args :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [((some-fn nil?
                    (comp r/TCResult? u/expr-type))
           %)]}
  (when-not (= 2 (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/satisfies?,"
                        " expected 2, given " (count (:args expr)))
                   opts))
  (when-some [v (when (symbol? (:form cls-expr))
                  (let [v (ana2/resolve-sym (:form cls-expr)
                                            (:env cls-expr)
                                            opts)]
                    (when (var? v)
                      v)))]
    (let [{[_ cexpr] :args :as expr}
          (-> expr
              (update-in [:args 1] check-expr nil opts))
          inst-of (c/Protocol-with-unknown-params (symbol v) opts)
          expr-tr (u/expr-type cexpr)]
      (assoc expr
             u/expr-type (below/maybe-check-below
                           (r/ret (c/Un [r/-true r/-false] opts)
                                  (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                          (if (:extend-via-metadata @v)
                                            fl/-top ;; satisfies? does not rule out metadata extension https://clojure.atlassian.net/browse/CLJ-2426
                                            (fo/-not-filter-at inst-of (r/ret-o expr-tr)))))
                           expected
                           opts)))))

(defmethod -check ::jana2/instance?
  [{cls :class the-expr :target :as expr} expected {::check/keys [check-expr] :as opts}]
  ;(assert nil ":instance? node not used")
  (impl/assert-clojure opts)
  (let [inst-of (c/RClass-of-with-unknown-params cls opts)
        cexpr (check-expr the-expr nil opts)
        expr-tr (u/expr-type cexpr)]
    (assoc expr
           :target cexpr
           u/expr-type (below/maybe-check-below
                         (r/ret (c/Un [r/-true r/-false] opts)
                                (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                        (fo/-not-filter-at inst-of (r/ret-o expr-tr))))
                         expected
                         opts))))

(defmulti -new-special (fn [{{:keys [form env] :as cls-expr} :class :as expr} expected opts]
                         {:pre [(= :maybe-class (:op cls-expr))]
                          :post [((some-fn nil? symbol?) %)]}
                         (let [cls (ana2/resolve-sym form
                                                     ; `new` ignores locals
                                                     (assoc env :locals {})
                                                     opts)]
                           (when (class? cls)
                             (coerce/Class->symbol cls)))))

;; TODO share this logic with a macro rule for `clojure.core/defmulti`
(defmethod -new-special 'clojure.lang.MultiFn
  [expr expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (when-not (== 4 (count (:args expr)))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn constructor" opts))
  (when-not expected
    (err/int-error "Expected type needed for defmulti" opts))
  (let [expected-t (r/ret-t expected)
        expected-d (multi/expected-dispatch-type expected-t opts)
        {[nme-expr cdispatch-expr default-expr hierarchy-expr] :args :as expr}
        (-> expr
            (update :class check-expr nil opts)
            ;name
            (update-in [:args 0] check-expr nil opts)
            ;dispatch-expr
            (update-in [:args 1] check-expr nil opts)
            ;default
            (update-in [:args 2] check-expr nil opts)
            ;hierarchy
            (update-in [:args 3] check-expr nil opts))
        _ (when-not (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
            (err/int-error "Multimethod hierarchy cannot be customised" opts))
        _ (when-not (= (:val default-expr) :default)
            (err/int-error "Non :default default dispatch value NYI" opts))
        mm-name (:val nme-expr)
        _ (when-not (string? mm-name)
            (err/int-error "MultiFn name must be a literal string" opts))
        inferred-dispatch-t (c/fully-resolve-type (r/ret-t (u/expr-type cdispatch-expr)) opts)
        inferred-dispatch-t (if (r/Intersection? inferred-dispatch-t)
                              (first (filter (some-fn r/SymbolicClosure? r/FnIntersection? r/Poly? r/PolyDots?)
                                             (:types inferred-dispatch-t)))
                              inferred-dispatch-t)
        _ (when-not inferred-dispatch-t
            (err/nyi-error (str "defmulti dispatch not a function: " (pr-str (r/ret-t (u/expr-type cdispatch-expr)))) opts))
        _ (when ((some-fn r/Union? r/Intersection?) inferred-dispatch-t)
            (err/nyi-error "defmulti dispatch function inferred as union or intersection" opts))
        ;_ (prn "inferred-dispatch-t" inferred-dispatch-t)
        ;_ (prn "expected-d" expected-d)
        resolved-dispatch-t (cond-> inferred-dispatch-t
                              (r/SymbolicClosure? inferred-dispatch-t) (-> (sub/check-symbolic-closure expected-d opts)
                                                                           u/expr-type
                                                                           r/ret-t))
        ;_ (prn "resolved-dispatch-t" resolved-dispatch-t)
        mm-qual (symbol (str (cu/expr-ns expr opts)) mm-name)
        _ (mm/add-multimethod-dispatch-type mm-qual resolved-dispatch-t opts)]
    (-> expr
        (assoc u/expr-type (below/maybe-check-below
                             (r/ret (c/In [#_(c/RClass-of clojure.lang.MultiFn opts) 
                                           expected-t]
                                          opts))
                             expected
                             opts)))))

(defmethod -new-special :default [expr expected opts])

(defn check-new
  [expr expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  ;(prn ":new" (mapv (juxt :op :tag) (cons (:class expr) (:args expr))))
  (impl/assert-clojure opts)
  (let [opts (-> opts
                 (assoc ::vs/current-env (:env expr))
                 (assoc ::vs/current-expr expr))]
    (or (-new-special expr expected opts)
        (let [checker (cenv/checker opts)
              inst-types (::inst-ctor-types opts)
              expr (-> expr
                       (update :class check-expr nil opts)
                       (update :args #(let [opts (assoc opts ::inst-ctor-types nil)]
                                        (mapv (fn [e] (check-expr e nil opts)) %)))
                       ;delegate eval to check-expr
                       (ana2/run-post-passes opts))
              ;; call when we're convinced there's no way to rewrite this AST node
              ;; in a non-reflective way.
              give-up (fn [expr]
                        (let [clssym (cu/NewExpr->qualsym expr)]
                          (err/tc-delayed-error (str "Unresolved constructor invocation " 
                                                     (type-hints/suggest-type-hints 
                                                       nil 
                                                       nil 
                                                       (map (comp r/ret-t u/expr-type) (:args expr))
                                                       {:constructor-call clssym}
                                                       opts)
                                                     ".\n\nHint: add type hints")
                                                {:form (ast-u/emit-form-fn expr opts)
                                                 :return (assoc expr
                                                                u/expr-type (cu/error-ret expected))}
                                                opts)))
              ;; returns the function type for this constructor, or nil if
              ;; it is reflective.
              ctor-fn (fn [expr]
                        (when (:validated? expr)
                          (let [clssym (cu/NewExpr->qualsym expr)]
                            (or (ctor-override/get-constructor-override checker clssym opts)
                                (and (dt-env/get-datatype checker clssym opts)
                                     (cu/DataType-ctor-type clssym opts))
                                (when-let [ctor (cu/NewExpr->Ctor expr)]
                                  (cu/Constructor->Function ctor opts))))))
              ;; check a non-reflective constructor
              check-validated (fn [expr]
                                (let [ifn (-> (ctor-fn expr)
                                              (cond-> inst-types
                                                (inst/manual-inst inst-types {} opts))
                                              r/ret)
                                      ;_ (prn "Expected constructor" (prs/unparse-type (r/ret-t ifn) opts))
                                      res-type (funapp/check-funapp expr (:args expr) ifn (map u/expr-type (:args expr)) expected {} opts)]
                                  (assoc expr
                                         u/expr-type res-type)))]
          ;; try to rewrite, otherwise error on reflection
          (cond
            (:validated? expr) (check-validated expr)

            (cu/should-rewrite? opts) (let [expr (update expr :args #(mapv host-interop/add-type-hints %))
                                            rexpr (host-interop/try-resolve-reflection expr opts)]
                                        ;; rexpr can only be :new
                                        (case (:op rexpr)
                                          (:new) (if (:validated? rexpr)
                                                   (check-validated rexpr)
                                                   (give-up rexpr))))
            :else (give-up expr))))))

(defn check-def
  [{:keys [var env] :as expr} expected opts]
  (impl/assert-clojure opts)
  (let [checker (cenv/checker opts)
        prs-ns (cu/expr-ns expr opts)
        mvar (meta var)
        qsym (coerce/var->symbol var)
        opts (-> opts
                 (assoc ::vs/current-env env)
                 (assoc ::prs/parse-type-in-ns prs-ns))]
    ; annotation side effect
    ;; TODO convert to type provider
    (when-let [[_ tsyn] (find mvar :ann)]
      (let [ann-type (prs/parse-type tsyn opts)]
        (var-env/add-var-type checker qsym ann-type opts)))
    (when (:no-check mvar)
      (var-env/add-nocheck-var checker qsym))
    (def/check-def expr expected opts)))

(defmethod -check ::jana2/deftype
  [expr expected opts]
  (deftype/check-deftype expr expected opts))

(defmethod -check ::jana2/reify
  [expr expected opts]
  (reify/check-reify expr expected opts))

(defmethod -check ::jana2/import
  [expr expected opts]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-nil)
                       expected
                       opts)))

(defmethod -check ::jana2/case-test
  [{:keys [test] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [ctest (check-expr test expected opts)]
    (assoc expr
           :test ctest
           u/expr-type (u/expr-type ctest))))

(defmethod -check ::jana2/case
  [{target :test :keys [tests thens default] :as expr} expected {::check/keys [check-expr] :as opts}]
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
  (let [opts (-> opts
                 (assoc ::vs/current-env (:env expr))
                 (assoc ::vs/current-expr expr))
        ctarget (check-expr target nil opts)
        target-ret (u/expr-type ctarget)
        _ (assert (r/TCResult? target-ret))
        ctests (mapv #(check-expr % nil opts) tests)
        tests-rets (map u/expr-type ctests)
        ; Can we derive extra information from 'failed'
        ; tests? Delegate to check-case-thens for future enhancements.
        cthens (case/check-case-thens target-ret tests-rets thens expected opts)
        cdefault (let [flag+ (volatile! true)
                       neg-tst-fl (let [val-ts (map (comp #(c/fully-resolve-type % opts) r/ret-t) tests-rets)]
                                    (if (every? r/Value? val-ts)
                                      (fo/-not-filter-at (c/Un val-ts opts)
                                                         (r/ret-o target-ret))
                                      fl/-top))
                       env-default (update/env+ (lex/lexical-env opts) [neg-tst-fl] flag+ opts)
                       _ (when-not @flag+
                           ;; FIXME should we ignore this branch?
                           (u/tc-warning "Local became bottom when checking case default" opts))]
                   ;(prn "neg-tst-fl" neg-tst-fl)
                   ;(prn "env-default" env-default)
                   (check-expr default expected (var-env/with-lexical-env opts env-default)))
        ;; FIXME this is a duplicated expected test, already done able
        case-result (let [type (c/Un (map (comp :t u/expr-type) (cons cdefault cthens)) opts)
                          ; TODO
                          filter (fo/-FS fl/-top fl/-top)
                          ; TODO
                          object obj/-empty]
                      (below/maybe-check-below
                        (r/ret type filter object)
                        expected
                        opts))]
    (assoc expr
           :test ctarget
           :tests ctests
           :thens cthens
           :default cdefault
           u/expr-type case-result)))

;; public ops

(defn check-expr
  "Type checks expr at optional expected type. expr must not have a u/expr-type entry.
  
  The return expr will be fully expanded, analyzed, evaluated (via ana2/eval-top-level),
  with a u/expr-type entry giving the TCResult of the whole expression.

  As an exception, an :unanalyzed node may be returned from this function. It will have a :tag,
  and :result (if top-level). If not top-level, the node will be expanded and evaluated as part
  of its enclosing top-level expression."
  [expr expected opts]
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
      (u/trace
        (str "Checking line " (:line env) ":" (:column env) ":" (:file env)
             "\n"
             "> " (binding [*print-level* (or *print-level* 10)
                            *print-length* (or *print-length* 10)]
                    (pr-str (:form expr))))
        opts)
      (if (= :unanalyzed (:op expr))
        ;; Type checks the :unanalyzed expr at expected type.
        ;; The return expr will be fully expanded, analyzed, evaluated (if top-level),
        ;; with a u/expr-type entry for the TCResult of the entire expression."
        (let [;register typing rules (ie., implementations of -unanalyzed-top-level
              ; and -unanalyzed-special)
              _ @*register-exts]
          (or (let [opts (-> opts
                             (update ::vs/current-env #(if (:line env) env %))
                             (assoc ::vs/current-expr expr))]
                (or (meta-ann/maybe-check-meta-ann expr expected opts)
                    (unanalyzed/-unanalyzed-special expr expected opts)
                    (maybe-check-inlineable expr expected opts)))
              (-> expr
                  (ana2/analyze-outer opts)
                  recur)))
        (let [opts (-> opts
                       (update ::vs/current-env #(if (:line env) env %))
                       (assoc ::vs/current-expr expr))]
          (-> expr
              (ana2/run-pre-passes opts)
              (-check expected opts)
              (ana2/run-post-passes opts)
              (ana2/eval-top-level opts)))))))

(defn check-top-level
  "Type check a top-level form at an expected type, returning a
  fully analyzed core.typed.analyzer AST node (ie., containing no :unanalyzed nodes)
  with a u/expr-type entry giving its TCResult type, and a :result entry
  holding its evaluation result."
  ([form expected {:keys [env] :as opt} {::vs/keys [check-config custom-expansions] :as opts}]
   ;(prn "check-top-level" form)
   ;(prn "*ns*" *ns*)
   (let [nsym (or (:ns env) (::prs/parse-type-in-ns opts))
         _ (assert (symbol? nsym))
         extra (when (= :before (:check-form-eval check-config))
                 {:result (eval form)})
         side-effects? (case (get-in opts [::vs/check-config :check-form-eval])
                         (:never :before) false
                         (:after nil) true)
         opts (-> opts
                  (assoc ::check/check-expr check-expr)
                  (assoc ::vs/lexical-env (lex/init-lexical-env))
                  ;; also copied to typed.cljs.checker.check/check-top-level
                  (assoc ::c/Un-cache (atom c/initial-Un-cache))
                  (assoc ::c/In-cache (atom {}))
                  (assoc ::c/RClass-of-cache (atom {}))
                  (assoc ::c/supers-cache (atom {}))
                  (assoc ::sub/subtype-cache (atom {}))
                  (assoc ::cgen/dotted-var-store (atom {}))
                  (assoc ::prs/parse-type-in-ns nsym)
                  (assoc ::prs/unparse-type-in-ns nsym)
                  (update ::ana2/eval-ast (fn [eval-ast]
                                            (fn [ast {::vs/keys [delayed-errors] :as opts}]
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
                                                (if side-effects?
                                                  (eval-ast ast opts)
                                                  ast)))))
                  (cond->
                    (not side-effects?)
                    (-> (assoc ::ana2/create-var (fn [sym {:keys [ns]} opts]
                                                   (or (find-var
                                                         (symbol (-> ns ns-name name)
                                                                 (name sym)))
                                                       (err/int-error
                                                         (format "Could not find var %s in namespace %s"
                                                                 sym (ns-name ns))
                                                         opts))))
                        ;; reify* also imports a class name, but it's gensym'd.
                        (assoc ::jana2/parse-deftype-with-existing-class true)))
                  (assoc ::ana2/macroexpand-1 ana-clj/macroexpand-1)
                  (update ::ana2/scheduled-passes #(if custom-expansions
                                                     ana-clj/scheduled-passes-for-custom-expansions
                                                     %))
                  (env/ensure (jana2/global-env)))]
     (with-bindings (dissoc (ana-clj/thread-bindings {} opts) #'*ns*) ; *ns* is managed by higher-level ops like check-ns1
       (-> form
           (ana2/unanalyzed-top-level (or env (jana2/empty-env nsym)) opts)
           (cache/check-top-level-expr expected opt opts)
           (into extra))))))

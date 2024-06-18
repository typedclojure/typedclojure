;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.check
  (:refer-clojure :exclude [delay])
  (:require [cljs.analyzer :as cljs-ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.tagged-literals :as tags]
            [cljs.util :as cljs-util]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.check.dot-cljs :as dot]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io]
            [clojure.string :as c-str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.clj.checker.tc-equiv :as equiv]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-impl :refer [-check]]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.def :as def]
            [typed.cljc.checker.check.print-env :as pr-env]
            [typed.cljc.checker.check.special.loop :as special-loop]
            [typed.cljc.checker.check.unanalyzed :as unanalyzed]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as f]
            [typed.cljc.checker.inst :as inst]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.local-result :as local-result]
            [typed.cljc.checker.ns-deps-utils :as ns-depsu]
            [typed.cljc.checker.object-rep :as o]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r :refer [ret ret-t ret-o]]
            [typed.cljc.checker.utils :as u :refer [expr-type]]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljs.analyzer :as tana2]
            [typed.cljs.checker.jsnominal-env :as jsnom]
            [typed.cljs.checker.util :as uc]
            [typed.clojure :as t]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]))

(def ^:private *register-exts (delay
                                (configs/register-cljs-config-anns)
                                (configs/register-cljs-config-exts)))

(defn maybe-check-unanalyzed [{:keys [form env] :as expr} expected {::check/keys [check-expr] :as opts}]
  (binding [vs/*current-expr* expr]
    (or (unanalyzed/-unanalyzed-special expr expected opts)
        ;; don't expand macros that inline raw js
        (when-some [rsym (when (seq? form) (ana2/resolve-sym (first form) env))]
          (when-some [cljsvar-ann (var-env/type-of-nofail rsym opts)]
            ;(prn "cljsvar-ann" rsym cljsvar-ann)
            (let [macro-var (find-var rsym)]
              (when (and (var? macro-var)
                         (-> macro-var meta :macro))
                (check-expr (assoc expr :form (-> form
                                                  vec
                                                  (update 0 #(with-meta (list 'do %) {::fake-do true}))
                                                  list*
                                                  (with-meta (meta form))))
                            expected
                            opts))))))))

(defn unanalyzed-top-level [form env]
  (tana2/unanalyzed form env))

(defn flush-analysis-side-effects [cexpr opts]
  (reduce (fn [ast pass] (pass (:env ast) ast opts))
          (ast/walk cexpr
                    identity
                    ana2/analyze-outer-root)
          cljs-ana/default-passes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check CLJS AST

(defmethod -check ::tana2/no-op
  [expr expected opts]
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected
                     opts)))

(defn check-def
  [{:keys [init] :as expr} expected opts]
  (if init
    (def/check-normal-def expr expected opts)
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (ret r/-any)
                         expected
                         opts))))

(defmethod -check ::tana2/js
  [{:keys [js-op args env] :as expr} expected {::check/keys [check-expr] :as opts}]
  (cond
    js-op (let [res (expr-type (check-expr {:op :invoke
                                            ::ana2/op ::ana2/invoke
                                            :from-js-op expr
                                            :env env
                                            :children [:fn :args]
                                            :fn {:op :var
                                                 :env env
                                                 :name js-op}
                                            :args args}
                                           expected
                                           opts))]
            (assoc expr
                   u/expr-type res))
    :else (do (u/tc-warning "js-op missing, inferring Any" opts)
              (assoc expr
                     u/expr-type (below/maybe-check-below
                                   (r/ret r/-any)
                                   expected
                                   opts)))))

(defmulti invoke-special (fn [{{:keys [op] :as fexpr} :fn :keys [env] :as expr} _expected _opts]
                           (case op
                             :var (:name fexpr)
                             :unanalyzed (let [{:keys [form]} fexpr]
                                           (when (symbol? form)
                                             (ana2/resolve-sym form env)))
                             nil)))

(defmethod invoke-special :default [expr expected _opts])

(defmethod invoke-special 'cljs.core.typed/print-env
  [{[{debug-string :form :as texpr} :as args] :args :as expr} expected opts]
  (assert (= 1 (count args)))
  (assert (string? debug-string))
  ;DO NOT REMOVE
  (pr-env/print-env* opts)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected
                     opts)))

; args are backwards if from inlining
(defmethod invoke-special 'cljs.core/instance?
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (assert (= 2 (count args)) "Wrong arguments to instance?")
  ; are arguments the correct way round?
  (assert (:from-js-op expr) "instance? without inlining NYI")
  (binding [vs/*current-expr* expr]
    (let [opts (assoc opts ::vs/current-env (:env expr))
          target-expr (first args)
          inst-of-expr (second args)
          varsym (when (#{:var} (:op inst-of-expr))
                   (-> inst-of-expr :name))
          _ (when-not varsym
              (err/int-error (str "First argument to instance? must be a datatype var "
                                (:op inst-of-expr))
                             opts))
          inst-of (c/DataType-with-unknown-params varsym opts)
          cexpr (check-expr target-expr nil opts)
          expr-tr (expr-type cexpr)
          final-ret (ret (r/JSBoolean-maker)
                         (fo/-FS (fo/-filter-at inst-of (ret-o expr-tr))
                                 (fo/-not-filter-at inst-of (ret-o expr-tr))))]
      (assoc expr
             expr-type final-ret))))

;=
(defmethod invoke-special 'cljs.core/= 
  [{:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv #(check-expr % nil opts) args)]
    (-> expr
        (update :fn check-expr nil opts)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected)))))


;only local bindings are immutable, vars/js do not partipate in occurrence typing
(defn js-var-result [expr vname expected opts]
  {:pre [((every-pred symbol? namespace) vname)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (let [t (var-env/type-of vname opts)]
      (below/maybe-check-below
        (ret t)
        expected
        opts))))

(defn check-var
  [{vname :name :as expr} expected opts]
  (impl/assert-cljs opts)
  (assoc expr expr-type
         (js-var-result expr vname expected opts)))

;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form :clojure.core.typed/loop [expr expected opts] (special-loop/check-special-loop expr expected opts))

(defmethod internal-special-form :default
  [expr expected opts]
  (err/int-error (str "No such internal form: " (ast-u/emit-form-fn expr opts)) opts))



(defmethod -check ::tana2/ns
  [expr expected opts]
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected
                     opts)))

(defmethod -check ::tana2/ns*
  [expr expected opts]
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret r/-any)
                       expected
                       opts)))


;; adding a bunch of missing methods: 

(defn fail-empty [expr]
  (throw (Exception. (str "Not implemented, yet: " (:op expr)))))

(defn check-new
  [{ctor :class :keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (impl/assert-cljs opts)
  (let [;; TODO check ctor
        cargs (mapv #(check-expr % nil opts) args)]
    (u/tc-warning "`new` special form is Unchecked" opts)
    (-> expr
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             ;; TODO actual checks
                             (r/ret (r/-unchecked 'new))
                             expected
                             opts)))))

;; TODO does this actually work?
#_
(defmethod -check ::tana2/case
  [{:keys [test nodes default :as expr]} expected]
  (chk/check ;;TODO what `check` is this?
   {:op :case
    :test test
    :default default
    :tests (mapcat :tests nodes)
    :thens (map :then nodes)}
   expected))

(defmethod -check ::tana2/case-node
  [expr expected]
  (fail-empty expr))

(defmethod -check ::tana2/case-test
  [expr expected]
  (fail-empty expr))

(defmethod -check ::tana2/case-then
  [expr expected]
  (fail-empty expr))

;TODO
(defmethod -check ::tana2/defrecord
  [expr expected opts]
  (u/tc-warning "`defrecord` special form is Unchecked" opts)
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected
                       opts)))

(defmethod -check ::tana2/deftype
  [expr expected opts]
  (u/tc-warning "`deftype` special form is Unchecked" opts)
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected
                       opts)))

; see clojure.core.typed.check.dot-cljs
;; TODO check
(defmethod -check ::tana2/host-call
  [{:keys [method target args] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [ctarget (check-expr target nil opts)
        cargs (mapv #(check-expr % nil opts) args)]
    #_(dot/check-dot ...)
    (u/tc-warning "`.` special form is Unchecked" opts)
    (assoc expr 
           :target ctarget
           :args cargs
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected
                         opts))))

; see clojure.core.typed.check.dot-cljs
;; TODO check
(defmethod -check ::tana2/host-field
  [{:keys [target] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [ctarget (check-expr target nil opts)]
    #_(dot/check-dot ...)
    (u/tc-warning "`.` special form is Unchecked" opts)
    (assoc expr 
           :target ctarget
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected
                         opts))))

;; TODO check
(defmethod -check ::tana2/js-array
  [{:keys [items] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [citems (mapv #(check-expr % nil opts) items)]
    #_(dot/check-dot ...)
    (u/tc-warning "`#js []` special form is Unchecked" opts)
    (assoc expr 
           :items citems
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected
                         opts))))

(defmethod -check ::tana2/js-object
  [{:keys [keys vals] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [cvals (mapv #(check-expr % nil opts) vals)]
    (assoc expr
           :vals cvals
           u/expr-type (below/maybe-check-below
                         (r/ret (r/JSObj-maker (zipmap (map keyword keys)
                                                       (map (comp r/ret-t u/expr-type) cvals)))
                                (fo/-true-filter))
                         expected
                         opts))))

; TODO check
(defmethod -check ::tana2/js-var
  [{:keys [name] :as expr} expected opts]
  (u/tc-warning (str "Assuming JS variable is unchecked " name) opts)
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected
                       opts)))


; TODO check
(defn check-the-var
  [expr expected opts]
  (impl/assert-cljs opts)
  (u/tc-warning "`var` special form is Unchecked" opts)
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected
                       opts)))

(defn check-expr
  ([{:keys [env] :as expr} expected opts]
   (loop [expr expr
          fuel 1000]
     (when (neg? fuel) (prn `check-expr "infinite loop"))
     #_
     (prn `check-expr "op" (:op expr) (:form expr)
          cljs-ana/*cljs-ns*
          )
     ;; should really be a map. worth asserting this at some point.
     (assert (not (symbol? (-> expr :env :ns)))
             (str "MALFORMED :env :ns " (pr-str (-> expr :env :ns))))
     (if (= :unanalyzed (:op expr))
       (do @*register-exts
           (or (maybe-check-unanalyzed expr expected opts)
               (recur (tana2/analyze-outer expr) (max -1 (dec fuel)))))
       (binding [vs/*current-expr* expr]
         (-check expr expected (update opts ::vs/current-env #(if (:line env) env %))))))))

(defn check-top-level
  "Type check a top-level form at an expected type, returning a
  fully analyzed core.typed.analyzer AST node (ie., containing no :unanalyzed nodes)
  with a u/expr-type entry giving its TCResult type, and a :result entry
  holding its evaluation result."
  ([form expected {:keys [env] :as opt} opts]
   ;(prn "check-top-level" form)
   ;(prn "*ns*" *ns*)
   ;(prn "*cljs-ns*" cljs-ana/*cljs-ns*)
   ;; TODO any bindings needed to be pinned here?
   (binding [ana2/scheduled-passes {:pre identity
                                    :post identity
                                    :init-ast identity}]
     (let [opts (-> opts
                    (assoc ::check/check-expr check-expr)
                    (assoc ::vs/lexical-env (lex/init-lexical-env)))
           cexpr (uc/with-cljs-typed-env
                   (-> form
                       (unanalyzed-top-level (or env (ana-api/empty-env)))
                       (check-expr expected opts)))]
       (flush-analysis-side-effects cexpr opts)
       cexpr))))

(defn check-ns1
  "Type checks an entire namespace."
  ([ns env {::vs/keys [check-config] :as opts}]
   (uc/with-cljs-typed-env
     (let [env (or env (ana-api/empty-env))
           res (coerce/ns->URL ns opts)]
       (assert res (str "Can't find " ns " in classpath"))
       (let [filename (str res)
             path     (.getPath res)]
         (uc/with-analyzer-bindings*
           path
           (fn []
             (with-open [rdr (io/reader res)]
               (let [pbr (readers/indexing-push-back-reader
                           (java.io.PushbackReader. rdr) 1 filename)
                     data-readers (merge tags/*cljs-data-readers*
                                         (cljs-ana/load-data-readers))
                     eof (Object.)
                     read-opts (cond-> {:eof eof :features #{:cljs}}
                                 (.endsWith filename "cljc") (assoc :read-cond :allow))]
                 (loop []
                   (let [form (binding [*ns* (do (when (:check-form-eval check-config)
                                                   ;; see clj implementation
                                                   (err/nyi-error ":check-form-eval in CLJS" opts))
                                                 (create-ns cljs-ana/*cljs-ns*))
                                        reader/*data-readers* data-readers
                                        reader/*alias-map* (uc/get-aliases)]
                                (reader/read read-opts pbr))]
                     (when-not (identical? form eof)
                       (check-top-level form nil {}
                                        #_ ;;TODO
                                        {:env (assoc env :ns (ns-name *ns*))
                                         :top-level-form-string sform
                                         :ns-form-string ns-form-str}
                                        opts)
                       (recur)))))))))))))

(defn check-ns-and-deps [nsym opts] (cu/check-ns-and-deps nsym check-ns1 opts))

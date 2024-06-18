;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check-form
  (:refer-clojure :exclude [delay])
  (:require [clojure.core.cache :as cache]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-utils-platform-specific :as plat-con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.repl :as repl]
            [typed.clj.analyzer.passes.beta-reduce :as beta-reduce]
            [typed.clj.checker.check :as chk]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.reset-caches :as reset-caches]
            [typed.cljc.analyzer :as ana]
            [typed.cljc.checker.lex-env :as lex-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])
  (:import (clojure.lang ExceptionInfo)))

(def *register-clj-anns (delay (configs/register-clj-config-anns)))
(def *register-cljs-anns (delay (configs/register-cljs-config-anns)))

;; (check-form-info config-map form & kw-args)
;; 
;; Takes a configuration map which different implementations can customize
;; (via eg. clojure.core.typed.check-form-{clj,cljs}), a form to type check
;; and keyword arguments propagated from core.typed users
;; (via eg. {clojure,cljs}.core.typed/check-form-info).
;;
;; Also see docstrings for clojure.core.typed/check-form-info
;; and cljs.core.typed/check-form-info.
;;
;; 
;; Takes config-map as first argument:
;;  Mandatory
;; - :check-top-level    function taking form and expected type and returns a checked AST.
;;
;;  Optional
;; - :eval-out-ast  function taking checked AST which evaluates it and returns the AST
;;                  with a :result entry attached, the result of evaluating it,
;;                  if no type errors occur.
;; - :unparse-ns    namespace in which to pretty-print type.  (FIXME Currently unused)
;; - :emit-form     function from AST+opts to equivalent form, returned in :out-form entry.
;; - :runtime-check-expr    function taking AST and expected type and returns an AST with inserted
;;                          runtime checks.
;; - :runtime-infer-expr    function taking AST and expected type and returns an AST with inserted
;;                          runtime instrumentation.
;; - :should-runtime-infer?  If true, instrument this expression for runtime inference.
;; - :custom-expansions?     If true, we are using custom expansions to type check forms.
;;
;;  (From here, copied from clojure.core.typed/check-form-info)
;; Keyword arguments
;;  Options
;;  - :expected        Type syntax representing the expected type for this form
;;                     type-provided? option must be true to utilise the type.
;;  - :type-provided?  If true, use the expected type to check the form.
;;  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
;;                     of (Map '{:line Int :column Int :file Str} Str).
;;  - :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,
;;                     annotated with the static types inferred after checking.
;;                     If a fatal type error occurs, :checked-ast is nil.
;;  - :bindings-atom   an atom which contains a value suitable for with-bindings.
;;                     Will be updated during macroexpansion and evaluation.
;;  - :beta-limit      A natural integer which denotes the maximum number of beta reductions
;;                     the type system can perform.
;;  - :check-config    Map of options for vs/*check-config*
;;  
;;  Default return map
;;  - :ret             TCResult inferred for the current form
;;  - :out-form        The macroexpanded result of type-checking, if successful. 
;;  - :result          The evaluated result of :out-form, unless :no-eval is provided.
;;  - :ex              If an exception was thrown during evaluation, this key will be present
;;                     with the exception as the value.
;;  DEPRECATED
;;  - :delayed-errors  A sequence of delayed errors (ex-info instances)
;;  - :profile         Use Timbre to profile the type checker. Timbre must be
;;                     added as a dependency.
(defn check-form-info
  [{:keys [check-top-level 
           custom-expansions?
           emit-form 
           env
           eval-out-ast 
           runtime-check-expr
           runtime-infer-expr 
           should-runtime-infer?
           instrument-infer-config
           unparse-ns
           analyze-bindings-fn] :as m1}
   form {:keys [expected-ret expected type-provided?
                checked-ast no-eval bindings-atom beta-limit
                check-config]
         {:keys [check-form-eval]
          :or {check-form-eval :after} :as check-config} :check-config
         :as opt}
   opts]
  {:pre [(nil? analyze-bindings-fn)
         ((some-fn nil? plat-con/atom?) bindings-atom)
         ((some-fn nil? symbol?) unparse-ns)
         (map? opt)
         (map? check-config)]}
  (assert (not (and expected-ret type-provided?)))
  (assert opts)
  (assert (not (:opts m1)))
  (do
    (impl/impl-case opts
      :clojure @*register-clj-anns
      :cljs @*register-cljs-anns)
    (reset-caches/reset-caches)
    (binding [vs/*in-check-form* true
              ;; custom expansions might not even evaluate
              vs/*can-rewrite* (not custom-expansions?)
              vs/*custom-expansions* custom-expansions?]
      (let [delayed-errors (err/-init-delayed-errors)
            opts (-> opts
                     (assoc ::vs/lexical-env (lex-env/init-lexical-env))
                     (assoc ::vs/already-checked (atom #{}))
                     (assoc ::vs/delayed-errors delayed-errors))
            expected (or
                       expected-ret
                       (when type-provided?
                         (r/ret (binding [prs/*parse-type-in-ns* unparse-ns]
                                  (prs/parse-type expected opts)))))
            delayed-errors-fn (fn [] (seq @delayed-errors))
            file-mapping-atom (atom [])
            should-runtime-check? (and runtime-check-expr
                                       (u/should-runtime-check-ns? *ns*))
            _ (assert (not (and should-runtime-infer? (not runtime-infer-expr)))
                      "Missing runtime inference function when inference is forced.")
            should-runtime-infer? (and runtime-infer-expr
                                       (or (u/should-runtime-infer-ns? *ns*)
                                           should-runtime-infer?))
            ;_ (prn "should-runtime-check?" should-runtime-check?)
            ;_ (prn "should-runtime-infer?" should-runtime-infer?)
            ;_ (prn "ns" *ns*)
            _ (assert (and (not should-runtime-infer?)
                           (not should-runtime-check?))
                      "FIXME")
            #_#_
            check-expr (or (when should-runtime-infer?
                             #(binding [vs/*instrument-infer-config* instrument-infer-config]
                                (runtime-infer-expr %1 %2)))
                           (when should-runtime-check?
                             runtime-check-expr)
                           check-expr)
            terminal-error (atom nil)
            ;_ (prn "before c-ast")
            c-ast (try
                    (binding [vs/*check-config* check-config]
                      (check-top-level form expected {} opts))
                    (catch Throwable e
                      (let [e (if (some-> e ex-data err/tc-error?)
                                (try
                                  ;(prn "printing errors")
                                  (err/print-errors! (vec (concat (delayed-errors-fn) [e])))
                                  (catch Throwable e
                                    e))
                                (throw e))]
                        ;(prn "reset terminal-error")
                        (reset! terminal-error e)
                        nil)))
            ;_ (prn "err" @terminal-error)
            res (some-> c-ast u/expr-type)
            delayed-errors (delayed-errors-fn)
            ex @terminal-error]
        (cond->
          {:delayed-errors (vec delayed-errors)
           :ret (or res (r/ret r/-error))}

          ex (assoc :ex ex)

          ;; fatal type error = nil
          checked-ast (assoc :checked-ast c-ast)

          (and (impl/checking-clojure? opts)
               (empty? delayed-errors)
               (not ex))
          (into (select-keys c-ast [:result]))

          (and c-ast emit-form (not ex))
          (assoc :out-form (emit-form c-ast opts)))))))

(defn check-form*
  [{:keys [impl unparse-ns] :as config} form expected type-provided? opt opts]
  {:pre [(map? opt)]}
  (let [{:keys [ex delayed-errors ret]} (check-form-info config form
                                                         (into {:expected expected 
                                                                :type-provided? type-provided?}
                                                               opt)
                                                         opts)]
    (if-let [errors (seq delayed-errors)]
      (err/print-errors! errors)
      (if ex
        (throw ex)
        (prs/unparse-TCResult-in-ns ret unparse-ns opts)))))

(defn check-form-info-with-config
  [config form opt opts]
  {:pre [(map? config)
         (map? opt)]}
  (impl/with-impl (:impl config)
    ((:check-form-info config) config
     form opt opts)))

(defn check-form*-with-config
  [config form expected type-provided? opt opts]
  {:pre [(map? opt)]}
  (impl/with-impl (:impl config)
    ((:check-form* config) config
     form expected type-provided? opt opts)))

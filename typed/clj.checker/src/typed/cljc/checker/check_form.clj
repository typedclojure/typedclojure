;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.check-form
  (:refer-clojure :exclude [delay])
  (:require [clojure.core.cache :as cache]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-utils-platform-specific :as plat-con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.repl :as repl]
            [typed.clj.checker.check :as chk]
            [typed.clj.checker.parse-unparse :as prs]
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
;;  - :ex              A fatal exception thrown during checking, if any.
;;  - :type-errors     A non-empty vector of delayed errors
(defn check-form-info
  [{:keys [check-top-level 
           emit-form 
           env
           eval-out-ast 
           runtime-check-expr
           unparse-ns] :as m1}
   form {:keys [expected-ret expected type-provided?
                checked-ast no-eval bindings-atom beta-limit
                check-config verbose-types trace]
         {:keys [check-form-eval]
          :or {check-form-eval :after} :as check-config} :check-config
         :as opt}
   opts]
  {:pre [(nil? (:analyze-bindings-fn m1))
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
    (let [type-errors (err/-init-type-errors)
          opts (-> opts
                   (assoc ::vs/can-rewrite true)
                   (assoc ::vs/lexical-env (lex-env/init-lexical-env))
                   (assoc ::vs/already-checked (atom #{}))
                   (assoc ::vs/type-errors type-errors)
                   (assoc ::prs/parse-type-in-ns unparse-ns)
                   (assoc ::vs/check-config check-config)
                   (assoc ::vs/verbose-types verbose-types)
                   (assoc ::vs/trace trace)
                   (assoc ::vs/in-check-form true))
          expected (or
                     expected-ret
                     (when type-provided?
                       (r/ret (prs/parse-type expected opts))))
          type-errors-fn (fn [] (seq @type-errors))
          file-mapping-atom (atom [])
          should-runtime-check? (and runtime-check-expr
                                     (u/should-runtime-check-ns? *ns*))
          terminal-error (atom nil)
          ;_ (prn "before c-ast")
          c-ast (try
                  (check-top-level form expected {} opts)
                  (catch Throwable e
                    (let [e e]
                      ;(prn "reset terminal-error")
                      (reset! terminal-error e)
                      nil)))
          ;_ (prn "err" @terminal-error)
          res (some-> c-ast u/expr-type)
          type-errors (type-errors-fn)
          ex @terminal-error]
      (cond->
        {:type-errors (vec type-errors)
         :ret (or res (r/ret r/-error))}

        ex (assoc :ex ex)

        ;; fatal type error = nil
        checked-ast (assoc :checked-ast c-ast)

        (and (impl/checking-clojure? opts)
             (empty? type-errors)
             (not ex))
        (into (select-keys c-ast [:result]))

        (and c-ast emit-form (not ex))
        (assoc :out-form (emit-form c-ast opts))))))

(defn check-form*
  [{:keys [impl unparse-ns] :as config} form expected type-provided? opt opts]
  {:pre [(map? opt)]}
  (let [{:keys [ex type-errors ret]} (check-form-info config form
                                                         (into {:expected expected 
                                                                :type-provided? type-provided?}
                                                               opt)
                                                         opts)]
    (if ex
      (do (when (seq type-errors)
            (let [s (err/print-errors->summary-message type-errors opts)]
              (binding [*out* *err*]
                (println s)
                (println "A fatal error was thrown during type checking, rethrowing."))))
          (throw ex))
      (if (seq type-errors)
        (err/print-errors! type-errors opts)
        (prs/unparse-TCResult-in-ns ret unparse-ns opts)))))

(defn check-form-info-with-config
  [config form opt opts]
  {:pre [(map? config)
         (map? opt)]}
  ((:check-form-info config) config
   form opt opts))

(defn check-form*-with-config
  [config form expected type-provided? opt opts]
  {:pre [(map? opt)]}
  ((:check-form* config) config
   form expected type-provided? opt opts))

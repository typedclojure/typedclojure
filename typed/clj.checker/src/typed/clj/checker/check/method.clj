;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.check.method
  (:require [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.check.type-hints :as type-hints]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.ast-utils :as ast-u]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.utils :as cu]
            [typed.clj.checker.method-override-env :as mth-override]
            [typed.cljc.runtime.env :as env]))

(defn method->Type [expr {:keys [method-override] :as opt} opts]
  {:post [((some-fn nil? r/Type?) (:rfin-type %))]}
  (let [method (when (#{:static-call :instance-call} (:op expr))
                 (cu/MethodExpr->Method expr opts))
        msym (cu/MethodExpr->qualsym expr opts)
        rfin-type (or method-override
                      (when msym (mth-override/get-method-override (env/checker opts) msym opts))
                      (some-> method (cu/Method->Type opts)))]
    {:rfin-type rfin-type
     :method method
     :msym msym}))

(defn check-method [expr expected {:keys [method-override] :as opt} opts]
  {:pre [(#{:static-method :instance-method} (:op expr))
         ((some-fn nil? r/TCResult?) expected)
         ((some-fn nil? r/Type?) method-override)]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [{:keys [method msym rfin-type]} (method->Type expr opt opts)]
    (assert rfin-type)
    (throw (ex-info (str "TODO " `check-method " " (:op expr))))))

;[MethodExpr Type Any -> Expr]
(defn check-invoke-method [{method-name :method :keys [args env] :as expr} expected
                           {:keys [method-override] :as opt} opts]
  {:pre [(#{:static-call :instance-call} (:op expr))
         (not (:ctarget opt)) ;not supported
         (not (:cargs opt))   ;not supported
         ((some-fn nil? r/TCResult?) expected)
         ((some-fn nil? r/Type?) method-override)]
   :post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [opts (-> opts
                 (assoc ::vs/current-env env)
                 (assoc ::vs/current-expr expr))
        inst? (= :instance-call (:op expr))
        {:keys [method msym rfin-type]} (method->Type expr opt opts)
        ctarget (:instance expr)]
    (if-not rfin-type
      (err/tc-delayed-error (str "Unresolved " (if inst? "instance" "static") 
                               " method invocation " method-name "." 
                               (type-hints/suggest-type-hints 
                                 method-name 
                                 (some-> ctarget u/expr-type r/ret-t)
                                 (map (comp r/ret-t u/expr-type) args)
                                 {}
                                 opts)
                               "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            {:form (ast-u/emit-form-fn expr opts)
                             :return (merge
                                       (assoc expr 
                                              u/expr-type (cu/error-ret expected))
                                       (when ctarget {:instance ctarget}))}
                            opts)
      (let [_ (when inst?
                (let [target-class (resolve (:declaring-class method))
                      _ (assert (class? target-class))]
                  ;                (prn "check target" (prs/unparse-type (r/ret-t (u/expr-type ctarget)) opts)
                  ;                     (prs/unparse-type (c/RClass-of (coerce/Class->symbol (resolve (:declaring-class method))) nil opts) opts))
                  (when-not (sub/subtype? (r/ret-t (u/expr-type ctarget)) (c/RClass-of-with-unknown-params target-class opts) opts)
                    (err/tc-delayed-error (str "Cannot call instance method " (cu/Method->symbol method)
                                             " on type " (pr-str (prs/unparse-type (r/ret-t (u/expr-type ctarget)) opts)))
                                          {:form (ast-u/emit-form-fn expr opts)}
                                          opts))))
            result-type (funapp/check-funapp expr args (r/ret rfin-type) (map u/expr-type args) expected {} opts)
            _ (when expected
                ;;FIXME check filters and object
                (when-not (sub/subtype? (r/ret-t result-type) (r/ret-t expected) opts)
                  (err/tc-delayed-error (str "Return type of " (if inst? "instance" "static")
                                             " method " (cu/Method->symbol method)
                                             " is " (prs/unparse-type (r/ret-t result-type) opts)
                                             ", expected " (prs/unparse-type (r/ret-t expected) opts) "."
                                             (when (sub/subtype? r/-nil (r/ret-t result-type) opts)
                                               (str "\n\nHint: Use `non-nil-return` and `nilable-param` to configure "
                                                    "where `nil` is allowed in a Java method call. `method-type` "
                                                    "prints the current type of a method.")))
                                        {:form (ast-u/emit-form-fn expr opts)}
                                        opts)))]
        (assoc expr
               u/expr-type result-type)))))

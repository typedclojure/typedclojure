;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure clojure.core.typed.check.dot-cljs
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljs.checker.jsnominal-env :as jsnom]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check.funapp :as funapp]
            [clojure.core.typed.errors :as err]))

(defn check-dot [check {:keys [target field method args] :as dot-expr} expected opts]
  (let [ctarget (check target)
        target-t (-> ctarget u/expr-type r/ret-t)
        resolved (let [t (c/fully-resolve-type target-t opts)]
                   ;TODO DataType
                   (when ((some-fn r/JSNominal? 
                                   r/JSString?
                                   #_r/DataType?) t)
                     t))]
    (if resolved
      (cond
        field
        (let [field-type (cond
                           (r/JSString? resolved)
                           (jsnom/get-field 'string nil field)
                           (r/JSNominal? resolved)
                           (jsnom/get-field (:name resolved) (:poly? resolved) field))
              _ (assert field-type (str "Don't know how to get field " field
                                        " from " (prs/unparse-type resolved opts)))]
          (assoc dot-expr
                 u/expr-type (r/ret field-type)))
        :else
        (let [method-type (cond
                            (r/JSString? resolved)
                            (jsnom/get-method 'string nil method)
                            (r/JSNominal? resolved)
                            (jsnom/get-method (:name resolved) (:poly? resolved) method))
              _ (assert method-type (str "Don't know how to call method " method
                                         " from " (prs/unparse-type resolved opts)))
              cargs (mapv check args)
              actual (funapp/check-funapp nil cargs (r/ret method-type) (map u/expr-type cargs)
                                          expected {} opts)]
          (assoc dot-expr
                 u/expr-type actual)))
      (err/tc-delayed-error (str "Don't know how to use type " (prs/unparse-type target-t opts)
                                 " with "
                                 (if field (str "field " field)
                                   (str "method " method)))
                            {:return 
                             (assoc dot-expr
                                    u/expr-type (r/ret (or (when expected
                                                             (r/ret-t expected))
                                                           (r/TCError-maker))))}
                            opts))))

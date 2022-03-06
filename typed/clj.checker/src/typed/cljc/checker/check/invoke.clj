;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.invoke
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.invoke-kw :as invoke-kw]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]))

(defn normal-invoke [check-fn expr fexpr args expected & {:keys [cfexpr cargs]}]
  {:pre [((some-fn nil? r/TCResult?) expected)
         (map? fexpr)
         (vector? args)
         ((some-fn nil?
                   (every-pred map? (comp r/TCResult? u/expr-type)))
          cfexpr)
         ((some-fn nil?
                   (con/vec-c? (comp r/TCResult? u/expr-type)))
          cargs)]}
  (let [cfexpr (or cfexpr (check-fn fexpr))
        cargs (or cargs (mapv check-fn args))
        ftype (u/expr-type cfexpr)
        argtys (map u/expr-type cargs)
        actual (funapp/check-funapp fexpr args ftype argtys expected {:expr expr})]
    (assoc expr
           :fn cfexpr
           :args cargs
           u/expr-type actual)))

(defn check-invoke [check-expr -invoke-special {fexpr :fn :keys [args env] :as expr} expected]
  {:pre [(#{:unanalyzed} (:op fexpr))]
   :post [(map? %)
          (-> % u/expr-type r/TCResult?)]}
  (or (-invoke-special expr expected)
      (when (and (keyword? (:form fexpr))
                 (#{1 2} (count args)))
        (let [{cfexpr :fn
               [ctarget cdefault] :args
               :as expr}
              (-> expr
                  (update :fn check-expr)
                  (update :args #(mapv check-expr %)))]
          (assoc expr
                 u/expr-type (invoke-kw/invoke-keyword 
                               expr
                               (u/expr-type cfexpr)
                               (u/expr-type ctarget)
                               (when cdefault
                                 (u/expr-type cdefault)) 
                               expected))))
      (normal-invoke check-expr expr fexpr args expected)))

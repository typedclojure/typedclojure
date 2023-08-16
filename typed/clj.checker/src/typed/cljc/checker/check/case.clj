;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.case
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.update :as update]
            [typed.clj.checker.tc-equiv :as equiv]))

(defn check-case-thens [check-fn target-ret tst-rets case-thens expected]
  {:pre [(r/TCResult? target-ret)
         (every? r/TCResult? tst-rets)
         (== (count tst-rets)
             (count case-thens))]
   :post [((every-pred vector?
                       (con/every-c? (comp #{:case-then} :op)))
           %)]}
  (letfn [(check-case-then [tst-ret {:keys [then] :as case-then}]
            (let [{{fs+ :then} :fl :as rslt} (equiv/tc-equiv := [target-ret tst-ret] nil)
                  flag+ (atom true)
                  env-thn (update/env+ (lex/lexical-env) [fs+] flag+)
                  _ (when-not @flag+
                      ;; FIXME should we ignore this branch?
                      (u/tc-warning "Local became bottom when checking case then"))
                  cthen (var-env/with-lexical-env env-thn
                          (check-fn then expected))]
              (assoc case-then
                     :then cthen
                     u/expr-type (u/expr-type cthen))))]
    (mapv check-case-then
      tst-rets 
      case-thens)))

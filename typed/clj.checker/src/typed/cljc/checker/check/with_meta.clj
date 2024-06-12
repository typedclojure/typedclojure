;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.with-meta
  (:require [typed.cljc.checker.check :as check]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.utils :as u]))

(defn visit-tail-pos [ast f opts]
  (let [rec #(visit-tail-pos % f opts)]
    (case (:op ast)
      :do (update ast :ret rec)
      ;; would be ambiguous when calculating whether to erase the :with-meta node
      :if (err/int-error "Not allowed :with-meta around :if" opts)
      (:let :letfn) (update ast :body rec)
      ;; probably possible to handle, but seems likely to never occur in practice
      :with-meta (err/int-error "Not allowed nested :with-meta" opts)
      (f ast))))

(defn check-with-meta
  [{:keys [expr meta] :as with-meta-expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [erase-atom (atom nil)
        expr (visit-tail-pos expr (fn [ast]
                                    (assoc ast ::erase-atom erase-atom))
                             opts)
        cexpr (check-expr expr expected opts)
        cmeta (check-expr meta nil opts)]
    (if @erase-atom
      cexpr
      (assoc with-meta-expr 
             :expr cexpr
             :meta cmeta
             u/expr-type (u/expr-type cexpr)))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core__doseq
  "Typing rules clojure.core/doseq"
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.check :refer [check-expr]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.clj.analyzer.utils :as ana-utils]
            [typed.clj.ext.clojure.core__for :as ext-for]
            [typed.clj.ext.clojure.core__let :as ext-let]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.if :as if]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))


;;==================
;; clojure.core/doseq

(defuspecial defuspecial__doseq
  "defuspecial implementation for clojure.core/doseq"
  [{ana-env :env :keys [form] :as expr} expected]
  (let [_ (assert (<= 2 (count form)) form)
        [args-syn & body-syns] (next form)
        {:keys [new-syms expanded-bindings prop-env ana-env reachable]}
        (ext-for/check-list-comprehension-binder 
          {:form form
           :args-syn args-syn
           :ana-env ana-env
           :prop-env (lex/lexical-env)})
        expr (-> expr
                 (update :form
                         (fn [form]
                           (-> (map-indexed
                                 (fn [i args-syn]
                                   ;; add back short-circuited args
                                   (if (= 1 i)
                                     expanded-bindings
                                     args-syn))
                                 form)
                               (with-meta (meta form))))))]
        (if-not reachable
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret r/-nil
                                      (fo/-false-filter))
                               expected))
          (let [cbody (var-env/with-lexical-env prop-env
                        (-> `(do ~@body-syns)
                            (ana2/unanalyzed ana-env)
                            check-expr))
                expr (-> expr
                         (update :form
                                 (fn [form]
                                   (-> form
                                       vec
                                       (subvec 0 2)
                                       (conj (emit-form/emit-form cbody))
                                       list*
                                       (with-meta (meta form))))))]
            (assoc expr
                   u/expr-type (below/maybe-check-below
                                 (r/ret r/-nil
                                        (fo/-false-filter))
                                 expected))))))

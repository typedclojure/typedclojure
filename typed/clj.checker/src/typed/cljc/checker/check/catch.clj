;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.catch
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.analyzer :as ana2]
            [clojure.core.typed.ast-utils :as ast-u]))

(defn check-catch [{handler :body :keys [local] :as expr} expected]
  (let [expr (-> expr
                 (update :class ana2/run-passes))
        ecls (ast-u/catch-op-class expr)
        local-sym (:name local)
        local-type (impl/impl-case
                     :clojure (c/RClass-of-with-unknown-params ecls)
                     :cljs (err/nyi-error "catch in CLJS"))
        chandler (lex/with-locals {local-sym local-type}
                   (check-expr handler expected))]
    (assoc expr
           :body chandler
           u/expr-type (u/expr-type chandler))))

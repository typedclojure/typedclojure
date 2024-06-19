;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.ext.clojure.core.typed__ann-form
  "Typing rules for clojure.core.typed ops."
  (:require [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

;; TODO clojure.core.typed.expand/expand-ann-form has ideas on improving error msgs
;; TODO move to defuspecial
(defn -unanalyzed-special__ann-form
  [{[_ body tsyn :as form] :form :keys [env] :as expr} expected {::check/keys [check-expr] :as opts}]
  (assert (= 3 (count form))
          (str "Incorrect number of arguments to ann-form: " form))
  (let [opts (assoc opts ::prs/parse-type-in-ns (cu/expr-ns expr opts))
        parsed-t (prs/parse-type tsyn opts)
        ;; TODO let users add expected filters etc
        this-expected (or (some-> expected (assoc :t parsed-t))
                          (r/ret parsed-t))
        _ (below/maybe-check-below
            this-expected
            expected
            opts)
        cret (-> body
                 (ana2/unanalyzed env)
                 (ana2/inherit-top-level expr)
                 (check-expr this-expected opts))]
    cret))

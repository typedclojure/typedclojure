;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core.typed
  "Typing rules for core.typed ops."
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.check :refer [check-expr]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial -unanalyzed-special]]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

;; ============================
;; clojure.core.typed/tc-ignore

;; TODO clojure.core.typed.expand/tc-ignore-typing-rule has ideas on improving error msgs
(defuspecial 'clojure.core.typed/tc-ignore
  [expr expected]
  (-> expr
      (assoc
        u/expr-type (below/maybe-check-below
                      (r/ret r/-any)
                      expected))))

;; ============================
;; clojure.core.typed/ann-form

;; TODO clojure.core.typed.expand/expand-ann-form has ideas on improving error msgs
(defmethod -unanalyzed-special 'clojure.core.typed/ann-form
  [{[_ body tsyn :as form] :form :keys [env] :as expr} expected]
  (assert (#{3} (count form))
          (str "Incorrect number of arguments to ann-form: " form))
  (let [parsed-t (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                   (prs/parse-type tsyn))
        ;; TODO let users add expected filters etc
        this-expected (or (some-> expected (assoc :t parsed-t))
                          (r/ret parsed-t))
        _ (below/maybe-check-below
            this-expected
            expected)
        cret (-> body
                 (ana2/unanalyzed env)
                 (ana2/inherit-top-level expr)
                 ;; TODO backend agnostic
                 (check-expr this-expected))]
    cret))

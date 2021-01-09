;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.ext.clojure.core.typed
  "Typing rules for Typed Clojure ops."
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.check :refer [defuspecial -unanalyzed-special]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defuspecial 'clojure.core.typed/tc-ignore
  [expr expected]
  (-> expr
      (assoc
        u/expr-type (below/maybe-check-below
                      (r/ret r/-any)
                      expected))))

;FIXME should propagate actual annotation into body like the special ann-form op does
#_
(defmethod -check-macro 'clojure.core.typed/ann-form
  [{[_ body tsyn :as form] :form :keys [env] :as expr} expected]
  (assert (#{3} (count form))
          (str "Incorrect number of arguments to ann-form: " form))
  (-> body
      (ana2/unanalyzed env)
      (check-expr (below/maybe-check-below
                    (r/ret (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                             (prs/parse-type tsyn)))
                    expected))))

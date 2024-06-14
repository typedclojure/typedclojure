;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core__assert
  "Typing rules clojure.core/assert"
  (:require [typed.cljc.checker.check :as check]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.analyzer :as ana2]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

;;======================
;; clojure.core/assert

(defuspecial defuspecial__assert
  "defuspecial implementation for clojure.core/assert"
  [expr expected {::check/keys [check-expr] :as opts}]
  (let [{:keys [check-form-eval]} vs/*check-config*]
    (when (= :never check-form-eval)
      (let [expr (binding [*assert* true]
                   (ana2/analyze-outer expr))]
        (check-expr expr expected opts)))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core__defn
  "Typing rules for clojure.core/defn"
  (:require [clojure.core.typed.internal :as internal]
            [typed.clj.checker.check :as chk]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

;;==================
;; clojure.core/defn

(defuspecial defuspecial__defn
  "defuspecial implementation for clojure.core/defn"
  [{:keys [form] :as expr} expected]
  (-> expr
      (update :form internal/add-defn-destructure-blame-form)
      ana2/analyze-outer
      (chk/check-expr expected)))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.clj.ext.clojure.core__defn
  "Typing rules for clojure.core/defn"
  (:require [clojure.core.typed.internal :as internal]
            [typed.cljc.checker.check :as check]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

;;==================
;; clojure.core/defn

(defuspecial defuspecial__defn
  "defuspecial implementation for clojure.core/defn"
  [{:keys [form] :as expr} expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (update :form internal/add-defn-destructure-blame-form)
      (ana2/analyze-outer opts)
      (check-expr expected opts)))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core
  "Typing rules for base Clojure distribution."
  (:require [clojure.core.typed.internal :as internal]
            [typed.clj.checker.check :refer [check-expr defuspecial -unanalyzed-special]]
            [typed.cljc.analyzer :as ana2]))

(defmacro install-unanalyzed-special [v impl]
  {:pre [(qualified-symbol? impl)]}
  `(defmethod -unanalyzed-special ~v
     [expr# expected#]
     ((requiring-resolve '~impl) expr# expected#)))

(defmacro install-defuspecial [v impl]
  {:pre [(qualified-symbol? impl)]}
  `(defuspecial ~v
     [expr# expected#]
     ((requiring-resolve '~impl) expr# expected#)))

;;==================
;; clojure.core/ns

(install-unanalyzed-special
  'clojure.core/ns
  typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns)

;;==================
;; clojure.core/defmacro

(install-defuspecial
  'clojure.core/defmacro
  typed.clj.ext.clojure.core__defmacro/defuspecial__defmacro)

;;==================
;; clojure.core/let

(install-defuspecial
  'clojure.core/let
  typed.clj.ext.clojure.core__let/defuspecial__let)

;;==================
;; clojure.core/for

(install-defuspecial
  'clojure.core/for
  typed.clj.ext.clojure.core__for/defuspecial__for)

;; ============================
;; clojure.core/fn

(defuspecial 'clojure.core/fn
  [expr expected]
  (-> expr
      (update :form internal/add-fn-destructure-blame-form)
      ana2/analyze-outer
      (check-expr expected)))

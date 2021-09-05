;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core
  "Typing rules for base Clojure distribution."
  (:require [typed.clj.checker.check :as chk]))

(defmacro install-unanalyzed-special [v impl]
  {:pre [(qualified-symbol? impl)]}
  `(defmethod chk/-unanalyzed-special ~v
     [expr# expected#]
     ((requiring-resolve '~impl) expr# expected#)))

(defmacro install-defuspecial [v impl]
  {:pre [(qualified-symbol? impl)]}
  `(chk/defuspecial ~v
     [expr# expected#]
     ((requiring-resolve '~impl) expr# expected#)))

;;==================
;; clojure.core/defmacro

(install-defuspecial
  'clojure.core/defmacro
  typed.clj.ext.clojure.core__defmacro/defuspecial__defmacro)

;;==================
;; clojure.core/defn

(install-defuspecial
  'clojure.core/defn
  typed.clj.ext.clojure.core__defn/defuspecial__defn)

;;==================
;; clojure.core/doseq

(install-defuspecial
  'clojure.core/doseq
  typed.clj.ext.clojure.core__doseq/defuspecial__doseq)

;; ============================
;; clojure.core/fn

(install-defuspecial
  'clojure.core/fn
  typed.clj.ext.clojure.core__fn/defuspecial__fn)

;;==================
;; clojure.core/for

(install-defuspecial
  'clojure.core/for
  typed.clj.ext.clojure.core__for/defuspecial__for)

;;==================
;; clojure.core/let

(install-defuspecial
  'clojure.core/let
  typed.clj.ext.clojure.core__let/defuspecial__let)

;;==================
;; clojure.core/ns

(install-unanalyzed-special
  'clojure.core/ns
  typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns)

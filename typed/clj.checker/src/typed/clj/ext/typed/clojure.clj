;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.typed.clojure
  "Typing rules for typed.clojure ops."
  (:require [typed.clj.checker.check.unanalyzed :refer [install-defuspecial install-unanalyzed-special]]))

;; ============================
;; typed.clojure/ann

(install-defuspecial
  'typed.clojure/ann
  'typed.clj.ext.clojure.core.typed__ann/defuspecial__ann)

;; ============================
;; typed.clojure/ann-form

(install-unanalyzed-special
  'typed.clojure/ann-form
  'typed.cljc.ext.clojure.core.typed__ann-form/-unanalyzed-special__ann-form)

;; ============================
;; typed.clojure/inst

(install-unanalyzed-special
  'clojure.core.typed/inst
  'typed.cljc.ext.clojure.core.typed__inst/-unanalyzed-special__inst)

;; ============================
;; typed.clojure/fn

(install-unanalyzed-special
  'typed.clojure/fn
  'typed.cljc.ext.clojure.core.typed__fn/-unanalyzed-special__fn)

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljs.ext.cljs.core
  "Typing rules for cljs.core."
  (:require [typed.cljs.checker.check.unanalyzed :as un-cljs]))

;;==================
;; cljs.core/defprotocol

(un-cljs/install-defuspecial
  'cljs.core/defprotocol
  'typed.clj.ext.clojure.core__defprotocol/defuspecial__defprotocol)

;;==================
;; cljs.core/implements?

(un-cljs/install-defuspecial
  'cljs.core/implements?
  'typed.cljs.ext.cljs.core__implements_huh/defuspecial__implements?)

;; ============================
;; clojure.core/fn

(un-cljs/install-defuspecial
  'cljs.core/fn
  'typed.clj.ext.clojure.core__fn/defuspecial__fn)

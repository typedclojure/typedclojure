;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljs.ext.cljs.core.typed
  "Typing rules for cljs.core.typed ops."
  (:require [typed.cljs.checker.check.unanalyzed :refer [install-defuspecial install-unanalyzed-special]]))

;;==================
;; cljs.core.typed/tc-ignore

(install-defuspecial
  'cljs.core.typed/tc-ignore
  'typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore)

;; ============================
;; clojure.core.typed/ann-form

(install-unanalyzed-special
  'cljs.core.typed/ann-form
  'typed.cljc.ext.clojure.core.typed__ann-form/-unanalyzed-special__ann-form)

;; ============================
;; clojure.core.typed/inst

(install-unanalyzed-special
  'cljs.core.typed/inst
  'typed.cljc.ext.clojure.core.typed__inst/-unanalyzed-special__inst)

;; ============================
;; clojure.core.typed/fn

(install-unanalyzed-special
  'cljs.core.typed/fn
  'typed.cljc.ext.clojure.core.typed__fn/-unanalyzed-special__fn)

(install-unanalyzed-special
  'clojure.core.typed.macros/fn
  'typed.cljc.ext.clojure.core.typed__fn/-unanalyzed-special__fn)

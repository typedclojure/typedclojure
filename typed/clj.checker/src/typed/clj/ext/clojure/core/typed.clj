;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core.typed
  "Typing rules for clojure.core.typed ops."
  (:require [typed.clj.checker.check.unanalyzed :refer [install-defuspecial install-unanalyzed-special]]))

;; ============================
;; clojure.core.typed/tc-ignore

(install-defuspecial
  'clojure.core.typed/tc-ignore
  'typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore)

;; ============================
;; clojure.core.typed/ann-form

(install-unanalyzed-special
  'clojure.core.typed/ann-form
  'typed.cljc.ext.clojure.core.typed__ann-form/-unanalyzed-special__ann-form)

;; ============================
;; clojure.core.typed/pred

(install-unanalyzed-special
  'clojure.core.typed/pred
  'typed.cljc.ext.clojure.core.typed__pred/-unanalyzed-special__pred)

;; ============================
;; clojure.core.typed/inst

(install-unanalyzed-special
  'clojure.core.typed/inst
  'typed.cljc.ext.clojure.core.typed__inst/-unanalyzed-special__inst)

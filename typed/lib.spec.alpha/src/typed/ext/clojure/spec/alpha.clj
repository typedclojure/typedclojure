;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.ext.clojure.spec.alpha
  "Typing rules for spec.alpha."
  (:require [typed.clj.checker.check.unanalyzed :refer [install-defuspecial]]))

;;=======================
;; clojure.spec.alpha/def

(install-defuspecial
  'clojure.spec.alpha/def
  'typed.ext.clojure.spec.alpha__def/defuspecial__def)

;;=======================
;; clojure.spec.alpha/fdef

(install-defuspecial
  'clojure.spec.alpha/fdef
  'typed.ext.clojure.spec.alpha__fdef/defuspecial__fdef)

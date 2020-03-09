;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.spec.clojure.core
  "Public API:
  
  Spec macros for clojure.core."
  (:require [clojure.alpha.spec :as s]
            [typed.clj.spec :as t]))

(defmacro reduced-spec
  "Example: (reduced-spec integer?)
  
  Spec that conforms the result of (clojure.core/reduced x) where x
  is a value of spec s."
  [s]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(reduced-spec ~s))))

(load "/typed/spec_impl/clojure/core")

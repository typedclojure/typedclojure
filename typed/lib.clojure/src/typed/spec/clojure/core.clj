;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.spec.clojure.core
  "Public API:
  
  Spec macros for clojure.core."
  (:require [clojure.alpha.spec :as s]))

(defmacro reduced-spec
  "Example: (reduced-spec integer?)
  
  Spec that conforms the result of (clojure.core/reduced x) where x
  is a value of spec s."
  [s]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(reduced-spec ~s))))

(defmacro atom-spec
  "Example: (atom-spec :read integer? :write integer?)

  Generates atoms that accept values conforming to :write (via
  inputs of reset!, swap! etc.), and provides values conforming to :read
  (via deref, return of old val in swap-vals! etc.).

  :write is contravariant (an input to the atom) and :read is covariant
  (an output of the atom).

  Conforms atoms that deref values conforming to :read.
  Generates atoms that conform updated values via reset!/swap! etc., to :write.

  Defaults:
  - :write    any?
  - :read     any?
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(atom-spec ~@args))))

(load "/typed/spec_impl/clojure/core")

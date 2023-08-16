;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.malli.generator
  "Public API for typed `malli.generator` ops."
  (:require [malli.generator :as mg]))

(defmacro generate [t & args]
  `(mg/generate
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
    ~@args))

(defmacro sample [t & args]
  `(mg/sample
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
    ~@args))

(defmacro generator [t & args]
  `(mg/generator
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
    ~@args))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.malli.json-schema
  "Public API for typed `malli.json-schema` ops."
  (:refer-clojure :exclude [requiring-resolve])
  (:require [malli.json-schema :as mj]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(defmacro transform [t]
  `(mj/transform
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)))

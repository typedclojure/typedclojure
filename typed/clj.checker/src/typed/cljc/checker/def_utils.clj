;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.def-utils
  (:refer-clojure :exclude [defrecord defprotocol definterface])
  (:require [clojure.core :as core]
            [typed.cljc.runtime.env-utils :as env-utils]))

(defmacro defprotocol [name & args]
  `(do
     (env-utils/invalidate-parsed-types!)
     (core/defprotocol ~name ~@args)))

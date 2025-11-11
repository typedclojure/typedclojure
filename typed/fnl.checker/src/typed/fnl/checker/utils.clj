;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.fnl.checker.utils
  (:require [typed.clojure :as t]
            [typed.fnl.checker.env :as fnl-env]
            [typed.fnl.analyzer :as ana]))

(defn ->opts []
  (into (fnl-env/fnl-opts)
        (ana/default-opts)))

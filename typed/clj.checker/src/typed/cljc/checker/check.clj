;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check
  (:require [typed.cljc.analyzer :as ana2]))

(defn ->check-expr [check-expr opts]
  (let [opts->check-expr (fn opts->check-expr [opts]
                           (let [opts (delay (assoc opts ::check-expr (opts->check-expr opts)))]
                             (fn
                               ([expr] (check-expr expr nil @opts))
                               ([expr expected] (check-expr expr expected @opts))
                               ([expr expected opts] (check-expr expr expected (assoc opts ::check-expr (opts->check-expr opts)))))))]
    (opts->check-expr opts)))

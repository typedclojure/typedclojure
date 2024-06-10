;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.reset-env
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.clj.checker.base-env :as bse-clj]
            [typed.cljc.checker.ns-options :as ns-opts]
            [clojure.core.typed.current-impl :as impl]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.clj.checker.mm-env :as mmenv]))

(def ^:private reset-cljs-envs! #(requiring-resolve 'typed.cljs.checker.base-env/reset-envs!))

(defn reset-envs!
  "Reset all environments for the current implementation."
  ([opts] (reset-envs! false opts))
  ([cljs? opts]
  (let []
    (impl/impl-case opts
      :clojure
      (do (bse-clj/reset-clojure-envs!)
          (mmenv/reset-mm-dispatch-env!)
          (ns-opts/reset-ns-opts! (impl/clj-checker)))
      :cljs
      (do
        (assert cljs? "No ClojureScript dependency")
        (when cljs?
          (reset-cljs-envs!)
          (ns-opts/reset-ns-opts! (impl/cljs-checker)))))
    nil)))

(defn load-core-envs!
  "Add core annotations to environments for the current implementation."
  ([opts] (load-core-envs! false opts))
  ([cljs? opts]
  (let []
    (impl/impl-case opts
      :clojure
      (do (bse-clj/refresh-core-clojure-envs!)
          ;(mmenv/reset-mm-dispatch-env!)
          ;(ns-opts/reset-ns-opts! (impl/clj-checker))
          )
      :cljs
      nil
      #_
      (do
        (assert nil "load-core-envs! TODO CLJS")
        (assert cljs? "No ClojureScript dependency")
        (reset-envs! true)
        (when cljs?
          (reset-cljs-envs!)
          (ns-opts/reset-ns-opts! (impl/cljs-checker)))))
    nil)))

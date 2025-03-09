;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.init
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.indirect-ops :as ind]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljc.checker.reset-env :refer [load-core-envs!
                                                  reset-envs!]]))

(defn load-impl
  ([] (load-impl false))
  ([cljs?]
   (do
     ;(println "Building core.typed base environments ...")
     ;(flush)
     (impl/register-clj!)
     (ind/collect-indirect-annotations)
     (load-core-envs! ((requiring-resolve 'typed.clj.runtime.env/clj-opts)))
     #_
     (when cljs?
       (impl/register-cljs!)
       ;; FIXME should be load-core-envs!
       (reset-envs! cljs?)
       ;; note: don't do below, need to move the reset-envs! call instead
       ;(reset! cljs-loaded? true)
       )
     ;(println "Finished building base environments")
     ;(flush)
     nil)))

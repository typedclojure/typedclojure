;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.init
  (:require [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.reset-env :refer [load-core-envs!
                                                  reset-envs!]]))

(defn load-impl 
  ([] (load-impl false))
  ([cljs?]
   (do
     ;(println "Building core.typed base environments ...")
     ;(flush)
     ;(impl/with-clojure-impl
     ;  (reset-envs!))
     (impl/register!)
     (impl/with-clojure-impl
       (load-core-envs!))
     (when cljs?
       (impl/with-cljs-impl
         ;; FIXME should be load-core-envs!
         (reset-envs! cljs?))
       ;; note: don't do below, need to move the reset-envs! call instead
       ;(reset! cljs-loaded? true)
       )
     ;(println "Finished building base environments")
     ;(flush)
     nil)))

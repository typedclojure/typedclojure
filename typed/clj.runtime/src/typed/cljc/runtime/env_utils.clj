;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.runtime.env-utils
  (:require [clojure.core.typed.util-vars :as uvs])
  (:import [java.lang.ref SoftReference]))

;; [[:-> Type] :-> [:-> Type]]
(defn delay-type* [f]
  ;;FIXME pull out impl-case into its own namespace
  (case ((requiring-resolve 'clojure.core.typed.current-impl/current-impl))
    :clojure.core.typed.current-impl/clojure
    (let [def-ns-vol (volatile! (SoftReference. *ns*))
          d (volatile! (delay (f)))]
      (fn []
        (when-some [^SoftReference sr @def-ns-vol]
          (when-some [def-ns (.get sr)]
            (if (identical? def-ns (find-ns (ns-name def-ns)))
              (let [t (force @d)]
                ;(prn "returning" ((juxt identity hash) def-ns) ((juxt identity hash) (find-ns (ns-name def-ns))))
                t)
              ;;forget types that were defined in stale namespaces
              (do ;(prn "FORGETTING ANNOTATION" (class @@d))
                  (vreset! def-ns-vol nil)
                  (vreset! d nil)
                  nil))))))
    ;; TODO cljs strategy for forgetting types from reloaded namespaces
    :clojure.core.typed.current-impl/clojurescript
    (delay (f))))

(defmacro delay-type [& args]
  `(delay-type* (fn [] (do ~@args))))

(defn force-type [v]
  (let [res (force (if (fn? v) (v) v))]
    (assert (not (or (delay? res) (fn? res)))
            (class res))
    res))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.runtime.env-utils
  ;; annotations live in typed.cljc.runtime.env-utils-annotations to avoid cyclic
  ;; load dependencies when using clojure.core.typed without typed.clojure
  ;;DON'T require typed.clojure here
  (:require [typed.clojure :as-alias t])
  (:import [java.lang.ref SoftReference]))

(defonce ^{:doc "Internal use only"} ^:no-doc parsed-types-invalidation-id (atom (str (random-uuid))))

(defn invalidate-parsed-types! []
  (reset! parsed-types-invalidation-id (str (random-uuid))))

;; [[:-> Type] :-> [:-> Type]]
;; Note: used directly by clojure.core.typed and current-impl to avoid cycles
(defn delay-type* [f]
  (let [f (bound-fn* f)
        this-invalidation-id (volatile! @parsed-types-invalidation-id)
        def-ns-vol (volatile! (SoftReference. *ns*))
        ->f-delay (fn [] (delay (f)))
        d (atom (->f-delay))]
    (fn []
      (when-some [^SoftReference sr @def-ns-vol]
        (when-some [def-ns (.get sr)]
          (assert (instance? clojure.lang.Namespace def-ns))
          (if (identical? def-ns (find-ns (ns-name def-ns)))
            (let [_ (when (not= @this-invalidation-id @parsed-types-invalidation-id)
                      ;; attempt to reparse type if internal namespaces have changed
                      (swap! d #(when % (->f-delay)))
                      (vreset! this-invalidation-id @parsed-types-invalidation-id))]
              (force @d))
            ;;forget types that were defined in stale namespaces
            (do ;(prn "FORGETTING ANNOTATION" (class @@d))
                (vreset! def-ns-vol nil)
                (reset! d nil)
                nil)))))))

(defmacro delay-type [& args]
  `(delay-type* (fn [] (do ~@args))))

(defn force-type [v]
  (let [res (force (if (fn? v) (v) v))]
    (assert (not (or (delay? res) (fn? res)))
            (class res))
    res))

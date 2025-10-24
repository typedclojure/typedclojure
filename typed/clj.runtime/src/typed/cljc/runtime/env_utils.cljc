;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.runtime.env-utils
  ;; annotations live in typed.cljc.runtime.env-utils-annotations to avoid cyclic
  ;; load dependencies when using clojure.core.typed without typed.clojure
  ;;DON'T require typed.clojure here
  (:refer-clojure :exclude [random-uuid #?(:clj delay)])
  (:require [typed.clojure :as-alias t]
            #?(:clj [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]))
  (:import #?(:clj [java.lang.ref SoftReference])))

(defn random-uuid []
  #?(:clj (java.util.UUID/randomUUID) ;;clojure 1.10 support
     :cljr (System.Guid/NewGuid)
     :default (throw (ex-info "TODO random-uuid" {}))))

(defonce ^{:doc "Internal use only"} ^:no-doc parsed-types-invalidation-id (atom (str (random-uuid))))

(defn invalidate-parsed-types! []
  (reset! parsed-types-invalidation-id (str (random-uuid))))

;; [[:-> Type] :-> [:-> Type]]
;; Note: used directly by clojure.core.typed and current-impl to avoid cycles
(defn delay-type**
  "Manual bound-fn"
  [f]
  (let [this-invalidation-id (volatile! @parsed-types-invalidation-id)
        def-ns-vol (volatile! (#?(:cljr identity :default SoftReference.) *ns*))
        ->f-delay (fn [] (delay (f)))
        ;;global cache
        d (atom (->f-delay))
        id (gensym)]
    (fn [{::keys [type-cache] :as _opts}]
      (when-some [^#?(:cljr Object :default SoftReference) sr @def-ns-vol]
        (when-some [def-ns #?(:cljr sr :default (.get sr))]
          (assert (instance? clojure.lang.Namespace def-ns))
          (if (identical? def-ns (find-ns (ns-name def-ns)))
            (if type-cache
              (force @(or (get @type-cache id)
                          (get (swap! type-cache update id #(or % (->f-delay))) id)))
              (let [_ (when (not= @this-invalidation-id @parsed-types-invalidation-id)
                        ;; attempt to reparse type if internal namespaces have changed
                        (swap! d #(when % (->f-delay)))
                        (vreset! this-invalidation-id @parsed-types-invalidation-id))]
                (force @d)))
            ;;forget types that were defined in stale namespaces
            (do ;(prn "FORGETTING ANNOTATION" (class @@d))
                (vreset! def-ns-vol nil)
                (reset! d nil)
                (some-> type-cache (swap! dissoc id))
                nil)))))))

(defn delay-type*
  "Automatic bound-fn"
  [f]
  (delay-type** (fn [] (f))))

(defmacro delay-type'
  "Manual bound-fn"
  [& args]
  `(delay-type** (fn [] (do ~@args))))

(defmacro delay-type
  "Automatic bound-fn"
  [& args]
  `(delay-type* (fn [] (do ~@args))))

(defn force-type [v opts]
  (let [res (force (if (fn? v) (v opts) v))]
    (assert (not (or (delay? res) (fn? res)))
            (class res))
    res))

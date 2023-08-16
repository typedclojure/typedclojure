;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.runtime.env-utils
  (:require [typed.clojure :as t])
  (:import [java.lang.ref SoftReference]))

(t/tc-ignore
  (def ^:private annotations (atom []))
  (defmacro ^:private ann [& args] `(swap! annotations conj #(t/ann ~@args)))
  (defmacro ^:private defalias [& args] `(swap! annotations conj #(t/defalias ~@args))))

(defalias InvalidationId s/Str)
(defalias ForcedType
  (t/TFn [[x :variance :covariant]]
         (t/Difference x t/Fn (t/Delay t/Any))))
(defalias DelayedType
  (t/TFn [[x :variance :covariant]]
         (t/U (ForcedType x)
              (t/Delay (ForcedType x)))))
(defalias ReparsableDelayedType
  (t/TFn [[x :variance :covariant]]
         (t/I t/Fn [:-> (DelayedType x)])))

(ann parsed-types-invalidation-id (t/Atom InvalidationId))
(defonce ^{:doc "Internal use only"} ^:no-doc parsed-types-invalidation-id (atom (str (random-uuid))))

(ann invalidate-parsed-types! [:-> InvalidationId])
(defn invalidate-parsed-types! []
  (reset! parsed-types-invalidation-id (str (random-uuid))))

;; [[:-> Type] :-> [:-> Type]]
;; Note: used directly by clojure.core.typed and current-impl to avoid cycles
(ann delay-type* (t/All [x] [(ReparsableDelayedType x) :-> [:-> (t/Nilable x)]]))
(defn delay-type* [f]
  (let [f (bound-fn* f)
        this-invalidation-id (atom @parsed-types-invalidation-id)]
    (let [def-ns-vol (atom (SoftReference. *ns*))
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
                        (reset! this-invalidation-id @parsed-types-invalidation-id))]
                (force @d))
              ;;forget types that were defined in stale namespaces
              (do ;(prn "FORGETTING ANNOTATION" (class @@d))
                  (reset! def-ns-vol nil)
                  (reset! d nil)
                  nil))))))))

(defmacro delay-type [& args]
  `(delay-type* (fn [] (do ~@args))))

(ann force-type (t/All [x] [(t/U (DelayedType x)
                                 [:-> (DelayedType x)])
                            :-> x]))
(defn force-type [v]
  (let [res (force (if (fn? v) (v) v))]
    (assert (not (or (delay? res) (fn? res)))
            (class res))
    res))

;; t/ann and t/defalias expand to calls to this ns. register types after interning all vars.
(t/tc-ignore
  (run! #(%) @annotations))

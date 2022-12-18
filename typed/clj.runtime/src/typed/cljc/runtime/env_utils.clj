;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; reloading internal namespaces often makes the global type environment
;; contain old versions of classes/functions and so forces a repl restart.
;; this namespace tries to contain the problem to keep development slick
;; until a better approach is found.
(ns ^:no-doc typed.cljc.runtime.env-utils
  (:require [clojure.core.typed.util-vars :as uvs]
            [clojure.core.typed.parsed-type-invalidation :refer [parsed-types-invalidation-id]]))

(defn invalidate-parsed-types! []
  (locking parsed-types-invalidation-id
    (vreset! parsed-types-invalidation-id (str (random-uuid)))))

;; [[:-> Type] :-> [:-> Type]]
(defn delay-type* [f]
  (if uvs/dev-mode?
    (let [v (volatile! nil)
          try-read #(when-some [[t invalidation-id] @v]
                      (when (= invalidation-id @parsed-types-invalidation-id)
                        t))]
      (fn []
        (or (try-read)
            (locking v
              (or (try-read)
                  ;; look up id _after_ (f) in case of side effects
                  (let [t (f)]
                    (vreset! v [t @parsed-types-invalidation-id])
                    t))))))
    (delay (f))))

(defmacro delay-type [& args]
  `(delay-type* (fn [] (do ~@args))))

(defn force-type [v]
  (let [res (force (if (fn? v) (v) v))]
    (assert (not (or (delay? res) (fn? res)))
            (class res))
    res))

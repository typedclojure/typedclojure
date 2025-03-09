;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.runtime.env-utils-annotations
  (:require [typed.clojure :as t]
            [typed.cljc.runtime.env-utils :as env-utils]))

(t/defalias env-utils/InvalidationId t/Str)
(t/defalias env-utils/ForcedType
  (t/TFn [[x :variance :covariant]]
         (t/Difference x t/Fn (t/Delay t/Any))))
(t/defalias env-utils/DelayedType
  (t/TFn [[x :variance :covariant]]
         (t/U (env-utils/ForcedType x)
              (t/Delay (env-utils/ForcedType x)))))
(t/defalias env-utils/ReparsableDelayedType
  (t/TFn [[x :variance :covariant]]
         (t/I t/Fn [:-> (env-utils/DelayedType x)])))

(t/ann env-utils/parsed-types-invalidation-id (t/Atom env-utils/InvalidationId))
(t/ann env-utils/invalidate-parsed-types! [:-> env-utils/InvalidationId])
(t/ann env-utils/delay-type* (t/All [x] [(env-utils/ReparsableDelayedType x) :-> [:-> (t/Nilable x)]]))
(t/ann env-utils/force-type (t/All [x] [(t/U (env-utils/DelayedType x)
                                             [:-> (env-utils/DelayedType x)])
                                        t/Any
                                        :-> x]))

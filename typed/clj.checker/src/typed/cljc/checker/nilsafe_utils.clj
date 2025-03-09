;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.nilsafe-utils
  (:require [clojure.set :as set]
            [typed.clojure :as t]))

(t/ann ^:no-check set-union 
       (t/All [x]
              (t/IFn [:-> (t/Set x)]
                     [(t/Nilable (t/Set x)) :-> (t/Set x)]
                     [(t/Nilable (t/Set x)) (t/Set x) :* :-> (t/Set x)])))
(def set-union (fnil set/union #{}))

(t/ann ^:no-check set-difference 
       (t/All [x]
              (t/IFn [(t/U nil (t/Set x)) -> (t/Set x)]
                     [(t/U nil (t/Set x)) (t/Set t/Any) :* -> (t/Set x)])))
(def set-difference (fnil set/difference #{}))

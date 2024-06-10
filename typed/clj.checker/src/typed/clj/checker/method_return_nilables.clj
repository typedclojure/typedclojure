;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.method-return-nilables
  (:require [typed.cljc.runtime.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.runtime.env-utils :as env-utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Return non-nilables

(def add-nonnilable-method-return impl/add-nonnilable-method-return)

(defn reset-nonnilable-method-return-env! [checker m]
  (env/swap-checker! checker assoc impl/method-return-nonnilable-env-kw m)
  nil)

(defn merge-nonnilable-method-return-env! [checker m]
  {:pre [(map? m)]}
  (env/swap-checker! checker update impl/method-return-nonnilable-env-kw merge m)
  nil)

(defn nonnilable-method-return-env [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/method-return-nonnilable-env-kw {}))

(defn nonnilable-return? [sym arity opts]
  (let [as (env-utils/force-type (get (nonnilable-method-return-env (env/checker opts)) sym))]
    (assert ((some-fn nil? set? #{:all}) as))
    (boolean (or (= :all as)
                 (contains? as arity)))))

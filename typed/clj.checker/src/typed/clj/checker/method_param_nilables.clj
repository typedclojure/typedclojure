;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.method-param-nilables
  (:require [typed.cljc.runtime.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.runtime.env-utils :as env-utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defn reset-method-nilable-param-env! [checker m]
  (env/swap-checker! checker assoc impl/method-param-nilable-env-kw m)
  nil)

(defn merge-method-nilable-param-env! [checker m]
  {:pre [(map? m)]}
  (env/swap-checker! checker update impl/method-param-nilable-env-kw merge m)
  nil)

(def add-method-nilable-param impl/add-method-nilable-param)

(defn nilable-param-env [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/method-param-nilable-env-kw {}))

(defn nilable-param? [sym arity param opts]
  (boolean 
    (when-some [nilables (env-utils/force-type (get (nilable-param-env (env/checker opts)) sym) opts)]
      (assert (set? nilables))
      (when-some [params (or (nilables :all)
                             (nilables arity))]
        (or (#{:all} params)
            (params param))))))

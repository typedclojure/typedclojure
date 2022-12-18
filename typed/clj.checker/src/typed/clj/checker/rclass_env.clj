;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; don't require the checker from here
(ns ^:no-doc typed.clj.checker.rclass-env
  (:require [typed.cljc.runtime.env :as env]
            [typed.cljc.runtime.env-utils :refer [force-env]]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

(def rclass-env-kw ::rclass-env)

(defn- rclasses []
  (get (env/deref-checker) impl/current-rclass-env-kw {}))

(defn get-rclass
  "Returns the RClass with class symbol csym.
  Returns nil if not found."
  [csym]
  {:post [(do (assert ((some-fn nil? r/RClass? r/TypeFn?) %)
                      [:looking-up csym
                       :gave-class (class %)
                       :provided-class-hash
                       (hash (class %))
                       :current-class-hash
                       (hash (Class/forName (.getName (class %))))
                       :equal?
                       (= (class %)
                          (Class/forName (.getName (class %))))
                       :latest-var? (= #'get-rclass (resolve `get-rclass))
                       :raw (get (rclasses) csym)])
              true)]}
  (force-env (get (rclasses) csym)))

(defn alter-class* [csym type]
  (env/swap-checker! assoc-in [impl/current-rclass-env-kw csym] type)
  nil)

(defn reset-rclass-env! [m]
  (env/swap-checker! assoc impl/current-rclass-env-kw m)
  nil)

;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from tools.analyzer
(ns ^:typed.clojure typed.cljc.analyzer.env
  (:refer-clojure :exclude [ensure]))

;;  "Global env atom
;;   Required options:
;;    * :namespaces an atom containing a map from namespace symbol to namespace map,
;;      the namespace map contains at least the following keys:
;;     ** :mappings a map of mappings of the namespace, symbol to var/class
;;     ** :aliases a map of the aliases of the namespace, symbol to symbol
;;     ** :ns a symbol representing the namespace"
(defn with-env
  "Binds the global env to env"
  [opts env]
  (let [env (cond
              (map? env) (atom env)
              (and (instance? clojure.lang.Atom env)
                   (map? @env)) env
              :default (throw (ex-info (str "global env must be a map or atom containing a map, not "
                                            (class env))
                                       {:env env})))]
    (assoc opts ::env env)))

;; if *env* is not bound, bind it to env
(defn ensure
  "If *env* is not bound it binds it to env"
  [opts env]
  (if (::env opts)
    opts
    (with-env opts env)))

(defn deref-env
  "Returns the value of the current global env if bound, otherwise
   throws an exception."
  [{::keys [env] :as opts}]
  (if env
    @env
    (throw (Exception. "global env not bound"))))

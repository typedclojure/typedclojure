;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.clj.checker.mm-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env :as env]))

;; Environment for storing multimethod types and inferred filters

(defn multimethod-dispatch-env [checker]
  (get (env/deref-checker checker) impl/multimethod-dispatch-env-kw {}))

(defn reset-mm-dispatch-env! [checker]
  (env/swap-checker! checker assoc impl/multimethod-dispatch-env-kw {})
  nil)

; [Symbol Type -> nil]
(defn add-multimethod-dispatch-type
  "Add the type of the dispatch function of the multimethod named by mmsym
  to the environment. If already exists, must be identical."
  [mmsym dtype opts]
  {:pre [(symbol? mmsym)
         (r/Type? dtype)]}
  (impl/assert-clojure opts)
  (let [checker (env/checker opts)
        old (get (multimethod-dispatch-env checker) mmsym)]
    ;(prn `add-multimethod-dispatch-type mmsym dtype)
    (when (and old (not= old dtype))
      (err/int-error 
        (str "Inconsistent dispatch type inferred for multimethod: " mmsym
             ".  JVM process restart probably necessary.")
        opts))
    (env/swap-checker! checker assoc-in [impl/multimethod-dispatch-env-kw mmsym] dtype))
  nil)

(defn multimethod-dispatch-type
  "Can return nil"
  [mmsym opts]
  {:pre [(symbol? mmsym)]
   :post [((some-fn nil? r/Type?) %)]}
  (impl/assert-clojure opts)
  (let [checker (env/checker opts)]
    (get (multimethod-dispatch-env checker) mmsym)))

#_
(defn get-multimethod-dispatch-type [mmsym opts]
  {:pre [(symbol? mmsym)]
   :post [(r/Type? %)]}
  (impl/assert-clojure opts)
  (let [checker (env/checker opts)
        t (get (multimethod-dispatch-env checker) mmsym)]
    (when-not t 
      (err/int-error (str "Multimethod requires dispatch type: " mmsym) opts))
    t))

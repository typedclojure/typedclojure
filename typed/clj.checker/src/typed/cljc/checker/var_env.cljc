;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.var-env
  (:refer-clojure :exclude [#?(:clj requiring-resolve) #?(:clj delay)])
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.set :as set]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])
            #?(:clj [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.name-env :as name-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env :as env]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [clojure.core.typed.runtime.configs :as configs]))

(defn clj-var-annotations []
  (get @(impl/clj-checker) impl/current-var-annotations-kw {}))

(defn clj-nocheck-var? []
  (get @(impl/clj-checker) impl/current-nocheck-var?-kw {}))

(defn clj-used-vars []
  (get @(impl/clj-checker) impl/current-used-vars-kw {}))

(def var-annotations-con (con/hash-c? (every-pred symbol? namespace) (some-fn delay? r/Type?)))
(def nocheck-var-con (con/set-c? (every-pred symbol? namespace)))
(def used-vars-con (con/set-c? (every-pred symbol? namespace)))
(def checked-var-defs-con (con/set-c? (every-pred symbol? namespace)))
(def cljs-jsvar-annotations-con (con/hash-c? symbol? r/Type?))

(defn with-lexical-env [opts env]
  (assoc opts ::vs/lexical-env env))

(defn var-annotations [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/current-var-annotations-kw {}))

(def var-no-checks impl/var-no-checks)

(defn used-vars [checker]
  {:post [(set? %)]}
  (get (env/deref-checker checker) impl/current-used-vars-kw #{}))

(defn checked-vars [checker]
  {:post [(set? %)]}
  (get (env/deref-checker checker) impl/current-checked-var-defs-kw #{}))

(defn untyped-var-annotations
  [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/untyped-var-annotations-kw {}))

(defn add-var-type [checker sym type opts]
  (when-let [old-t ((var-annotations checker) sym)]
    ;; if old type is realized, it's probably been
    ;; used. We should force the new type to ensure
    ;; it's the same.
    (when (and (delay? old-t)
               (realized? old-t))
      (when (not= (force-type old-t opts)
                  (force-type type opts))
        (println (str "WARNING: Duplicate var annotation: " sym))
        (flush))))
  (env/swap-checker! checker assoc-in [impl/current-var-annotations-kw sym] type)
  nil)

(def add-untyped-var impl/add-untyped-var)

(def check-var? impl/check-var?)

(defn checked-var-def? [checker sym]
  (contains? (checked-vars checker) sym))

(defn used-var? [checker sym]
  (contains? (used-vars checker) sym))

(def add-nocheck-var impl/add-nocheck-var)
(def remove-nocheck-var impl/remove-nocheck-var)

(defn add-used-var [checker sym]
  (env/swap-checker! checker update impl/current-used-vars-kw (fnil conj #{}) sym)
  nil)

(defn add-checked-var-def [checker sym]
  (env/swap-checker! checker update impl/current-checked-var-defs-kw (fnil conj #{}) sym)
  nil)

(defn vars-with-unchecked-defs [checker]
  (set/difference (used-vars checker)
                  (checked-vars checker)
                  (var-no-checks checker)))

(defn reset-current-var-annotations! [checker m]
  (env/swap-checker! checker assoc impl/current-var-annotations-kw m)
  nil)

(defn merge-current-var-annotations! [checker m]
  {:pre [(map? m)]}
  (env/swap-checker! checker update impl/current-var-annotations-kw merge m)
  nil)

(defn reset-current-nocheck-var?! [checker nocheck]
  (env/swap-checker! checker assoc impl/current-nocheck-var?-kw nocheck)
  nil)

(defn merge-current-nocheck-var?! [checker nocheck]
  (env/swap-checker! checker update impl/current-nocheck-var?-kw (fnil into #{}) nocheck)
  nil)

(defn reset-current-used-vars! [checker s]
  (env/swap-checker! checker assoc impl/current-used-vars-kw s)
  nil)

(defn reset-current-checked-var-defs! [checker s]
  (env/swap-checker! checker assoc impl/current-checked-var-defs-kw s)
  nil)

(defn reset-var-type-env! [checker m nocheck]
  (reset-current-var-annotations! checker m)
  (reset-current-nocheck-var?! checker nocheck)
  (reset-current-used-vars! checker #{})
  (reset-current-checked-var-defs! checker #{})
  nil)

(defn refresh-var-type-env! [checker m nocheck]
  (merge-current-var-annotations! checker m)
  (merge-current-nocheck-var?! checker nocheck)
  (reset-current-used-vars! checker #{})
  (reset-current-checked-var-defs! checker #{})
  nil)

(defn reset-jsvar-type-env! [checker m]
  {:pre [(cljs-jsvar-annotations-con m)]
   :post [(nil? %)]}
  (env/swap-checker! checker assoc impl/cljs-jsvar-annotations-kw m)
  nil)

(defn jsvar-annotations [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/cljs-jsvar-annotations-kw {}))

(def clj-var-providers (delay (configs/clj-config-var-providers)))
#?(:clj (def cljs-var-providers (delay (configs/cljs-config-var-providers))))

(defn lookup-Var-nofail [nsym opts]
  {:pre [(symbol? nsym)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [checker (env/checker opts)]
    (or (let [e (var-annotations checker)]
          (force-type (e nsym) opts))
        (when (impl/checking-clojurescript? opts)
          (force-type ((jsvar-annotations checker) nsym) opts))
        (when-some [ts (not-empty
                         (into (sorted-map) (map (fn [fsym]
                                                   (let [f (requiring-resolve fsym)]
                                                     (assert f fsym)
                                                     (some->> (f nsym opts)
                                                              (vector fsym)))))
                               (impl/impl-case opts
                                 :clojure @clj-var-providers
                                 #?@(:clj [:cljs @cljs-var-providers]))))]
          (let [chosen-entry (first ts)
                _ (when (< 1 (count ts))
                    (println (format "WARNING: multiple type providers for var %s (%s), using %s"
                                     nsym
                                     (vec (keys ts))
                                     (key chosen-entry))))]
            (val chosen-entry))))))

(defn lookup-Var [nsym opts]
  {:post [((some-fn nil? r/Type?) %)]}
  (or (lookup-Var-nofail nsym opts)
      (err/int-error
        (str "Untyped var reference: " nsym)
        opts)))

(defn type-of-nofail [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (or (when (and (not (namespace sym))
                 (not-any? #{\.} (str sym)))
        (lex/lookup-local sym opts))
      (lookup-Var-nofail sym opts)))

(defn type-of [sym opts]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (or (type-of-nofail sym opts)
      (err/int-error (str (when-some [env (::vs/current-env opts)]
                            (str (:line env) ": "))
                          "Missing type for binding: " (pr-str sym))
                     opts)))

(defn get-untyped-var [checker nsym sym opts]
  {:pre [(symbol? nsym)
         (symbol? sym)]
   :post [(or (nil? %)
              (r/Type? %))]}
  (some-> (untyped-var-annotations checker)
          (get nsym)
          (get sym)
          (force-type opts)))

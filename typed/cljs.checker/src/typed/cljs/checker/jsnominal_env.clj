;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljs.checker.jsnominal-env
  (:refer-clojure :exclude [get-method])
  (:require [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env :as env])
  (:import (typed.cljc.checker.type_rep Scope)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSNominal

(t/defalias JSNominalEntry
  "A map entry for a JSNominal type."
  '{:jsnominal r/Type
    :fields (t/Map t/Sym (t/U Scope r/Type))
    :methods (t/Map t/Sym (t/U Scope r/Type))
    :ctors (t/U Scope r/Type nil)
    :ancestors (t/Set (t/U Scope r/Type))})

(t/defalias JSNominalEnv
  "A map of symbols of JSNomainalEntry's"
  (t/Map t/Sym JSNominalEntry))

(defn jsnominal-env [opts]
  {:post [(map? %)]}
  (impl/jsnominal-env (env/checker opts)))

(def jsnominal-env?
  (con/hash-c? symbol?
               (con/hmap-c? :jsnominal r/Type?
                            :fields (con/hash-c? symbol? (some-fn r/Scope? r/Type?))
                            :methods (con/hash-c? symbol? (some-fn r/Scope? r/Type?))
                            :ctor (some-fn nil? r/Scope? r/Type?)
                            :ancestors (con/set-c? (some-fn r/Scope? r/Type?)))))

(t/ann init-jsnominal-entry [r/Type -> JSNominalEntry])
(defn init-jsnominal-entry [nom]
  {:post [(jsnominal-env? %)]}
  {:jsnominal nom
   :fields {}
   :methods {}
   :ctors nil
   :ancestors #{}})

(t/ann ^:no-check get-jsnominal [t/Any t/Any -> (t/Nilable r/Type)])
(defn get-jsnominal
  "Returns the nomainal JS type with class symbol csym.
  Returns nil if not found."
  [csym opts]
  {:post [((some-fn nil? r/Type?) %)]}
  (-> (get (impl/jsnominal-env (env/checker opts)) csym) :jsnominal))

#_#_
(t/ann contains-jsnominal? [t/Any t/Any -> boolean])
(defn contains-jsnominal?
  [csym opts]
  (boolean (get-jsnominal csym opts)))

;(t/ann add-jsnominal [t/Sym r/Type -> nil])
;(defn add-jsnominal [csym type]
;  (assert (r/Type? type)
;          (str "JS nominal" csym " not a type: " type))
;  ; remove old fields etc.
;  (swap! JSNOMINAL-ENV assoc csym (init-jsnominal-entry type))
;  nil)
;
;(t/ann add-method [t/Sym JSNominal (t/U Scope r/Type) -> nil])
;(defn add-method 
;  "Add a new method to the JS nominal type csym. Assumes
;  the method type is properly Scope'd"
;  [csym method-sym type]
;  (swap! JSNOMINAL-ENV assoc-in [csym :methods method-sym] type)
;  nil)
;
;(t/ann add-field [t/Sym JSNominal (t/U Scope r/Type) -> nil])
;(defn add-field 
;  "Add a new field to the JS nominal type csym. Assumes
;  the field type is properly Scope'd"
;  [csym field-sym type]
;  (swap! JSNOMINAL-ENV update-in [csym :fields field-sym] (constantly type)))

(t/ann ^:no-check get-inherited-property [[t/Sym (t/Option (t/Seqable r/Type)) t/Sym -> (t/Option r/Type)] 
                                          t/Sym (t/Option (t/Seqable r/Type)) t/Sym -> (t/Option r/Type)])
(defn get-inherited-property
  "search for the property in the interfaces ancestors
   method: (get-inherited-property get-method csym args method-sym)
   field:  (get-inherited-property get-field csym args field-sym)"
  [f csym args method-sym opts]
  (->> (get-in (impl/jsnominal-env (env/checker opts)) [csym :ancestors])
       (map #(f (:id %) args method-sym opts))
       (filter identity)
       first))

(t/ann ^:no-check get-method [t/Sym (t/U nil (t/Seqable r/Type)) t/Sym t/Any -> (t/U nil r/Type)])
(defn get-method
  "Returns the instantiated method type named method-sym on nominal csym."
  [csym args method-sym opts]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? method-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  ;(println (str "Searching " csym "#" method-sym))
  (if-some [tscope (get-in (impl/jsnominal-env (env/checker opts)) [csym :methods method-sym])]
    (c/inst-and-subst tscope args opts)
    (get-inherited-property get-method csym args method-sym opts)))

(t/ann ^:no-check get-field [t/Sym (t/U nil (t/Seqable r/Type)) t/Sym -> (t/U nil r/Type)])
(defn get-field
  "Returns the instantiated field type named field-sym on nominal csym."
  [csym args field-sym opts]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? field-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (if-some [tscope (get-in (impl/jsnominal-env (env/checker opts)) [csym :fields field-sym])]
    (c/inst-and-subst tscope args opts)
    (get-inherited-property get-field csym args field-sym opts)))

(t/ann ^:no-check get-ctor [t/Sym (t/U nil (t/Seqable r/Type)) t/Any -> (t/U nil r/Type)])
(defn get-ctor
  "Returns the instantiated constructor type on nominal csym."
  [csym args opts]
  {:pre [(symbol? csym)
         (every? r/Type? args)]
   :post [((some-fn nil? r/Type?) %)]}
  (some-> (get-in (impl/jsnominal-env (env/checker opts)) [csym :ctor])
          (c/inst-and-subst args opts)))

(t/ann ^:no-check reset-jsnominal! [JSNominalEnv -> nil])
(defn reset-jsnominal! [m]
  {:pre [(jsnominal-env? m)]
   :post [(nil? %)]}
  (impl/reset-jsnominal-env! m)
  nil)

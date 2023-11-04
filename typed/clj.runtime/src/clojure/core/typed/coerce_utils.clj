;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.coerce-utils
  (:require [typed.clojure :as t]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.typed.current-impl :as impl])
  (:import (clojure.lang RT Var)))

(t/ann symbol->Class [t/Sym -> Class])
(defn symbol->Class 
  "Returns the Class represented by the symbol. Works for
  primitives (eg. byte, int). Does not further resolve the symbol."
  [sym]
  {:pre [(symbol? sym)]
   :post [(class? %)]}
  (case sym
    byte Byte/TYPE
    short Short/TYPE
    int Integer/TYPE
    long Long/TYPE
    float Float/TYPE
    double Double/TYPE
    boolean Boolean/TYPE
    char Character/TYPE
    (RT/classForName (str sym))))

(t/ann Class->symbol [Class -> t/Sym])
(defn Class->symbol [^Class cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

(t/ann var->symbol [t/AnyVar -> t/Sym])
(defn var->symbol [^Var var]
  {:pre [(var? var)]
   :post [(symbol? %)
          (namespace %)]}
  (symbol var))

(t/ann kw->symbol [t/Kw -> t/Sym])
(defn kw->symbol [kw]
  {:pre [(keyword? kw)]
   :post [(symbol? %)]}
  (symbol kw))

(t/ann ns->file (t/IFn [t/Sym -> t/Str]
                       [t/Sym t/Bool -> t/Str]))
(defn ns->file 
  ([nsym] (ns->file nsym true))
  ([nsym suffix?]
   {:pre [(symbol? nsym)]
    :post [(string? %)]}
   ;copied basic approach from tools.emitter.jvm
   (let [res (munge nsym)
         f (str/replace (str res) #"\." "/")
         ex (when suffix?
              (impl/impl-case
                :clojure ".clj"
                :cljs ".cljs"))
         p (str f ex)
         p (if (or (io/resource p)
                   (not suffix?))
             p
             (str f ".cljc"))
         p (if (.startsWith p "/") (subs p 1) p)]
     p)))

(t/ann ns->URL [t/Sym -> (t/Nilable java.net.URL)])
(defn ns->URL ^java.net.URL [nsym]
  {:pre [(symbol? nsym)]
   :post [((some-fn #(instance? java.net.URL %)
                    nil?) 
           %)]}
  (let [p (ns->file nsym)]
    (io/resource p)))

(t/ann sym->kw [t/Sym -> t/Kw])
(defn sym->kw [sym]
  {:pre [(symbol? sym)]
   :post [(keyword? %)]}
  (keyword sym))

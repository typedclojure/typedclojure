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
            #?(:cljr [clojure.clr.io] :default [clojure.java.io :as io])
            [clojure.core.typed.current-impl :as impl])
  (:import (clojure.lang RT Var)))

(t/ann symbol->Class [t/Sym -> Class])

#?(:cljr

(defn symbol->Class 
  "Returns the Class represented by the symbol. Works for
  primitives (eg. byte, int). Does not further resolve the symbol."
  [sym]
  {:pre [(symbol? sym)]
   :post [(class? %)]}
  (case sym
    byte Byte
	sbyte SByte
    short Int16
	ushort UInt16
    int Int32
	uint UInt32
    long Int64
	ulong UInt64
    float Single
    double Double
    boolean Boolean
    char Char
	decimal Decimal
    (RT/classForName (str sym))))
	
:default
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
)

(t/ann Class->symbol [Class -> t/Sym])
(defn Class->symbol [^#?(:cljr Type :default Class) cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (#?(:cljr .FullName :default .getName) cls)))

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
                :cljs ".cljs"
				:cljr ".cljr"))
         p (str f ex)
         p (if (or #?(:cljr false :default (io/resource p))    ;; no equivalent of io/resource for CLR
                   (not suffix?))
             p
             (str f ".cljc"))
         p (if (#?(:cljr .StartsWith :default .startsWith) p "/") (subs p 1) p)]
     p)))

#?(:cljr  :ignore :default
(do 
  (t/ann ns->URL [t/Sym -> (t/Nilable java.net.URL)])

  (defn ns->URL ^java.net.URL [nsym]
    {:pre [(symbol? nsym)]
     :post [((some-fn #(instance? java.net.URL %)
                      nil?) 
             %)]}
    (let [p (ns->file nsym)]
      (io/resource p)))
))  ;; no equivalent of io/resource for CLR 


(t/ann sym->kw [t/Sym -> t/Kw])
(defn sym->kw [sym]
  {:pre [(symbol? sym)]
   :post [(keyword? %)]}
  (keyword sym))

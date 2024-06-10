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

(t/ann symbol->Class [t/Sym -> #?(:cljr Type :default Class)])
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
    #?@(:cljr [
    sbyte SByte
    ushort UInt16
    uint UInt32
    ulong UInt64
    decimal Decimal
    ])
    (RT/classForName (str sym))))

(t/ann Class->symbol [#?(:cljr Type :default Class) -> t/Sym])
(defn Class->symbol [^#?(:cljr Type :default Class) cls]
  #_{:pre [(class? cls)]
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

(t/ann ns->file [t/Sym t/Any -> t/Str])
(defn ns->file [nsym opts]
  {:pre [(symbol? nsym)]
   :post [(string? %)]}
  ;copied basic approach from tools.emitter.jvm
  (let [res (munge nsym)
        f (str/replace (str res) #"\." "/")
        p (or (some (fn [ex]
                      (let [p (str f ex)]
                        ;;FIXME how to check if file exists on "classpath" in CLR?
                        (when #?(:cljr (throw (ex-info "how to check if file exists on classpath in CLR?"
                                                       {}))
                                 :default (io/resource p))
                          p)))
                    (impl/impl-case opts
                      :clojure [#?(:cljr ".cljr") ".clj"]
                      :cljs [".cljs"]))
              (str f ".cljc"))]
    (cond-> p
      (str/starts-with? p "/") (subs 1))))

;; no equivalent of io/resource for CLR 
#?(:cljr nil :default (do
(t/ann ns->URL [t/Sym t/Any -> (t/Nilable java.net.URL)])

(defn ns->URL ^java.net.URL [nsym opts]
  {:pre [(symbol? nsym)]
   :post [((some-fn #(instance? java.net.URL %)
                    nil?) 
           %)]}
  (let [p (ns->file nsym opts)]
    (io/resource p)))
))

(t/ann sym->kw [t/Sym -> t/Kw])
(defn sym->kw [sym]
  {:pre [(symbol? sym)]
   :post [(keyword? %)]}
  (keyword sym))

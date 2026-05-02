;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; experimental
(ns ^{:doc "Wrapper macros that do the right thing for each platform.
           See typed.clojure.jvm for jvm-specific ops."}
  ^:typed.clojure/ignore
  typed.clojure
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                            defn atom ref cast #?(:clj requiring-resolve)])
  (:require #?(;; not loadable in self hosted CLJS, otherwise always needed for
               ;; CLJ AOT compilation compatibility
               :default clojure.core.typed)
            [clojure.core :as cc]
            [clojure.core.typed.platform-case :refer [platform-case]]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

(defmacro ^::ignore ann
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym 
  while type checking. Supports namespace aliases and fully qualified namespaces
  to annotate vars in other namespaces.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  (platform-case
    :clj `(clojure.core.typed/ann ~varsym ~typesyn)
    :cljs `(cljs.core.typed/ann ~varsym ~typesyn)))

(defmacro ^::ignore ann-many [& args]
  (platform-case
    :clj `(clojure.core.typed/ann-many ~@args)
    :cljs (throw (ex-info "ann-many not yet implemented in CLJS" {}))))

(defmacro ann-protocol [& args]
  (platform-case
    :clj `(clojure.core.typed/ann-protocol ~@args)
    :cljs `(cljs.core.typed/ann-protocol ~@args)))

(defmacro ann-datatype [& args]
  (platform-case
    :clj `(clojure.core.typed/ann-datatype ~@args)
    :cljs `(cljs.core.typed/ann-datatype ~@args)))

(defmacro ann-record [& args]
  (platform-case
    :clj `(clojure.core.typed/ann-record ~@args)
    :cljs (throw (ex-info "TODO ann-record in CLJS" {}))))

(defmacro ^::ignore defalias [& args]
  (platform-case
    :clj `(clojure.core.typed/defalias ~@args)
    :cljs `(cljs.core.typed/defalias ~@args)))

(defmacro inst [& args]
  (platform-case
    :clj `(clojure.core.typed/inst ~@args)
    :cljs `(cljs.core.typed/inst ~@args)))

#?(:cljs nil :default
   (defmacro inst-ctor [& args]
     (platform-case
       :clj `(clojure.core.typed/inst-ctor ~@args)
       :cljs (throw (ex-info "inst-ctor does not applicable in CLJS" {})))))

(defmacro declare-datatypes [& args]
  (platform-case
    :clj `(clojure.core.typed/declare-datatypes ~@args)
    :cljs (throw (ex-info "declare-datatypes not yet implemented in CLJS" {}))))

(defmacro declare-protocols [& args]
  (platform-case
    :clj `(clojure.core.typed/declare-protocols ~@args)
    :cljs (throw (ex-info "declare-protocols not yet implemented in CLJS" {}))))

(defmacro declare-alias-kind [& args]
  (platform-case
    :clj `(clojure.core.typed/declare-alias-kind ~@args)
    :cljs (throw (ex-info "declare-alias-kind not yet implemented in CLJS" {}))))

(defmacro declare-names [& args]
  (platform-case
    :clj `(clojure.core.typed/declare-names ~@args)
    :cljs (throw (ex-info "declare-names not yet implemented in CLJS" {}))))

(defmacro def [& args]
  (platform-case
    :clj `(clojure.core.typed/def ~@args)
    :cljs `(cljs.core.typed/def ~@args)))

(defmacro fn [& args]
  (platform-case
    :clj `(clojure.core.typed/fn ~@args)
    :cljs `(cljs.core.typed/fn ~@args)))

(defmacro loop [& args]
  (platform-case
    :clj `(clojure.core.typed/loop ~@args)
    :cljs `(cljs.core.typed/loop ~@args)))

(defmacro ann-form [& args]
  (platform-case
    :clj `(clojure.core.typed/ann-form ~@args)
    :cljs `(cljs.core.typed/ann-form ~@args)))

(defmacro ^::ignore tc-ignore [& args]
  (platform-case
    :clj `(clojure.core.typed/tc-ignore ~@args)
    :cljs `(cljs.core.typed/tc-ignore ~@args)))

(defmacro defprotocol [& args]
  (platform-case
    :clj `(clojure.core.typed/defprotocol ~@args)
    :cljs `(cljs.core.typed/defprotocol ~@args)))

(defmacro defn [& args]
  (platform-case
    :clj `(clojure.core.typed/defn ~@args)
    :cljs `(cljs.core.typed/defn ~@args)))

(defmacro atom
  ;;copied from clojure.core.typed.macros
  "Like atom, but with optional type annotations.
  
  Same as (atom (ann-form init t) args*)
  
  eg. (atom 1) : (Atom (Value 1))
      (atom :- Num, 1) : (Atom Num)"
  [& args]
  (platform-case
    :clj `(clojure.core.typed/atom ~@args)
    :cljs `(cljs.core.typed/atom ~@args)))

#?(:cljs nil :default
(defmacro ref [& args]
   (platform-case
     :clj `(clojure.core.typed/ref ~@args)
     :cljs (throw (ex-info "ref does not exist in CLJS" {})))))

;; checker ops

(cc/defn check-ns-clj
  "In Clojure, checks the current namespace or provided namespaces.
  Similar for self-hosted ClojureScript, except for macros namespaces."
  ([] #?(:cljs (cljs.core.typed/check-ns-macros)
         :default ((requiring-resolve 'typed.clj.checker/check-ns3))))
  ([ns-or-syms & {:as opt}]
   (#?(:cljs cljs.core.typed/check-ns-macros
       :default (requiring-resolve 'typed.clj.checker/check-ns3))
    ns-or-syms
    opt)))

(defmacro cns
  "In Clojure, expands to (check-ns-clj ~@args).
  In ClojureScript JVM, emulates calling (check-ns-cljs ~@args) at expansion time
  (note: args will not be evaluated).
  Self hosted ClojureScript semantics TBD.
  
  Only suitable for REPL development:
  1. (t/cns) or (t/cns 'my-ns.foo)
  2. Fix type errors, save file, go back to 1 until fixed.
  
  Not compatible with AOT compilation."
  [& args]
  (assert (#{0 1} (count args)))
  #?(;; idea:
     ;; - if *ns* ends in $macros, check-ns-clj
     ;; - otherwise, check-ns-cljs
     :default (platform-case
                ;; hmm, should this be evaluated at compile-time too for consistency?
                :clj `(check-ns-clj ~@args))))

(defmacro cf-clj
  "Check a Clojure form in the current *ns*."
  [& args]
  #?(:default (platform-case
                :clj `(clojure.core.typed/cf ~@args))))

;; TODO add check-form-clj{s} defn's for symmetry
(defmacro cf
  "In Clojure, expands to (clojure.core.typed/cf ~@args).
  In ClojureScript JVM, expands to (cljs.core.typed/cf ~@args)."
  [& args]
  #?(:default (platform-case
                :clj `(cf-clj ~@args))))

(defmacro doc-clj
  "Pass any syntax fragment related to Typed Clojure to print documentation on it.
  eg., (doc-clj t/Rec)
       (doc-clj '[])
       (doc-clj +) ;; print var annotation
       (doc-clj MyAlias) ;; print defalias mapping

  Use :doc/index for documentation index."
  [syn] ((requiring-resolve 'typed.cljc.doc/type-doc-clj) syn))

(cc/defn check-dir-clj [dirs]
  ((requiring-resolve 'typed.cljc.dir/check-dir-clj) dirs))

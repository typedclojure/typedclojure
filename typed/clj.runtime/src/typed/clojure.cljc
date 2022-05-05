;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; experimental
(ns ^{:doc "Wrapper macros that do the right thing for each platform.
           See typed.clojure.jvm for jvm-specifics ops."}
  typed.clojure
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                            defn atom ref cast])
  (:require #?(;; not loadable in self hosted CLJS, otherwise always needed for
               ;; CLJ AOT compilation compatibility
               :clj clojure.core.typed
               ;; for self hosted CLJS normal :require's from .clj/c files. for
               ;; .clj{s,c} files, loaded via :require-macros in typed/clojure.cljs.
               :cljs cljs.core.typed)
            [clojure.core :as cc]
            [clojure.core.typed.macros :as macros]))

(alias 't 'typed.clojure)

(defmacro ann
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
  (macros/platform-case
    :clj `(clojure.core.typed/ann ~varsym ~typesyn)
    :cljs `(cljs.core.typed/ann ~varsym ~typesyn)))

(defmacro ann-many [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-many ~@args)
    :cljs (throw (ex-info "ann-many not yet implemented in CLJS" {}))))

(defmacro ann-protocol [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-protocol ~@args)
    :cljs `(cljs.core.typed/ann-protocol ~@args)))

(defmacro ann-datatype [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-datatype ~@args)
    :cljs `(cljs.core.typed/ann-datatype ~@args)))

(defmacro ann-record [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-record ~@args)
    :cljs (throw (ex-info "TODO ann-record in CLJS" {}))))

(defmacro defalias [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/defalias ~@args)
    :cljs `(cljs.core.typed/defalias ~@args)))

(defmacro inst [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/inst ~@args)
    :cljs `(cljs.core.typed/inst ~@args)))

#?(:clj
   (defmacro inst-ctor [& args]
     (macros/platform-case
       :clj `(clojure.core.typed/inst ~@args)
       :cljs (throw (ex-info "inst-ctor does not applicable in CLJS" {})))))

(defmacro declare-datatypes [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-datatypes ~@args)
    :cljs (throw (ex-info "declare-datatypes not yet implemented in CLJS" {}))))

(defmacro declare-protocols [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-protocols ~@args)
    :cljs (throw (ex-info "declare-protocols not yet implemented in CLJS" {}))))

(defmacro declare-alias-kind [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-alias-kind ~@args)
    :cljs (throw (ex-info "declare-alias-kind not yet implemented in CLJS" {}))))

(defmacro declare-names [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-names ~@args)
    :cljs (throw (ex-info "declare-names not yet implemented in CLJS" {}))))

(defmacro def [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/def ~@args)
    :cljs `(cljs.core.typed/def ~@args)))

(defmacro fn [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/fn ~@args)
    :cljs `(cljs.core.typed/fn ~@args)))

(defmacro loop [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/loop ~@args)
    :cljs `(cljs.core.typed/loop ~@args)))

(defmacro ann-form [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-form ~@args)
    :cljs `(cljs.core.typed/ann-form ~@args)))

(defmacro tc-ignore [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/tc-ignore ~@args)
    :cljs `(cljs.core.typed/tc-ignore ~@args)))

(defmacro defprotocol [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/defprotocol ~@args)
    :cljs `(cljs.core.typed/defprotocol ~@args)))

(defmacro defn [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/defn ~@args)
    :cljs `(cljs.core.typed/defn ~@args)))

(defmacro atom [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/atom ~@args)
    :cljs `(cljs.core.typed/atom ~@args)))

#?(:clj
   (defmacro ref [& args]
     (macros/platform-case
       :clj `(clojure.core.typed/ref ~@args)
       :cljs (throw (ex-info "ref does not exist in CLJS" {})))))

;; checker ops

#_ ;; FIXME :no-check removal warning when compiling
(t/ann-many (t/IFn [-> ':ok]
                   [(t/U t/Sym (t/Seqable t/Sym)) -> ':ok])
            ^:no-check check-ns-clj
            ^:no-check check-ns-cljs)
(t/tc-ignore
  (defn check-ns-clj
    "In Clojure, checks the current namespace or provided namespaces.
    Similar for self-hosted ClojureScript, except for macros namespaces."
    ([] #?(:clj ((requiring-resolve 'typed.clj.checker/check-ns3))
           :cljs (cljs.core.typed/check-ns-macros)))
    ([ns-or-syms & {:as opt}]
     (#?(:clj (requiring-resolve 'typed.clj.checker/check-ns3)
         :cljs cljs.core.typed/check-ns-macros)
      ns-or-syms
      opt))))

(t/tc-ignore
  (defn check-ns-cljs
    ([] (check-ns-cljs (ns-name *ns*)))
    ([& args]
     (apply #?(:clj (requiring-resolve 'cljs.core.typed/check-ns*)
               :cljs cljs.core.typed/check-ns*)
            args))))

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
  #?(:clj (macros/platform-case
            ;; hmm, should this be evaluated at compile-time too for consistency?
            :clj `(check-ns-clj ~@args)
            :cljs (apply (requiring-resolve 'cljs.core.typed/check-ns-expansion-side-effects) args))
     ;; idea:
     ;; - if *ns* ends in $macros, check-ns-clj
     ;; - otherwise, check-ns-cljs
     :cljs `(check-ns-cljs ~@args)))

(defmacro cf-clj
  "Check a Clojure form in the current *ns*."
  [& args]
  #?(:clj (macros/platform-case
            :clj `(clojure.core.typed/cf ~@args)
            :cljs (binding [*ns* (create-ns @(requiring-resolve 'cljs.analyzer/*cljs-ns*))]
                    (list 'quote
                          (apply (requiring-resolve 'clojure.core.typed/check-form*)
                                 (case (count args)
                                   ;; form | expected expected-provided?
                                   1 (concat args [nil nil])
                                   ;; form expected | expected-provided?
                                   2 (concat args [true]))))))
     ;;TODO check in macros ns?
     :cljs `(cljs.core.typed/cf ~@args)))

(defmacro cf-cljs
  "Check a ClojureScript form in the same namespace as the current platform."
  [& args]
  #?(:clj (macros/platform-case
            :clj `(with-bindings {(requiring-resolve 'cljs.analyzer/*cljs-ns*) (ns-name *ns*)}
                    (apply (requiring-resolve 'cljs.core.typed/check-form*)
                           '~(case (count args)
                               ;; form | expected expected-provided?
                               1 (concat args [nil nil])
                               ;; form expected | expected-provided?
                               2 (concat args [true]))))
            :cljs (list 'quote
                        (apply (requiring-resolve 'cljs.core.typed/check-form*)
                               (case (count args)
                                 ;; form | expected expected-provided?
                                 1 (concat args [nil nil])
                                 ;; form expected | expected-provided?
                                 2 (concat args [true])))))
     :cljs `(cljs.core.typed/cf ~@args)))

;; TODO add check-form-clj{s} defn's for symmetry
(defmacro cf
  "In Clojure, expands to (clojure.core.typed/cf ~@args).
  In ClojureScript JVM, expands to (cljs.core.typed/cf ~@args)."
  [& args]
  #?(:clj (macros/platform-case
            :clj `(cf-clj ~@args)
            :cljs `(cf-cljs ~@args))
     :cljs `(cf-cljs ~@args)))

(defmacro doc-clj
  "Pass any syntax fragment related to Typed Clojure to print documentation on it.
  eg., (doc-clj t/Rec)
       (doc-clj '[])
       (doc-clj +) ;; print var annotation
       (doc-clj MyAlias) ;; print defalias mapping

  Use :doc/index for documentation index."
  [syn] ((requiring-resolve 'typed.cljc.doc/type-doc-clj) syn))

(defmacro doc-cljs
  "Pass any syntax fragment related to Typed Clojure to print documentation on it.
  eg., (doc-cljs t/Rec)
       (doc-cljs '[])
       (doc-cljs +) ;; print var annotation
       (doc-cljs MyAlias) ;; print defalias mapping

  Use :doc/index for documentation index."
  [syn] ((requiring-resolve 'typed.cljc.doc/type-doc-cljs) syn))

(defn check-dir-clj [dirs]
  ((requiring-resolve 'typed.cljc.dir/check-dir-clj) dirs))

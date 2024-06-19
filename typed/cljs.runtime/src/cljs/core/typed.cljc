;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc
  ^:typed.clojure/ignore
  cljs.core.typed
  "Macros for Clojurescript type checking.

  Note: The (non self hosted) ClojureScript compiler will automatically rewrite clojure.core.typed
  requires to cljs.core.typed, so (:require [clojure.core.typed :as t]) also works in
  ClojureScript, unless self hosting."
  (:refer-clojure :exclude [fn loop let defn atom defprotocol requiring-resolve delay])
  (:require [clojure.core.typed.load-if-needed :as load]
            [clojure.core :as core]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.import-macros :as import-m]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.macros :as macros]
            [clojure.core.typed.util-vars :as vs]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [cljs.analyzer.api :as ana-api]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]))

(core/defn register!
  "Internal -- Do not use"
  []
  ((requiring-resolve 'clojure.core.typed.current-impl/register-cljs!)))

(core/defn load-if-needed
  "Load and initialize all of core.typed if not already"
  []
  (load/load-if-needed true))

(core/defn reset-caches
  "Reset internal type caches."
  []
  nil)

; many of these macros resolve to CLJS functions in 
; the CLJS ns cljs.core.typed

(def ^:private cljs-ns #((requiring-resolve 'typed.cljs.checker.util/cljs-ns)))

(defmacro ^:private delay-tc-parse
  [t opts]
  `(core/let [t# ~t
              app-outer-context# (bound-fn [f#] (f#))
              opts# ~opts]
     (delay
       (app-outer-context#
         (core/fn []
           ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-cljs) t#
            (assoc opts# :typed.clj.checker.parse-unparse/parse-type-in-ns (cljs-ns))))))))

(def ^:private int-error #(apply (requiring-resolve 'clojure.core.typed.errors/int-error) %&)) 

(defmacro ^:private ^:no-doc with-current-location
  [opts {:keys [form env] :as m}]
  (assert (map? m))
  `(core/let [opts# ~opts
              form# ~form
              env# ~env]
     (assoc opts# ::vs/current-env
            {:ns (or (:ns env#)
                     {:name (cljs-ns)})
             :line (or (-> form# meta :line)
                       (:line env#)
                       :column (or (-> form# meta :column)
                                   (:column env#)))})))

(core/defn ^:no-doc add-tc-type-name [form qsym t]
  {:pre [(seq? form)
         (qualified-symbol? qsym)]}
  (impl/with-cljs-impl
    (core/let
      [;; preserve *ns*
       opts ((requiring-resolve 'typed.cljs.runtime.env/cljs-opts))
       bfn (bound-fn []
             (core/let [opts (with-current-location opts {:form form :env nil})]
               @(delay-tc-parse t opts)))
       t (delay
           ;(prn "CLJS" qsym t)
           (core/let [;unparse-type (requiring-resolve 'typed.clj.checker.parse-unparse/unparse-type)
                      t (bfn)
                      #_#_
                      _ (impl/with-cljs-impl
                          (when-let [tfn ((requiring-resolve 'typed.cljc.checker.declared-kind-env/declared-kind-or-nil) qsym)]
                            (when-not ((requiring-resolve 'typed.clj.checker.subtype/subtype?) t tfn)
                              (int-error (str "Declared kind " (unparse-type tfn)
                                              " does not match actual kind " (unparse-type t))))))]
             t))]
      ((requiring-resolve 'clojure.core.typed.current-impl/add-tc-type-name)
       ((requiring-resolve 'clojure.core.typed.current-impl/cljs-checker))
       qsym t)))
  nil)

(core/defn ^:no-doc
  ann*-macro-time
  "Internal use only. Use ann."
  [qsym typesyn check? form env]
  (core/let [opts (with-current-location
                    ((requiring-resolve 'typed.cljs.runtime.env/cljs-opts))
                    {:form form :env env})
             checker (impl/cljs-checker)
             _ (when (and (contains? (impl/var-env checker) qsym)
                          (not (impl/check-var? checker qsym))
                          check?)
                 (err/warn (str "Removing :no-check from var " qsym))
                 (impl/remove-nocheck-var checker qsym))
             _ (when-not check?
                 (impl/add-nocheck-var checker qsym))
             #_#_ast (delay-rt-parse typesyn)
             tc-type (delay-tc-parse typesyn opts)]
    #_(impl/with-impl impl/clojurescript
        (impl/add-var-env checker qsym ast))
    (impl/with-impl impl/clojurescript
      (impl/add-tc-var-type checker qsym tc-type)))
  nil)

#?(:clj
   (defn- qualify-sym [varsym]
     (core/let [current-nsym (cljs-ns)
                varsym-nsym (some-> (namespace varsym) symbol)
                qsym (symbol (name
                               (or (get ((requiring-resolve 'typed.cljs.checker.util/get-aliases)
                                         current-nsym)
                                        varsym-nsym)
                                   varsym-nsym
                                   current-nsym))
                             (name varsym))]
       qsym)))

(defmacro ann
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym while type checking.

  eg. ; annotate the var foo in this namespace
  (ann foo [Number -> Number])

  ; annotate a var in another namespace
  (ann another.ns/bar [-> nil])

  ; don't check this var
  (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  {:pre [(symbol? varsym)]}
  (core/let [qsym (qualify-sym varsym)
             #_#_ ;;FIXME warn if (namespace qsym) is cljs.core
             _ (when-not (ana-api/resolve &env varsym) ;; FIXME throws on missing?
                 (println (str "WARNING: " varsym " not resolvable, annotating as " qsym)))
             opts (meta varsym)
             check? (not (:no-check opts))]
    (ann*-macro-time qsym typesyn check? &form &env)
    nil))

(core/defn- ^:no-doc
  ann-protocol*-macro-time
  [vbnd varsym mth form]
  (core/let [opts ((requiring-resolve 'typed.cljs.runtime.env/cljs-opts))
             checker ((requiring-resolve 'clojure.core.typed.current-impl/cljs-checker))
             add-protocol-env (requiring-resolve 'clojure.core.typed.current-impl/add-protocol-env)
             gen-protocol* (requiring-resolve 'clojure.core.typed.current-impl/gen-protocol*)
             qualsym (if (namespace varsym)
                       varsym
                       (symbol (name (cljs-ns)) (name varsym)))]
    #_
    (impl/with-cljs-impl
      (add-protocol-env
        checker
        qualsym
        {:name qualsym
         :methods mth
         :bnds vbnd}))
    (impl/with-cljs-impl
      (core/let [opts (with-current-location opts {:form form :env nil})]
        (gen-protocol*
          (::vs/current-env opts)
          (cljs-ns)
          varsym
          vbnd
          mth
          checker
          opts))))
  nil)

(defmacro 
  ^{:forms '[(ann-protocol vbnd varsym & methods)
             (ann-protocol varsym & methods)]}
  ann-protocol 
  "Annotate a possibly polymorphic protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])

      ; polymorphic
      (ann-protocol [[x :variance :covariant]]
        IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])"
  [& args]
  (core/let [bnd-provided? (vector? (first args))
             vbnd (when bnd-provided?
                    (first args))
             [varsym & mth] (if bnd-provided?
                              (next args)
                              args)
             _ (assert (symbol? varsym) "Protocol name must be a symbol")
             _ (assert (not (vector? (first mth)))
                       "Type variable binder goes before protocol name in ann-protocol")
             _ (core/let [fs (frequencies (map first (partition 2 mth)))]
                 (when-let [dups (seq (filter (core/fn [[_ freq]] (< 1 freq)) fs))]
                   (println (str "WARNING: Duplicate method annotations in ann-protocol (" varsym 
                                 "): " (str/join ", " (map first dups))))))
             ; duplicates are checked above.
             {:as mth} mth
             qsym (qualify-sym varsym)]
    (ann-protocol*-macro-time vbnd qsym mth &form)
    nil))

(defmacro ann-jsnominal
  "Equivalent of TypeScript interface"
  [varsym jsnom]
  (core/let [qualsym (if (namespace varsym)
                       varsym
                       (symbol (str (ns-name *ns*)) (name varsym)))]
    (assert nil "NYI")
    nil))

(core/defn ^:no-doc
  ann-datatype*-macro-time
  "Internal use only. Use ann-datatype."
  [vbnd dname fields opts form]
  (impl/with-cljs-impl
    (core/let [opts ((requiring-resolve 'typed.cljs.runtime.env/cljs-opts))
               checker ((requiring-resolve 'clojure.core.typed.current-impl/cljs-checker))
               add-datatype-env (requiring-resolve 'clojure.core.typed.current-impl/add-datatype-env)
               gen-datatype* (requiring-resolve 'clojure.core.typed.current-impl/gen-datatype*)
               dname-nsym (some-> dname namespace symbol)
               qname (with-meta
                       (symbol (name
                                 (or (get ((requiring-resolve 'typed.cljs.checker.util/get-aliases)
                                           (cljs-ns))
                                          dname-nsym)
                                     dname-nsym
                                     (cljs-ns)))
                               (name dname))
                       (meta dname))]
      #_
      (add-datatype-env 
        checker
        qname
        {:record? false
         :name qname
         :fields fields
         :bnd vbnd})
      (core/let [opts (with-current-location opts {:form form :env nil})]
        (gen-datatype* (::vs/current-env opts) (cljs-ns) qname fields vbnd opts false checker opts))
      nil)))

(defmacro
  ^{:forms '[(ann-datatype dname [field :- type*] opts*)
             (ann-datatype binder dname [field :- type*] opts*)]}
  ann-datatype
  "Annotate datatype Class name dname with expected fields.
  If unqualified, qualify in the current namespace.

  eg. (ann-datatype MyDatatype [a :- Number,
  b :- Long])

  (ann-datatype another.ns.TheirDatatype
  [str :- String,
  vec :- (IPersistentVector Number)])"
  [& args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
  (core/let [bnd-provided? (vector? (first args))
             vbnd (when bnd-provided?
                    (first args))
             [dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
             (if bnd-provided?
               (next args)
               args)]
    (assert (not rplc) "Replace NYI")
    (assert (symbol? dname)
            (str "Must provide name symbol: " dname))
    (ann-datatype*-macro-time vbnd dname fields opts &form)
    nil))

(defn- defalias*-macro-time
  [form qsym t]
  (add-tc-type-name form qsym t)
  nil)

(defmacro defalias 
  "Define a type alias. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (defalias MyAlias
        \"Here is my alias\"
        (U nil String))"
  ([sym doc-str t]
   (assert (symbol? sym) (str "First argument to defalias must be a symbol: " sym))
   (assert (string? doc-str) "Doc-string passed to defalias must be a string")
   (with-meta `(defalias ~(vary-meta sym assoc :doc doc-str) ~t)
              (meta &form)))
  ([sym t]
   (assert (symbol? sym) (str "First argument to defalias must be a symbol: " sym))
   (core/let [current-nsym (cljs-ns)
              sym-nsym (some-> (namespace sym) symbol)
              qsym (with-meta
                     (symbol (name
                               (or (get ((requiring-resolve 'typed.cljs.checker.util/get-aliases)
                                         current-nsym)
                                        sym-nsym)
                                   sym-nsym
                                   current-nsym))
                             (name sym))
                     (meta sym))]
     (defalias*-macro-time &form qsym t)
     nil)))

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  inst-of)

(defmacro 
  ^{:forms '[(letfn> [fn-spec-or-annotation*] expr*)]}
  letfn>
  "Like letfn, but each function spec must be annotated.

  eg. (letfn> [a :- [Number -> Number]
               (a [b] 2)

               c :- [Symbol -> nil]
               (c [s] nil)]
        ...)"
  [fn-specs-and-annotations & body]
  (core/let [bindings fn-specs-and-annotations
             ; (Vector (U '[Symbol TypeSyn] LetFnInit))
             normalised-bindings
             (core/loop [[fbnd :as bindings] bindings
                         norm []]
               (cond
                 (empty? bindings) norm
                 (symbol? fbnd) (do
                                  (assert (#{:-} (second bindings))
                                          "letfn> annotations require :- separator")
                                  (assert (<= 3 (count bindings)))
                                  (recur 
                                    (drop 3 bindings)
                                    (conj norm [(nth bindings 0)
                                                (nth bindings 2)])))
                 (list? fbnd) (recur
                                (next bindings)
                                (conj norm fbnd))
                 :else (throw (Exception. (str "Unknown syntax to letfn>: " fbnd)))))
             {anns false inits true} (group-by list? normalised-bindings)
             ; init-syn unquotes local binding references to be compatible with hygienic expansion
             init-syn (into {}
                            (for [[lb type] anns]
                              [lb `'~type]))]
    `(cljs.core/letfn ~(vec inits)
       ;unquoted to allow bindings to resolve with hygiene
       ~init-syn
       ;;preserve letfn empty body
       ;;nil
       ~@body)))

(core/defn check-form*
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided? & {:as opt}]
  ((requiring-resolve 'typed.cljs.checker/check-form) form expected expected-provided? opt))

(core/defn check-form-info 
  [form & opt]
  (apply (requiring-resolve 'typed.cljs.checker/check-form-info) form opt))

(defmacro cf
  "Check a single form with an optional expected type."
  ([form] `(check-form* '~form nil nil))
  ([form expected] `(check-form* '~form '~expected true)))

(core/defn check-ns-info
  "Check a Clojurescript namespace, or the current namespace.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see check-ns."
  ([] ((requiring-resolve 'typed.cljs.checker/check-ns-info)))
  ([ns-or-syms & {:as opt}] ((requiring-resolve 'typed.cljs.checker/check-ns-info) ns-or-syms opt)))

(core/defn check-ns-macros [& args]
  #?(:clj (apply (requiring-resolve 'clojure.core.typed/check-ns) args)
     :cljs (throw (ex-info "check-ns-macros not yet implemented in self hosted CLJS" {}))))

(core/defn check-ns-info-macros [& args]
  #?(:clj (apply (requiring-resolve 'clojure.core.typed/check-ns-info) args)
     :cljs (throw (ex-info "check-ns-info-macros not yet implemented in self hosted CLJS" {}))))

(core/defn check-ns*
  "Check a Clojurescript namespace, or the current namespace.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see check-ns."
  ([] ((requiring-resolve 'typed.cljs.checker/check-ns)))
  ([ns-or-syms & {:as opt}]
   ((requiring-resolve 'typed.cljs.checker/check-ns) ns-or-syms opt)))

(core/defn ^:internal check-ns-expansion-side-effects
  ([] (check-ns* (cljs-ns)))
  ([ns-or-syms]
   (core/let [quoted? #(and (seq? %)
                            (= 'quote (first %))
                            (= 2 (count %)))
              quoted-symbol? (every-pred quoted? (comp simple-symbol? second))
              quoted-coll? (every-pred quoted? (comp coll? second))
              _ (assert (coll? ns-or-syms)
                        (str "check-ns arguments must be quoted symbols: " (pr-str ns-or-syms)))
              ns-or-syms (mapv (core/fn [quoted-sym]
                                 (assert (quoted-symbol? quoted-sym)
                                         (str "All args to check-ns must be quoted simple symbols: " (pr-str quoted-sym)))
                                 (second quoted-sym))
                               (if (quoted-symbol? ns-or-syms)
                                 [ns-or-syms]
                                 (if (quoted-coll? ns-or-syms)
                                   ;; '(quote [foo bar]) => ['(quote foo) '(quote bar)]
                                   (map #(list 'quote %) (second ns-or-syms))
                                   ns-or-syms)))]
     (check-ns* ns-or-syms))))

(defmacro check-ns
  "Check a Clojurescript namespace, or the current namespace with zero args. This macro
  is intended to be called at the Clojurescript REPL with a subset of
  clojure.core.typed/check-ns's features.
 
  For the equivalent function (callable from Clojure only) see cljs.core.typed/check-ns*."
  ([] 
   (check-ns-expansion-side-effects))
  ([ns-or-syms]
   (check-ns-expansion-side-effects ns-or-syms)))

(import-m/import-macros clojure.core.typed.macros
  [fn tc-ignore ann-form def loop let defn atom defprotocol])

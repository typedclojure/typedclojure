;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.fnl.analyzer
  "Analyzer for Fennel code. Provides integration with typed.fnl.reader."
  (:refer-clojure :exclude [macroexpand-1])
  (:require [typed.cljc.analyzer :as ana]
            [typed.cljc.analyzer.utils :as u]
            [typed.clj.analyzer :as clj-ana]
            [typed.fnl.reader :as fnl-reader]))

(defn empty-env
  "Returns an empty env map for Fennel analysis.
  For Fennel, we don't use namespaces like Clojure, but we still need
  an :ns key for compatibility with the analyzer infrastructure."
  ([]
   (empty-env 'fennel.user))
  ([ns-name]
   {:context :ctx/expr
    :locals  {}
    :ns {:name ns-name}}))

(defn unanalyzed
  "Create an :unanalyzed AST node from a Fennel form.
  
  The form should be a Clojure data structure as returned by typed.fnl.reader.
  We reuse the clj analyzer's unanalyzed function since Fennel forms are
  represented as Clojure data structures."
  [form env opts]
  {:pre [(map? env)]}
  (clj-ana/unanalyzed form env opts))

;;; Fennel Special Form Parsers

(defn parse-let
  "Parse Fennel 'let' form (non-destructuring version).
  (let [x 1 y 2] body...)"
  [form env opts]
  (ana/parse-let* form env opts))

(defn parse-local
  "Parse Fennel 'local' form for local variable declaration.
  (local x 10)"
  [[_ name init :as form] env opts]
  (when-not (symbol? name)
    (throw (ex-info "local requires a symbol name" {:form form})))
  {:op :local
   ::ana/op ::local
   :env env
   :form form
   :name name
   :init (unanalyzed init env opts)
   :children [:init]})

(defn parse-var
  "Parse Fennel 'var' form for mutable variable declaration.
  (var x 10)"
  [[_ name init :as form] env opts]
  (when-not (symbol? name)
    (throw (ex-info "var requires a symbol name" {:form form})))
  {:op :var
   ::ana/op ::var
   :env env
   :form form
   :name name
   :init (when init (unanalyzed init env opts))
   :children (if init [:init] [])})

(defn parse-values
  "Parse Fennel 'values' form for multiple return values.
  (values 1 2 3)"
  [[_ & exprs :as form] env opts]
  {:op :values
   ::ana/op ::values
   :env env
   :form form
   :exprs (mapv #(unanalyzed % env opts) exprs)
   :children [:exprs]})

(defn parse-lua
  "Parse Fennel 'lua' form for embedding Lua code.
  (lua \"return 42\")"
  [[_ lua-str :as form] env opts]
  (when-not (string? lua-str)
    (throw (ex-info "lua requires a string argument" {:form form})))
  {:op :lua
   ::ana/op ::lua
   :env env
   :form form
   :code lua-str
   :children []})

(defn parse-comment
  "Parse Fennel 'comment' form (ignores body, returns nil).
  (comment ...)"
  [form env opts]
  {:op :const
   ::ana/op ::ana/const
   :env env
   :form form
   :type :nil
   :val nil})

(defn parse-or
  "Parse Fennel 'or' form.
  (or a b c)"
  [[_ & exprs :as form] env opts]
  (when (empty? exprs)
    (throw (ex-info "or requires at least one argument" {:form form})))
  {:op :or
   ::ana/op ::or
   :env env
   :form form
   :exprs (mapv #(unanalyzed % env opts) exprs)
   :children [:exprs]})

(defn parse-and
  "Parse Fennel 'and' form.
  (and a b c)"
  [[_ & exprs :as form] env opts]
  (when (empty? exprs)
    (throw (ex-info "and requires at least one argument" {:form form})))
  {:op :and
   ::ana/op ::and
   :env env
   :form form
   :exprs (mapv #(unanalyzed % env opts) exprs)
   :children [:exprs]})

(defn parse-include
  "Parse Fennel 'include' form for compile-time code inclusion.
  (include \"module\")"
  [[_ module-name & opts-args :as form] env opts]
  {:op :include
   ::ana/op ::include
   :env env
   :form form
   :module module-name
   :options opts-args
   :children []})

(defn parse-require-macros
  "Parse Fennel 'require-macros' form.
  (require-macros {: foo} :bar)"
  [[_ & args :as form] env opts]
  {:op :require-macros
   ::ana/op ::require-macros
   :env env
   :form form
   :args args
   :children []})

(defn parse-unquote
  "Parse Fennel 'unquote' form.
  ,expr"
  [[_ expr :as form] env opts]
  {:op :unquote
   ::ana/op ::unquote
   :env env
   :form form
   :expr (unanalyzed expr env opts)
   :children [:expr]})

(defn -parse
  "Extension to typed.cljc.analyzer/-parse for Fennel special forms.
  
  Dispatches on the first element of the form to determine which special
  form parser to use. Falls back to invoke for regular function calls."
  [form env opts]
  ((case (first form)
     ;; Reuse clojure's do parser since it's identical in Fennel
     do              ana/parse-do
     ;; Fennel-specific special forms
     let             parse-let
     local           parse-local
     var             parse-var
     values          parse-values
     lua             parse-lua
     comment         parse-comment
     or              parse-or
     and             parse-and
     quote           ana/parse-quote
     include         parse-include
     require-macros  parse-require-macros
     unquote         parse-unquote
     #_:else         ana/parse-invoke)
   form env opts))

;;TODO
(defn macroexpand-1 [form env opts] form)

(declare -analyze-outer)

(defn default-opts
  "Returns default analysis options for Fennel.
  
  Uses clj analyzer's default-opts as a base since Fennel forms
  are represented as Clojure data structures."
  []
  {::ana/resolve-ns (fn [sym env opts]
                      (throw (ex-info "TODO typed.fnl.analyzer/resolve-ns" {})))
   ::ana/current-ns-name (fn [env opts]
                           (throw (ex-info "TODO typed.fnl.analyzer/current-ns-name" {})))
   ::ana/parse -parse
   ::ana/eval-ast (fn [ast opts]
                    (throw (ex-info "TODO typed.fnl.analyzer/eval-ast" {})))
   ::ana/create-var (fn [sym env opts]
                      (throw (ex-info "TODO typed.fnl.analyzer/create-var" {})))
   ::ana/unanalyzed unanalyzed
   ::ana/macroexpand-1 macroexpand-1
   ::ana/analyze-outer -analyze-outer
   ;;TODO
   ::ana/scheduled-passes (fn [opts]
                            {:pre (fn [x opts] x)
                             :post (fn [x opts] x)
                             :init-ast (fn [x opts] x)})
   ::ana/var? (fn [x opts]
                ;;TODO
                #_(throw (ex-info "TODO typed.fnl.analyzer/var?" {}))
                false
                )
   ::ana/var->sym (fn [& args] (throw (ex-info "TODO var->sym" {})))
   ::ana/resolve-sym (fn [& args]
                       ;;TODO
                       #_(throw (ex-info "TODO resolve-sym" {})))
   }
  )

(defn analyze-seq
  "Analyzes a Fennel sequence (list) form.
  
  Follows typed.cljc.analyzer/analyze-seq pattern:
  1. Checks for nil operator
  2. Attempts macro expansion
  3. If not a macro, parses as special form or function invocation"
  [form env opts]
  (let [op (first form)]
    (when (nil? op)
      (throw (ex-info "Can't call nil"
                      (merge {:form form}
                             (u/-source-info form env)))))
    (let [mform (macroexpand-1 form env opts)]
      (if (identical? form mform) ;; not a macro, parse as special form or invoke
        (-parse mform env opts)
        ;; macro expanded, analyze the result
        (-> (unanalyzed mform env opts)
            (update :raw-forms (fnil conj ())
                    (vary-meta form assoc ::resolved-op (ana/resolve-sym op env opts))))))))

(defn analyze-form
  "Analyzes a Fennel form. Dispatches based on the form type."
  [form env opts]
  (cond
    (seq? form) (analyze-seq form env opts)
    :else (ana/analyze-const form env opts)))

(defn -analyze-outer
  "If ast is :unanalyzed, then analyze it, otherwise returns ast.
  
  For strings, creates an analyzed :const node with type information.
  For other values, delegates to analyze-form for full analysis."
  [ast {::ana/keys [current-ns-name] :as opts}]
  (case (:op ast)
    :unanalyzed (let [{::ana/keys [config]
                       :keys [form env]} ast]
                  (-> form
                      (ana/analyze-form env opts)
                      (assoc ::ana/config config)
                      ana/propagate-top-level))
    ast))

(defn analyze
  "Analyzes a Fennel form.
  
  The form should be a Clojure data structure as returned by typed.fnl.reader.
  This creates an analyzed AST node that can be type-checked."
  [form env opts]
  (ana/run-passes (unanalyzed form env opts) opts))

(defn read-fennel
  "Read Fennel source code and return Clojure data structures.
  
  Uses typed.fnl.reader to parse Fennel syntax into Clojure-friendly forms."
  ([source] (fnl-reader/read-string source))
  ([source opts] (fnl-reader/read-string source opts)))

(defn read-fennel-file
  "Read a Fennel file and return all forms as Clojure data structures."
  [filename]
  (fnl-reader/read-file filename))

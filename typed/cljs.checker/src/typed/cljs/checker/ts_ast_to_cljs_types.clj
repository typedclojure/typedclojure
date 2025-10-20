;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.ts-ast-to-cljs-types
  "Converts TypeScript AST nodes (from ts-declaration-parser) to Typed ClojureScript types.
  
  This namespace provides functions to translate TypeScript type AST nodes
  into their corresponding Typed ClojureScript Type objects."
  (:require [clojure.string :as str]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.parse-unparse :as prs]))

(defn ast->Type
  "Convert a TypeScript AST node to a Typed ClojureScript Type.
  
  The AST format is the output from typed.cljs.checker.ts-declaration-parser/parse-type.
  
  Examples:
    {:base \"any\"} => (r/-unchecked nil)
    {:base \"unknown\"} => r/-any
    {:union [{:base \"string\"} {:base \"number\"}]} => (r/Union-maker [...])]
  
  Returns a Type object from typed.cljc.checker.type_rep."
  [ast-node opts]
  ;;TODO non-linear dispatch
  (let [ast->Type (fn
                    ([ast-node] (ast->Type ast-node opts))
                    ([ast-node opts] (ast->Type ast-node opts)))]
    (cond
      ;; Base types (primitives and keywords)
      (contains? ast-node :base)
      (case (:base ast-node)
        ;; Keyword types - special TypeScript types
        "any" (r/-unchecked nil)
        "unknown" r/-any
        "void" r/-nil
        "never" r/-nothing
        
        ;; Primitive types - use JS primitive types for TypeScript
        "string" (r/JSString-maker)
        "number" (r/JSNumber-maker)
        "boolean" (r/JSBoolean-maker)
        "symbol" (r/Name-maker 'js/Symbol)
        "bigint" r/-integer-cljs  ; TypeScript bigint -> ClojureScript integer
        "object" (r/JSObject-maker)  ; TS object is any JavaScript object
        
        ;; Literal types
        "null" (r/JSNull-maker)
        "undefined" (r/JSUndefined-maker)
        "true" (r/-val true)
        "false" (r/-val false)
        
        ;; Built-in types (capitalized versions)
        "String" (r/JSString-maker)
        "Number" (r/JSNumber-maker)
        "Boolean" (r/JSBoolean-maker)
        "Object" (r/JSObject-maker)
        "Symbol" (r/Name-maker 'js/Symbol)
        "BigInt" r/-integer-cljs
        
        "Array" (r/ArrayCLJS-maker r/-any r/-any)  ; Untyped array
        "ReadonlyArray" (r/ArrayCLJS-maker r/-any r/-any)  ; ReadonlyArray is also just an array
        
        ;; Built-in JavaScript types - use Name with js/ namespace
        "Date" (r/Name-maker 'js/Date)
        "RegExp" (r/Name-maker 'js/RegExp)
        "Error" (r/Name-maker 'js/Error)
        
        ;; Default: treat as identifier/type name reference
        ;; For simple identifiers (like type parameters), treat as free variables
        ;; For qualified names, treat as Name types
        (let [base-str (:base ast-node)]
          (if (or (str/includes? base-str ".")  ; Has dots (e.g., "foo.bar")
                  (str/includes? base-str "/"))  ; Has namespace separator
            ;; Qualified name - use Name type
            (r/Name-maker (symbol base-str))
            ;; Simple identifier - treat as free variable (F type)
            ;; This handles type parameters like T, U, etc.
            (r/make-F (symbol base-str)))))
      
      ;; Literal types
      (contains? ast-node :literal)
      (let [lit (:literal ast-node)]
        ;; String and number literals become singleton types using r/-val
        ;; Parser returns strings for all literals
        ;; Try to parse as number if it looks numeric, otherwise use as string
        (r/-val 
          (if (and (string? lit) (re-matches #"-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?" lit))
            ;; Numeric string - parse to number
            (try
              (if (str/includes? lit ".")
                (Double/parseDouble lit)
                (Long/parseLong lit))
              (catch Exception _
                lit))
            ;; Non-numeric string - keep as string
            lit)))
      
      (contains? ast-node :union) (c/make-Union (mapv ast->Type (:union ast-node)) opts)
      (contains? ast-node :intersection) (c/make-Intersection (mapv ast->Type (:intersection ast-node)) opts)
      
      ;; Array types - TypeScript arrays are JavaScript arrays
      (contains? ast-node :array)
      (let [elem-type (ast->Type (:array ast-node))]
        (r/ArrayCLJS-maker elem-type elem-type))
      
      ;; Generic/parameterized types
      (contains? ast-node :generic)
      (let [name (:generic ast-node)
            args (:args ast-node)]
        ;; Handle common generic types specially
        (case name
          "Array" (let [elem-type (ast->Type (first args))]
                    (r/ArrayCLJS-maker elem-type elem-type))
          "ReadonlyArray" (let [elem-type (ast->Type (first args))]
                            (r/ArrayCLJS-maker elem-type elem-type))
          ;;TODO
          ;"Promise" (prs/parse-type (list 'js/Promise (ast->Type (first args))) opts)
          ;; TypeScript Map and Set are JavaScript Map and Set, not Clojure immutable collections
          ;; For now, we'll use JSObject as we don't have specific JS Map/Set types
          "Map" (r/JSObject-maker)  ; TODO: Need specific JS Map type
          "Set" (r/JSObject-maker)  ; TODO: Need specific JS Set type
          "Record" (r/JSObject-maker)  ; Record<K, V> is an object type
          
          ;; Default: reference to a generic type
          ;; Need to parse this as a type application
          ;TODO
          #_
          (prs/parse-type (cons (symbol name) (map ast->Type args)) opts)))
      
      ;; Function/arrow types
      (contains? ast-node :arrow)
      (let [{:keys [params return]} (:arrow ast-node)
            ;; Convert parameters to types
            param-types (mapv (fn [p]
                                (let [base-type (ast->Type (:type p))]
                                  (if (:optional p)
                                    (c/make-Union [base-type (r/JSUndefined-maker)] opts)
                                    base-type)))
                              params)
            return-type (ast->Type return)
            ;; Create a Result from the return type
            result (r/make-Result return-type)
            ;; Create a Function with the parameters and result
            fn-type (r/make-Function param-types result)]
        ;; Wrap in FnIntersection (TypeScript functions map to t/IFn)
        (r/make-FnIntersection fn-type))
      
      ;; Object types (interfaces/object literals)
      (contains? ast-node :object)
      ;; TypeScript object types map to JSObject
      ;; For simple property-only objects, we use JSObject
      ;; Index signatures { [key: string]: number } also map to JSObject
      ;; (JSObject has limited expressiveness but is the best fit)
      (r/JSObject-maker)
      
      ;; Utility types
      (contains? ast-node :utility)
      (let [util-name (:utility ast-node)
            args (:args ast-node)]
        (case util-name
          ;; NonNullable<T> - remove null and undefined
          "NonNullable" (let [inner-type (ast->Type (first args))]
                          (c/make-Intersection [inner-type 
                                                (r/NotType-maker r/-nil)]
                                               opts))
          
          ;; Default - unsupported utility type
          r/-any))
      
      ;; Readonly types - Readonly<T>
      (contains? ast-node :readonly)
      ;; ClojureScript data structures are immutable by default
      ;; So we can just translate the inner type
      (ast->Type (:readonly ast-node))
      
      ;; typeof queries
      (contains? ast-node :typeof)
      ;; typeof expressions - use TypeOf when the value/symbol is available
      (let [expr (:typeof ast-node)]
        (if (and (map? expr) (contains? expr :base))
          ;; Simple identifier - can use t/TypeOf
          (r/TypeOf-maker (symbol (:base expr)))
          ;; Complex expression - use Any for now
          r/-any))
      
      ;; This types - this in class/object contexts
      (contains? ast-node :this)
      ;; For now, map to r/-any (could be improved with proper self types later)
      r/-any
      
      ;; Type predicate - x is Type  
      (contains? ast-node :type-predicate)
      ;; For now, map to function type returning JSBoolean (could be improved later)
      (let [pred-type (ast->Type (:type-predicate ast-node))]
        ;; A type predicate function: (x: any) => x is T maps to [any -> JSBoolean]
        (r/make-FnIntersection (r/make-Function [r/-any] (r/make-Result (r/JSBoolean-maker)))))
      
      ;; Template literal types
      (contains? ast-node :template-literal)
      ;; Template literal types are TypeScript-specific string manipulation
      ;; Map to JSString
      (r/JSString-maker)
      
      :else (err/nyi-error (str "TypeScript to Typed ClojureScript translation for: " (binding [*print-level* 2] (pr-str ast-node)))
                           opts))))

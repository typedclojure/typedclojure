;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.ts-declaration-parser
  "TypeScript declaration file parser using Instaparse.
  
  This parser handles TypeScript declaration syntax and converts it to
  a programmatically manipulatable AST format. It supports:
  - Types: primitives, unions, intersections, generics, arrays, tuples, function types
  - Declarations: functions, type aliases, interfaces, classes, enums
  
  The parser outputs a 'friendly format' - maps with known keys for recognized constructs."
  (:require [instaparse.core :as insta]))

(def ts-grammar
  "Instaparse grammar for TypeScript declarations and types"
  (insta/parser
    "
    <S> = declaration+
    
    (* Top-level declarations *)
    declaration = function-decl | type-alias | interface-decl | class-decl | enum-decl | var-decl
    
    function-decl = <'function'> identifier type-params? <'('> param-list? <')'> return-type? <';'>
    
    type-alias = <'type'> identifier type-params? <'='> type <';'>
    
    interface-decl = <'export'?> <'interface'> identifier type-params? extends-clause? <'{'> member* <'}'>
    
    class-decl = <'export'?> <'class'> identifier type-params? extends-clause? implements-clause? <'{'> class-member* <'}'>
    
    enum-decl = <'export'?> <'enum'> identifier <'{'> enum-member-list? <'}'>
    
    var-decl = <'declare'?> var-kind identifier <':'> type <';'>
    
    var-kind = 'const' | 'let' | 'var'
    
    (* Interface/class members *)
    member = property-sig | method-sig | call-sig | construct-sig | index-sig
    
    property-sig = readonly? identifier optional? <':'> type <';'?>
    
    method-sig = identifier type-params? <'('> param-list? <')'> <':'> type <';'?>
    
    call-sig = <'('> param-list? <')'> <':'> type <';'?>
    
    construct-sig = <'new'> <'('> param-list? <')'> <':'> type <';'?>
    
    index-sig = <'['> identifier <':'> type <']'> <':'> type <';'?>
    
    class-member = property-member | method-member | constructor-member
    
    property-member = modifier* identifier optional? <':'> type <';'?>
    
    method-member = modifier* identifier type-params? <'('> param-list? <')'> <':'> type <';'?>
    
    constructor-member = <'constructor'> <'('> param-list? <')'> <';'?>
    
    modifier = 'public' | 'private' | 'protected' | 'static' | 'readonly'
    
    readonly = <'readonly'>
    
    (* Enum members *)
    enum-member-list = enum-member (<','> enum-member)* <','?>
    
    enum-member = identifier (<'='> (number | string-literal))?
    
    (* Type parameters *)
    type-params = <'<'> type-param-list <'>'>
    
    type-param-list = type-param (<','> type-param)*
    
    type-param = identifier (<'extends'> type)? (<'='> type)?
    
    (* Parameters *)
    param-list = param (<','> param)*
    
    param = rest-param | optional-param | required-param
    
    required-param = identifier <':'> type
    
    optional-param = identifier <'?'> <':'> type
    
    rest-param = <'...'> identifier <':'> type
    
    (* Return type *)
    return-type = <':'> type
    
    (* Extends/implements clauses *)
    extends-clause = <'extends'> type-ref-list
    
    implements-clause = <'implements'> type-ref-list
    
    type-ref-list = type-ref (<','> type-ref)*
    
    type-ref = identifier type-args?
    
    (* Types *)
    type = union-type
    
    union-type = intersection-type (<'|'> intersection-type)*
    
    intersection-type = primary-type (<'&'> primary-type)*
    
    primary-type = function-type | array-type | tuple-type | generic-type | 
                   parenthesized-type | literal-type | primitive-type | 
                   object-type | type-query | keyword-type | identifier
    
    function-type = <'('> param-list? <')'> <'=>'> type
    
    array-type = primary-type <'['> <']'>
    
    tuple-type = <'['> type (<','> type)* <']'>
    
    generic-type = identifier type-args
    
    type-args = <'<'> type (<','> type)* <'>'>
    
    parenthesized-type = <'('> type <')'>
    
    object-type = <'{'> member* <'}'>
    
    type-query = <'typeof'> identifier
    
    keyword-type = 'any' | 'unknown' | 'never' | 'void'
    
    literal-type = string-literal | number | boolean-literal | null-literal | undefined-literal
    
    primitive-type = 'string' | 'number' | 'boolean' | 'object' | 'symbol' | 'bigint'
    
    boolean-literal = 'true' | 'false'
    
    null-literal = 'null'
    
    undefined-literal = 'undefined'
    
    optional = <'?'>
    
    (* Identifiers and literals *)
    identifier = #'[a-zA-Z_$][a-zA-Z0-9_$.]*'
    
    string-literal = <'\\\"'> #'[^\\\"]*' <'\\\"'> | <\"'\"> #\"[^']*\" <\"'\">
    
    number = #'-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?'
    "
    :auto-whitespace :standard))

;; ========== Translation to Friendly Format ==========

(declare translate-type translate-member translate-class-member)

(defn- translate-identifier
  [[_ name]]
  name)

(defn- translate-param
  [param-node]
  (let [actual-param (if (= :param (first param-node))
                       (second param-node)
                       param-node)]
    (case (first actual-param)
      :required-param
      (let [[_ [_ name] type-node] actual-param]
        {:name name
         :type (translate-type type-node)
         :optional false
         :variadic false})
      
      :optional-param
      (let [[_ [_ name] type-node] actual-param]
        {:name name
         :type (translate-type type-node)
         :optional true
         :variadic false})
      
      :rest-param
      (let [[_ [_ name] type-node] actual-param]
        {:name name
         :type (translate-type type-node)
         :optional false
         :variadic true}))))

(defn- translate-param-list
  [[_ & params]]
  (mapv translate-param params))

(defn- translate-type-param
  [type-param-node]
  (let [[_ [_ name] & rest] type-param-node
        constraint (when (and (seq rest) (= :type (first (first rest))))
                     (translate-type (first rest)))
        default (when (and (> (count rest) 1) (= :type (first (second rest))))
                   (translate-type (second rest)))]
    (cond-> {:name name}
      constraint (assoc :constraint constraint)
      default (assoc :default default))))

(defn- translate-type-params
  [type-params-node]
  (let [[_ type-param-list-node] type-params-node
        type-params (rest type-param-list-node)]
    (mapv translate-type-param type-params)))

(defn- translate-type
  [type-node]
  (case (first type-node)
    :type (translate-type (second type-node))
    
    :union-type
    (if (= 2 (count type-node))
      (translate-type (second type-node))
      {:union (mapv translate-type (rest type-node))})
    
    :intersection-type
    (if (= 2 (count type-node))
      (translate-type (second type-node))
      {:intersection (mapv translate-type (rest type-node))})
    
    :primary-type
    (translate-type (second type-node))
    
    :primitive-type
    {:base (second type-node)}
    
    :keyword-type
    {:base (second type-node)}
    
    :literal-type
    (translate-type (second type-node))
    
    :string-literal
    {:literal (second type-node)}
    
    :number
    {:literal (second type-node)}
    
    :boolean-literal
    {:literal (second type-node)}
    
    :null-literal
    {:base "null"}
    
    :undefined-literal
    {:base "undefined"}
    
    :identifier
    {:base (second type-node)}
    
    :array-type
    (let [[_ elem-type] type-node]
      {:array (translate-type elem-type)})
    
    :tuple-type
    {:tuple (mapv translate-type (rest type-node))}
    
    :generic-type
    (let [[_ [_ name] [_ & type-args]] type-node]
      {:generic name
       :args (mapv translate-type type-args)})
    
    :function-type
    (let [parts (rest type-node)
          param-list-node (when (= :param-list (first (first parts)))
                            (first parts))
          parts (if param-list-node (rest parts) parts)
          return-type-node (first parts)
          params (if param-list-node
                   (translate-param-list param-list-node)
                   [])]
      {:arrow {:params params
               :return (translate-type return-type-node)}})
    
    :parenthesized-type
    (translate-type (second type-node))
    
    :object-type
    (let [[_ & members] type-node]
      {:object (mapv #(translate-member %) members)})
    
    :type-query
    (let [[_ [_ name]] type-node]
      {:typeof name})
    
    :type-ref
    (let [[_ [_ name] type-args-node] type-node]
      (if type-args-node
        {:generic name
         :args (mapv translate-type (rest type-args-node))}
        {:base name}))))

(defn- translate-member
  [member-node]
  (let [actual-member (if (= :member (first member-node))
                        (second member-node)
                        member-node)]
    (case (first actual-member)
      :property-sig
      (let [parts (rest actual-member)
            readonly? (= :readonly (first (first parts)))
            parts (if readonly? (rest parts) parts)
            [_ name] (first parts)
            rest-parts (rest parts)
            opt-node (when (= :optional (first (first rest-parts)))
                       (first rest-parts))
            rest-parts (if opt-node (rest rest-parts) rest-parts)
            type-node (first rest-parts)]
        {:kind :property
         :name name
         :type (translate-type type-node)
         :optional (boolean opt-node)
         :readonly readonly?})
      
      :method-sig
      (let [parts (rest actual-member)
            [_ name] (first parts)
            rest-parts (rest parts)
            type-params-node (when (= :type-params (first (first rest-parts)))
                               (first rest-parts))
            rest-parts (if type-params-node (rest rest-parts) rest-parts)
            param-list-node (when (= :param-list (first (first rest-parts)))
                              (first rest-parts))
            rest-parts (if param-list-node (rest rest-parts) rest-parts)
            type-node (first rest-parts)
            type-params (when type-params-node
                          (translate-type-params type-params-node))
            params (if param-list-node
                     (translate-param-list param-list-node)
                     [])]
        (cond-> {:kind :method
                 :name name
                 :params params
                 :return-type (translate-type type-node)}
          type-params (assoc :type-params type-params)))
      
      :call-sig
      (let [[_ param-list-node type-node] actual-member
            params (if param-list-node
                     (translate-param-list param-list-node)
                     [])]
        {:kind :call-signature
         :params params
         :return-type (translate-type type-node)})
      
      :construct-sig
      (let [[_ param-list-node type-node] actual-member
            params (if param-list-node
                     (translate-param-list param-list-node)
                     [])]
        {:kind :construct-signature
         :params params
         :return-type (translate-type type-node)})
      
      :index-sig
      (let [[_ [_ key-name] key-type-node value-type-node] actual-member]
        {:kind :index-signature
         :key-name key-name
         :key-type (translate-type key-type-node)
         :value-type (translate-type value-type-node)}))))

(defn- translate-function-decl
  [func-node]
  (let [parts (rest func-node)
        [_ name] (first parts)
        rest-parts (rest parts)
        type-params-node (when (= :type-params (first (first rest-parts)))
                           (first rest-parts))
        rest-parts (if type-params-node (rest rest-parts) rest-parts)
        param-list-node (when (= :param-list (first (first rest-parts)))
                          (first rest-parts))
        rest-parts (if param-list-node (rest rest-parts) rest-parts)
        return-type-node (when (= :return-type (first (first rest-parts)))
                           (first rest-parts))
        type-params (when type-params-node
                      (translate-type-params type-params-node))
        params (if param-list-node
                 (translate-param-list param-list-node)
                 [])
        return-type (if return-type-node
                      (translate-type (second return-type-node))
                      {:base "void"})]
    (cond-> {:name name
             :params params
             :return-type return-type}
      type-params (assoc :type-params type-params))))

(defn- translate-type-alias
  [alias-node]
  (let [parts (rest alias-node)
        [_ name] (first parts)
        rest-parts (rest parts)
        type-params-node (when (= :type-params (first (first rest-parts)))
                           (first rest-parts))
        rest-parts (if type-params-node (rest rest-parts) rest-parts)
        type-node (first rest-parts)
        type-params (when type-params-node
                      (translate-type-params type-params-node))]
    (cond-> {:name name
             :type (translate-type type-node)}
      type-params (assoc :type-params type-params))))

(defn- translate-interface-decl
  [interface-node]
  (let [parts (rest interface-node)
        [id-node & rest-parts] parts
        [_ name] id-node
        type-params-node (when (= :type-params (first (first rest-parts)))
                           (first rest-parts))
        rest-parts (if type-params-node (rest rest-parts) rest-parts)
        extends-node (when (= :extends-clause (first (first rest-parts)))
                       (first rest-parts))
        rest-parts (if extends-node (rest rest-parts) rest-parts)
        members rest-parts]
    (cond-> {:name name
             :members (mapv translate-member members)}
      type-params-node (assoc :type-params (translate-type-params type-params-node))
      extends-node (assoc :extends (mapv #(translate-type (second %))
                                          (rest extends-node))))))

(defn- translate-class-decl
  [class-node]
  (let [parts (rest class-node)
        [id-node & rest-parts] parts
        [_ name] id-node
        type-params-node (when (= :type-params (first (first rest-parts)))
                           (first rest-parts))
        rest-parts (if type-params-node (rest rest-parts) rest-parts)
        extends-node (when (= :extends-clause (first (first rest-parts)))
                       (first rest-parts))
        rest-parts (if extends-node (rest rest-parts) rest-parts)
        implements-node (when (= :implements-clause (first (first rest-parts)))
                          (first rest-parts))
        rest-parts (if implements-node (rest rest-parts) rest-parts)
        members rest-parts]
    (cond-> {:name name
             :members (mapv #(translate-class-member %) members)}
      type-params-node (assoc :type-params (translate-type-params type-params-node))
      extends-node (assoc :extends (mapv #(translate-type (second %))
                                          (rest extends-node)))
      implements-node (assoc :implements (mapv #(translate-type (second %))
                                               (rest implements-node))))))

(defn- translate-class-member
  [member-node]
  (let [actual-member (if (= :class-member (first member-node))
                        (second member-node)
                        member-node)]
    (case (first actual-member)
      :property-member
      (let [parts (rest actual-member)
            modifiers (take-while #(= :modifier (first %)) parts)
            rest-parts (drop-while #(= :modifier (first %)) parts)
            [_ name] (first rest-parts)
            rest-parts (rest rest-parts)
            opt-node (when (= :optional (first (first rest-parts)))
                       (first rest-parts))
            rest-parts (if opt-node (rest rest-parts) rest-parts)
            type-node (first rest-parts)]
        {:kind :property
         :name name
         :type (translate-type type-node)
         :optional (boolean opt-node)
         :modifiers (mapv second modifiers)})
      
      :method-member
      (let [parts (rest actual-member)
            modifiers (take-while #(= :modifier (first %)) parts)
            rest-parts (drop-while #(= :modifier (first %)) parts)
            [id-node & method-parts] rest-parts
            [_ name] id-node
            type-params-node (when (= :type-params (first (first method-parts)))
                               (first method-parts))
            method-parts (if type-params-node (rest method-parts) method-parts)
            [param-list-node type-node] method-parts
            params (if param-list-node
                     (translate-param-list param-list-node)
                     [])]
        (cond-> {:kind :method
                 :name name
                 :params params
                 :return-type (translate-type type-node)
                 :modifiers (mapv second modifiers)}
          type-params-node (assoc :type-params (translate-type-params type-params-node))))
      
      :constructor-member
      (let [[_ param-list-node] actual-member
            params (if param-list-node
                     (translate-param-list param-list-node)
                     [])]
        {:kind :constructor
         :params params}))))

(defn- translate-enum-decl
  [enum-node]
  (let [[_ [_ name] enum-members-node] enum-node
        members (when enum-members-node
                  (mapv (fn [member-node]
                          (let [[_ [_ member-name] value-node] member-node]
                            (if value-node
                              {:name member-name
                               :value (second value-node)}
                              {:name member-name})))
                        (rest enum-members-node)))]
    {:name name
     :members (or members [])}))

(defn- translate-var-decl
  [var-node]
  (let [[_ [_ kind] [_ name] type-node] var-node]
    {:name name
     :kind kind
     :type (translate-type type-node)}))

(defn- translate-declaration
  [decl-node]
  (case (first decl-node)
    :function-decl
    {:type :function
     :declaration (translate-function-decl decl-node)}
    
    :type-alias
    {:type :type-alias
     :declaration (translate-type-alias decl-node)}
    
    :interface-decl
    {:type :interface
     :declaration (translate-interface-decl decl-node)}
    
    :class-decl
    {:type :class
     :declaration (translate-class-decl decl-node)}
    
    :enum-decl
    {:type :enum
     :declaration (translate-enum-decl decl-node)}
    
    :var-decl
    {:type :variable
     :declaration (translate-var-decl decl-node)}))

(defn parse-type
  "Parse a TypeScript type string into an AST."
  [type-str]
  (let [result (insta/parse ts-grammar type-str :start :type)]
    (if (insta/failure? result)
      (throw (ex-info "Failed to parse TypeScript type"
                      {:input type-str
                       :failure (insta/get-failure result)}))
      (translate-type result))))

(defn- translate-to-friendly-format
  "Translate the parse tree to a TypedClojureScript-friendly format.
  
  Returns a map with keys like :functions, :type-aliases, :interfaces, etc."
  [parse-tree]
  (let [;; parse-tree is a list/seq of [:declaration ...] nodes
        declarations (if (sequential? parse-tree)
                       (if (= :declaration (first (first parse-tree)))
                         ;; Extract the actual declaration nodes from inside [:declaration ...]
                         (map second parse-tree)
                         parse-tree)
                       [parse-tree])
        translated (mapv translate-declaration declarations)
        grouped (group-by :type translated)]
    (into {} (remove (comp empty? val))
          {:functions (mapv :declaration (:function grouped))
           :type-aliases (mapv :declaration (:type-alias grouped))
           :interfaces (mapv :declaration (:interface grouped))
           :classes (mapv :declaration (:class grouped))
           :enums (mapv :declaration (:enum grouped))
           :variables (mapv :declaration (:variable grouped))})))

(defn parse-declaration
  "Parse a TypeScript declaration string into an AST."
  [decl-str]
  (let [result (insta/parse ts-grammar decl-str)]
    (if (insta/failure? result)
      (throw (ex-info "Failed to parse TypeScript declaration"
                      {:input decl-str
                       :failure (insta/get-failure result)}))
      (translate-to-friendly-format result))))

(defn parse-declaration-file
  "Parse a TypeScript declaration file into an AST."
  [decl-file]
  (parse-declaration (slurp decl-file)))

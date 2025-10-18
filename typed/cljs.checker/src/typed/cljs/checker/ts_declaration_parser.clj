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
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def ts-grammar
  "Instaparse grammar for TypeScript declarations and types"
  (insta/parser
    "
    <S> = statement+
    
    (* Statements include declarations, imports, exports, directives, and comments *)
    statement = comment | triple-slash-directive | import-statement | export-statement | declaration
    
    (* Comments *)
    comment = <'/*'> #'[^*]*\\*+(?:[^/*][^*]*\\*+)*' <'/'>
            | <'//'> <#'[^\\n]*'> <#'\\n'?>
    
    (* Triple-slash directives *)
    triple-slash-directive = <'///'> <#'[^\\n]*'> <#'\\n'?>
    
    (* Import statements *)
    import-statement = <'import'> import-clause <'from'> string-literal <';'>
                     | <'import'> identifier <'='> <'require'> <'('> string-literal <')'> <';'>
                     | <'import'> string-literal <';'>
    
    import-clause = identifier
                  | <'{'> identifier-list <'}'>
                  | identifier <','> <'{'> identifier-list <'}'>
                  | <'*'> <'as'> identifier
    
    identifier-list = identifier (<','> identifier)*
    
    (* Export statements *)
    export-statement = <'export'> declaration
                     | <'export'> <'as'> <'namespace'> identifier <';'>
                     | <'export'> <'{'> identifier-list <'}'> (<'from'> string-literal)? <';'>
                     | <'export'> <'*'> (<'as'> identifier)? <'from'> string-literal <';'>
                     | <'export'> <'='> identifier <';'>
                     | <'export'> <'default'> (declaration | identifier) <';'?>
    
    (* Top-level declarations *)
    declaration = function-decl | type-alias | interface-decl | class-decl | enum-decl | var-decl | namespace-decl | module-decl | declare-block
    
    function-decl = export-modifier? declare-modifier? <'function'> identifier type-params? <'('> param-list? <')'> (asserts-decl | type-predicate-decl | return-type)? <';'>
    
    asserts-decl = <':'> <'asserts'> identifier (<'is'> type)?
    
    type-predicate-decl = <':'> identifier <'is'> type
    
    type-alias = export-modifier? declare-modifier? <'type'> identifier type-params? <'='> type <';'>
    
    interface-decl = export-modifier? declare-modifier? <'interface'> identifier type-params? extends-clause? <'{'> member* <'}'>
    
    class-decl = export-modifier? declare-modifier? decorator* abstract-modifier? <'class'> identifier type-params? extends-clause? implements-clause? <'{'> class-member* <'}'>
    
    abstract-modifier = <'abstract'>
    
    enum-decl = export-modifier? declare-modifier? <'enum'> identifier <'{'> enum-member-list? <'}'>
    
    var-decl = export-modifier? declare-modifier? var-kind identifier <':'> type (<'='> var-initializer)? <';'>
    
    var-initializer = identifier | string-literal | number | boolean-literal | <'{'> <'}'> | <'['> <']'>
    
    namespace-decl = declare-modifier? <'namespace'> identifier <'{'> statement* <'}'>
    
    module-decl = declare-modifier? <'module'> (string-literal | module-wildcard) <'{'> statement* <'}'>
    
    module-wildcard = string-literal-with-wildcard
    
    string-literal-with-wildcard = <'\\\"'> #'[^\\\"]*\\*[^\\\"]*' <'\\\"'> | <\"'\"> #\"[^']*\\*[^']*\" <\"'\">
    
    declare-block = <'declare'> (<'global'> | <''>) <'{'> statement* <'}'>
    
    export-modifier = <'export'>
    
    declare-modifier = <'declare'>
    
    var-kind = 'const' | 'let' | 'var'
    
    var-kind = 'const' | 'let' | 'var'
    
    (* Interface/class members *)
    member = comment* (property-sig | method-sig | call-sig | construct-sig | index-sig)
    
    property-sig = readonly? property-name optional? <':'> type <';'?>
    
    method-sig = property-name type-params? <'('> param-list? <')'> <':'> type <';'?>
    
    property-name = identifier | string-literal | computed-property-name
    
    computed-property-name = <'['> identifier <']'> | <'['> string-literal <']'>
    
    call-sig = <'('> param-list? <')'> <':'> type <';'?>
    
    construct-sig = <'new'> <'('> param-list? <')'> <':'> type <';'?>
    
    index-sig = readonly? <'['> identifier <':'> type <']'> <':'> type <';'?>
    
    class-member = comment* decorator* (accessor-member | property-member | method-member | constructor-member)
    
    decorator = <'@'> identifier (<'('> decorator-args? <')'>)?
    
    decorator-args = identifier (<','> identifier)*
    
    accessor-member = modifier* (<'get'> | <'set'>) (identifier | private-identifier) <'('> param-list? <')'> <':'> type <';'?>
    
    property-member = modifier* (identifier | private-identifier) optional? <':'> type <';'?>
    
    method-member = modifier* (identifier | private-identifier) type-params? <'('> param-list? <')'> <':'> type <';'?>
    
    constructor-member = <'constructor'> <'('> param-list? <')'> <';'?>
    
    private-identifier = <'#'> identifier
    
    modifier = 'public' | 'private' | 'protected' | 'static' | 'readonly' | 'abstract'
    
    readonly = <'readonly'>
    
    (* Enum members *)
    enum-member-list = enum-member (<','> enum-member)* <','?>
    
    enum-member = identifier (<'='> (number | string-literal))?
    
    (* Type parameters *)
    type-params = <'<'> type-param-list <'>'>
    
    type-param-list = type-param (<','> type-param)*
    
    type-param = variance-modifier? const-modifier? identifier (<'extends'> type)? (<'='> type)?
    
    variance-modifier = 'in' | 'out'
    
    const-modifier = <'const'>
    
    (* Parameters *)
    param-list = param (<','> param)* <','?>
    
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
    
    intersection-type = conditional-type (<'&'> conditional-type)*
    
    conditional-type = primary-type (<'extends'> type <'?'> type <':'> type)?
    
    primary-type = constructor-type | function-type | indexed-access-type | array-type | tuple-type | readonly-array-type | readonly-type | generic-type | 
                   parenthesized-type | literal-type | primitive-type | builtin-type | utility-type |
                   mapped-type | object-type | keyof-type | type-query | import-type | keyword-type | infer-type | template-literal-type | identifier
    
    template-literal-type = <'`'> template-literal-part* <'`'>
    
    template-literal-part = template-string | template-substitution
    
    template-string = #'[^`$]+'
    
    template-substitution = <'${'> type <'}'>
    
    utility-type = #'(Partial|Required|Pick|Omit|Record|Exclude|Extract|NonNullable|ReturnType|InstanceType|Parameters|ConstructorParameters|ThisType|Uppercase|Lowercase|Capitalize|Uncapitalize|Awaited|OmitThisParameter|ThisParameterType|NoInfer)' type-args
    
    infer-type = <'infer'> identifier
    
    indexed-access-type = postfix-type <'['> type <']'>
    
    postfix-type = generic-type | parenthesized-type | literal-type | primitive-type | builtin-type |
                   object-type | keyof-type | type-query | keyword-type | identifier
    
    mapped-type = <'{'> mapped-type-readonly? <'['> identifier <'in'> type <']'> mapped-type-optional? <':'> type <'}'>
    
    mapped-type-readonly = 'readonly' | '+readonly' | '-readonly'
    
    mapped-type-optional = '?' | '+?' | '-?'
    
    readonly-type = #'Readonly' type-args
    
    readonly-array-type = #'ReadonlyArray' type-args
    
    constructor-type = <'new'> <'('> param-list? <')'> <'=>'> type
    
    function-type = <'('> param-list? <')'> <'=>'> type
    
    array-type = postfix-type <'['> <']'>
    
    tuple-type = <'['> tuple-element (<','> tuple-element)* <']'>
    
    tuple-element = rest-element | optional-element | type
    
    rest-element = <'...'> type
    
    optional-element = type <'?'>
    
    generic-type = identifier type-args
    
    type-args = <'<'> type (<','> type)* <'>'>
    
    parenthesized-type = <'('> type <')'>
    
    object-type = <'{'> member* <'}'>
    
    type-query-expr = identifier (<'.'> identifier | <'['> string-literal <']'>)*
    type-query = <'typeof'> type-query-expr
    
    import-type = <'import'> <'('> string-literal <')'> (<'.'> identifier)*
    
    keyof-type = <'keyof'> type
    
    keyword-type = 'any' | 'unknown' | 'never' | 'void'
    
    literal-type = string-literal | number | boolean-literal | null-literal | undefined-literal
    
    primitive-type = 'string' | 'number' | 'boolean' | 'object' | 'symbol' | 'bigint' | unique-symbol
    
    unique-symbol = <'unique'> <'symbol'>
    
    builtin-type = 'Date' | 'Buffer' | 'Error' | 'RegExp' | 'Function' | 'Promise' | 'Map' | 'Set' 
                  | 'WeakMap' | 'WeakSet' | 'ArrayBuffer' | 'DataView' | 'SharedArrayBuffer'
                  | 'Int8Array' | 'Uint8Array' | 'Uint8ClampedArray' | 'Int16Array' | 'Uint16Array'
                  | 'Int32Array' | 'Uint32Array' | 'Float32Array' | 'Float64Array'
                  | 'BigInt64Array' | 'BigUint64Array'
                  | 'Intl' | 'JSON' | 'Math' | 'Reflect' | 'Proxy' | 'Atomics'
                  | 'String' | 'Number' | 'Boolean' | 'Object' | 'Symbol' | 'BigInt'
                  | 'Array' | 'ReadonlyArray'
    
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
  (let [parts (rest type-param-node)
        variance-node (when (= :variance-modifier (first (first parts)))
                        (first parts))
        parts (if variance-node (rest parts) parts)
        const-node (when (= :const-modifier (first (first parts)))
                     (first parts))
        parts (if const-node (rest parts) parts)
        [_ name] (first parts)
        rest-parts (rest parts)
        constraint (when (and (seq rest-parts) (= :type (first (first rest-parts))))
                     (translate-type (first rest-parts)))
        default (when (and (> (count rest-parts) 1) (= :type (first (second rest-parts))))
                   (translate-type (second rest-parts)))]
    (cond-> {:name name}
      variance-node (assoc :variance (second variance-node))
      const-node (assoc :const true)
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
    
    :conditional-type
    (if (= 2 (count type-node))
      (translate-type (second type-node))
      (let [[_ check-type extends-type true-type false-type] type-node]
        {:conditional {:check (translate-type check-type)
                       :extends (translate-type extends-type)
                       :true (translate-type true-type)
                       :false (translate-type false-type)}}))
    
    :infer-type
    (let [[_ [_ name]] type-node]
      {:infer name})
    
    :indexed-access-type
    (let [[_ obj-type index-type] type-node]
      {:indexed-access {:object (translate-type obj-type)
                        :index (translate-type index-type)}})
    
    :mapped-type
    (let [parts (rest type-node)
          readonly-node (when (= :mapped-type-readonly (first (first parts)))
                          (first parts))
          parts (if readonly-node (rest parts) parts)
          [_ param-name] (first parts)
          parts (rest parts)
          in-type (first parts)
          parts (rest parts)
          optional-node (when (= :mapped-type-optional (first (first parts)))
                          (first parts))
          parts (if optional-node (rest parts) parts)
          value-type (first parts)]
      (cond-> {:mapped {:param param-name
                        :in (translate-type in-type)
                        :type (translate-type value-type)}}
        readonly-node (assoc-in [:mapped :readonly] (second readonly-node))
        optional-node (assoc-in [:mapped :optional] (second optional-node))))
    
    :postfix-type
    (translate-type (second type-node))
    
    :primary-type
    (translate-type (second type-node))
    
    :primitive-type
    (if (= :unique-symbol (first (second type-node)))
      {:base "unique symbol"}
      {:base (second type-node)})
    
    :unique-symbol
    {:base "unique symbol"}
    
    :builtin-type
    {:base (second type-node)}
    
    :keyword-type
    {:base (second type-node)}
    
    :readonly-type
    (let [[_ type-arg] type-node]
      {:readonly (translate-type type-arg)})
    
    :readonly-array-type
    (let [[_ [_ & type-args]] type-node]
      {:readonly-array (mapv translate-type type-args)})
    
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
    (let [elements (rest type-node)]
      {:tuple (mapv (fn [elem]
                      (if (= :tuple-element (first elem))
                        (let [inner (second elem)]
                          (case (first inner)
                            :rest-element {:rest (translate-type (second inner))}
                            :optional-element {:optional (translate-type (second inner))}
                            :type (translate-type inner)))
                        (translate-type elem)))
                    elements)})
    
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
    
    :constructor-type
    (let [parts (rest type-node)
          param-list-node (when (= :param-list (first (first parts)))
                            (first parts))
          parts (if param-list-node (rest parts) parts)
          return-type-node (first parts)
          params (if param-list-node
                   (translate-param-list param-list-node)
                   [])]
      {:constructor {:params params
                     :return (translate-type return-type-node)}})
    
    :parenthesized-type
    (translate-type (second type-node))
    
    :object-type
    (let [[_ & members] type-node]
      {:object (mapv #(translate-member %) members)})
    
    :type-query-expr
    (let [[_ & parts] type-node]
      ;; Build a string representation: identifier followed by .prop or ["prop"] accesses
      (str/join "" (map (fn [part]
                         (cond
                           (and (vector? part) (= :identifier (first part)))
                           (second part)
                           (string? part)
                           part
                           (and (vector? part) (= :string-literal (first part)))
                           (str "[\"" (second part) "\"]")
                           :else
                           (str part)))
                       parts)))
    
    :type-query
    (let [[_ expr] type-node]
      {:typeof (translate-type expr)})
    
    :import-type
    (let [parts (rest type-node)
          module-name (second (first parts))
          properties (mapv #(second %) (rest parts))]
      (if (seq properties)
        {:import-type {:module module-name :path properties}}
        {:import-type {:module module-name}}))
    
    :keyof-type
    (let [[_ target-type] type-node]
      {:keyof (translate-type target-type)})
    
    :utility-type
    (let [[_ util-name [_ & type-args]] type-node]
      {:utility util-name
       :args (mapv translate-type type-args)})
    
    :template-literal-type
    (let [parts (rest type-node)]
      {:template-literal (mapv (fn [part]
                                  (if (= :template-literal-part (first part))
                                    (let [inner (second part)]
                                      (case (first inner)
                                        :template-string {:string (second inner)}
                                        :template-substitution {:type (translate-type (second inner))}))
                                    (case (first part)
                                      :template-string {:string (second part)}
                                      :template-substitution {:type (translate-type (second part))})))
                                parts)})
    
    :type-ref
    (let [[_ [_ name] type-args-node] type-node]
      (if type-args-node
        {:generic name
         :args (mapv translate-type (rest type-args-node))}
        {:base name}))
    
    ;; Default case - throw descriptive error for unsupported type nodes
    (throw (ex-info (str "Unsupported TypeScript type node: " (first type-node))
                    {:type-node-tag (first type-node)
                     :full-node type-node
                     :message "This TypeScript construct is not yet supported by the parser. Please report this with the full-node details."}))))

(defn- translate-member
  [member-node]
  (let [;; Skip any comment nodes at the beginning
        parts (if (= :member (first member-node))
                (rest member-node)
                [member-node])
        ;; Filter out comment nodes
        parts (filter #(not= :comment (first %)) parts)
        actual-member (first parts)]
    (case (first actual-member)
      :property-sig
      (let [parts (rest actual-member)
            readonly? (= :readonly (first (first parts)))
            parts (if readonly? (rest parts) parts)
            name-node (first parts)
            name (if (= :property-name (first name-node))
                   (let [child (second name-node)]
                     (case (first child)
                       :identifier (second child)
                       :string-literal (second child)
                       :computed-property-name (let [inner (second child)]
                                                 (if (= :identifier (first inner))
                                                   (str "[" (second inner) "]")
                                                   (str "[" (second inner) "]")))
                       (second child))) ;; fallback
                   (second name-node)) ;; fallback to old behavior
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
            name-node (first parts)
            name (if (= :property-name (first name-node))
                   (let [child (second name-node)]
                     (case (first child)
                       :identifier (second child)
                       :string-literal (second child)
                       :computed-property-name (let [inner (second child)]
                                                 (if (= :identifier (first inner))
                                                   (str "[" (second inner) "]")
                                                   (str "[" (second inner) "]")))
                       (second child))) ;; fallback
                   (second name-node)) ;; fallback to old behavior
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
      (let [parts (rest actual-member)
            readonly? (= :readonly (first (first parts)))
            parts (if readonly? (rest parts) parts)
            [_ key-name] (first parts)
            key-type-node (second parts)
            value-type-node (nth parts 2)]
        {:kind :index-signature
         :key-name key-name
         :key-type (translate-type key-type-node)
         :value-type (translate-type value-type-node)
         :readonly readonly?}))))

(defn- translate-function-decl
  [func-node]
  (let [parts (->> (rest func-node)
                   (remove #(#{:export-modifier :declare-modifier} (first %))))
        [_ name] (first parts)
        rest-parts (rest parts)
        type-params-node (when (= :type-params (first (first rest-parts)))
                           (first rest-parts))
        rest-parts (if type-params-node (rest rest-parts) rest-parts)
        param-list-node (when (= :param-list (first (first rest-parts)))
                          (first rest-parts))
        rest-parts (if param-list-node (rest rest-parts) rest-parts)
        asserts-node (when (= :asserts-decl (first (first rest-parts)))
                       (first rest-parts))
        type-pred-node (when (and (not asserts-node)
                                  (= :type-predicate-decl (first (first rest-parts))))
                         (first rest-parts))
        return-type-node (when (and (not asserts-node)
                                    (not type-pred-node) 
                                    (= :return-type (first (first rest-parts))))
                           (first rest-parts))
        type-params (when type-params-node
                      (translate-type-params type-params-node))
        params (if param-list-node
                 (translate-param-list param-list-node)
                 [])
        return-type (cond
                      asserts-node
                      (let [parts (rest asserts-node)
                            [_ param-name] (first parts)
                            type-node (when (> (count parts) 1)
                                        (second parts))]
                        (if type-node
                          {:asserts {:param param-name
                                    :type (translate-type type-node)}}
                          {:asserts {:param param-name}}))
                      type-pred-node
                      (let [[_ [_ param-name] pred-type] type-pred-node]
                        {:type-predicate {:param param-name
                                         :type (translate-type pred-type)}})
                      return-type-node
                      (translate-type (second return-type-node))
                      :else
                      {:base "void"})]
    (cond-> {:name name
             :params params
             :return-type return-type}
      type-params (assoc :type-params type-params))))

(defn- translate-type-alias
  [alias-node]
  (let [parts (->> (rest alias-node)
                   (remove #(#{:export-modifier :declare-modifier} (first %))))
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
  (let [parts (->> (rest interface-node)
                   (remove #(#{:export-modifier :declare-modifier} (first %))))
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
  (let [parts (->> (rest class-node)
                   (remove #(#{:export-modifier :declare-modifier :abstract-modifier} (first %))))
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
  (let [;; Skip any comment nodes at the beginning
        parts (if (= :class-member (first member-node))
                (rest member-node)
                [member-node])
        ;; Filter out comment and decorator nodes
        parts (filter #(and (not= :comment (first %)) (not= :decorator (first %))) parts)
        actual-member (first parts)]
    (case (first actual-member)
      :accessor-member
      (let [parts (rest actual-member)
            modifiers (take-while #(= :modifier (first %)) parts)
            rest-parts (drop-while #(= :modifier (first %)) parts)
            accessor-type (first rest-parts) ; either "get" or "set"
            name-node (second rest-parts)
            name (if (= :private-identifier (first name-node))
                   (str "#" (second (second name-node)))
                   (second name-node))
            method-parts (drop 2 rest-parts)
            [param-list-node type-node] method-parts
            params (if param-list-node
                     (translate-param-list param-list-node)
                     [])]
        {:kind :accessor
         :accessor-type accessor-type
         :name name
         :params params
         :return-type (translate-type type-node)
         :modifiers (mapv second modifiers)})
      
      :property-member
      (let [parts (rest actual-member)
            modifiers (take-while #(= :modifier (first %)) parts)
            rest-parts (drop-while #(= :modifier (first %)) parts)
            name-node (first rest-parts)
            name (if (= :private-identifier (first name-node))
                   (str "#" (second (second name-node)))
                   (second name-node))
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
            name-node (first rest-parts)
            name (if (= :private-identifier (first name-node))
                   (str "#" (second (second name-node)))
                   (second name-node))
            method-parts (rest rest-parts)
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
  (let [parts (->> (rest enum-node)
                   (remove #(#{:export-modifier :declare-modifier} (first %))))
        [_ name] (first parts)
        enum-members-node (second parts)
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
  (let [parts (->> (rest var-node)
                   (remove #(#{:export-modifier :declare-modifier} (first %))))
        [_ kind] (first parts)
        [_ name] (second parts)
        type-node (nth parts 2)]
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
     :declaration (translate-var-decl decl-node)}
    
    :module-decl
    ;; Module declarations are ignored for now
    nil
    
    :namespace-decl
    ;; Namespace declarations are ignored for now
    nil))

(defn parse-type
  "Parse a TypeScript type string into an AST."
  [type-str]
  (let [result (insta/parse ts-grammar type-str :start :type)]
    (if (insta/failure? result)
      (throw (ex-info "Failed to parse TypeScript type"
                      {:input type-str
                       :failure (insta/get-failure result)}))
      (translate-type result))))

(defn- extract-declaration
  "Extract a declaration from a statement node."
  [stmt-node]
  (case (first stmt-node)
    :statement (extract-declaration (second stmt-node))
    :export-statement 
    ;; Check if the export-statement contains a declaration
    (let [child (second stmt-node)]
      (when (and child (= :declaration (first child)))
        (extract-declaration child)))
    :declaration (second stmt-node)
    :triple-slash-directive nil
    :import-statement nil
    :comment nil
    :module-decl nil
    :namespace-decl nil
    :declare-block nil
    ;; If it's already a declaration type node, return it
    stmt-node))

(defn- translate-to-friendly-format
  "Translate the parse tree to a TypedClojureScript-friendly format.
  
  Returns a map with keys like :functions, :type-aliases, :interfaces, etc."
  [parse-tree]
  (let [;; parse-tree is a list/seq of [:statement ...] or [:declaration ...] nodes
        statements (if (sequential? parse-tree)
                     (if (or (= :statement (first (first parse-tree)))
                             (= :declaration (first (first parse-tree))))
                       parse-tree
                       [parse-tree])
                     [parse-tree])
        ;; Extract declarations from statements (ignoring imports, exports, directives)
        declarations (->> statements
                         (map extract-declaration)
                         (filter some?)
                         (filter #(not= :namespace-decl (first %)))
                         (filter #(not= :declare-block (first %))))
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

(ns typed-test.cljs.checker.ts-declaration-parser-test
  "Tests for TypeScript declaration parser"
  (:require [clojure.test :refer [deftest is testing]]
            [typed.cljs.checker.ts-declaration-parser :as parser]))

;; ========== Type Parsing Tests ==========

(deftest parse-primitive-types-test
  (testing "Primitive types can be parsed"
    (is (= {:base "string"} (parser/parse-type "string")))
    (is (= {:base "number"} (parser/parse-type "number")))
    (is (= {:base "boolean"} (parser/parse-type "boolean")))
    (is (= {:base "object"} (parser/parse-type "object")))))

(deftest parse-keyword-types-test
  (testing "Keyword types can be parsed"
    (is (= {:base "any"} (parser/parse-type "any")))
    (is (= {:base "unknown"} (parser/parse-type "unknown")))
    (is (= {:base "void"} (parser/parse-type "void")))
    (is (= {:base "never"} (parser/parse-type "never")))))

(deftest parse-literal-types-test
  (testing "Literal types can be parsed"
    (is (= {:literal "hello"} (parser/parse-type "\"hello\"")))
    (is (= {:literal "42"} (parser/parse-type "42")))
    (is (= {:base "true"} (parser/parse-type "true")))
    (is (= {:base "null"} (parser/parse-type "null")))
    (is (= {:base "undefined"} (parser/parse-type "undefined")))))

(deftest parse-array-types-test
  (testing "Array types can be parsed"
    (is (= {:array {:base "number"}} (parser/parse-type "number[]")))
    (is (= {:array {:base "string"}} (parser/parse-type "string[]")))))

(deftest parse-tuple-types-test
  (testing "Tuple types can be parsed"
    (is (= {:tuple [{:base "string"} {:base "number"}]} (parser/parse-type "[string, number]")))
    (is (= {:tuple [{:base "number"} {:base "number"} {:base "number"}]} (parser/parse-type "[number, number, number]")))))

(deftest parse-union-types-test
  (testing "Union types can be parsed"
    (is (= {:union [{:base "string"} {:base "number"}]} (parser/parse-type "string | number")))
    (is (= {:union [{:base "string"} {:base "number"} {:base "null"}]} (parser/parse-type "string | number | null")))))

(deftest parse-intersection-types-test
  (testing "Intersection types can be parsed"
    (is (= {:intersection [{:base "A"} {:base "B"}]} (parser/parse-type "A & B")))
    (is (= {:intersection [{:base "A"} {:base "B"} {:base "C"}]} (parser/parse-type "A & B & C")))))

(deftest parse-generic-types-test
  (testing "Generic types can be parsed"
    (is (= {:generic "Array", :args [{:base "number"}]} (parser/parse-type "Array<number>")))
    (is (= {:generic "Map", :args [{:base "string"} {:base "number"}]} (parser/parse-type "Map<string, number>")))))

(deftest parse-function-types-test
  (testing "Function types with no parameters can be parsed"
    (is (= {:arrow {:params [], :return {:base "void"}}} (parser/parse-type "() => void"))))

  (testing "Function types with parameters can be parsed"
    (is (= {:arrow
            {:params
             [{:name "x",
               :type {:base "number"},
               :optional false,
               :variadic false}],
             :return {:base "number"}}}
           (parser/parse-type "(x: number) => number")))
    (is (= {:arrow
            {:params
             [{:name "x",
               :type {:base "number"},
               :optional false,
               :variadic false}
              {:name "y",
               :type {:base "string"},
               :optional false,
               :variadic false}],
             :return {:base "boolean"}}}
           (parser/parse-type "(x: number, y: string) => boolean")))))

(deftest parse-optional-and-variadic-params-test
  (testing "Optional parameters can be parsed"
    (is (= {:arrow
            {:params
             [{:name "x",
               :type {:base "number"},
               :optional true,
               :variadic false}],
             :return {:base "void"}}} (parser/parse-type "(x?: number) => void"))))

  (testing "Variadic parameters can be parsed"
    (is (= {:arrow
            {:params
             [{:name "args",
               :type {:array {:base "any"}},
               :optional false,
               :variadic true}],
             :return {:base "void"}}}
           (parser/parse-type "(...args: any[]) => void")))))

(deftest parse-parenthesized-types-test
  (testing "Parenthesized types can be parsed"
    (is (= {:union [{:base "string"} {:base "number"}]}
           (parser/parse-type "(string | number)")))))

(deftest parse-typeof-test
  (testing "typeof queries can be parsed"
    (is (= {:typeof "foo"} (parser/parse-type "typeof foo")))))

;; ========== Declaration Parsing Tests ==========

(deftest parse-function-declaration-test
  (testing "Simple function declaration"
    (is (= {:functions
            [{:name "add",
              :params
              [{:name "x",
                :type {:base "number"},
                :optional false,
                :variadic false}
               {:name "y",
                :type {:base "number"},
                :optional false,
                :variadic false}],
              :return-type {:base "number"}}]}
           (parser/parse-declaration "function add(x: number, y: number): number;"))))

  (testing "Function with optional parameter"
    (is (= {:functions
            [{:name "greet",
              :params
              [{:name "name",
                :type {:base "string"},
                :optional true,
                :variadic false}],
              :return-type {:base "void"}}]}
           (parser/parse-declaration "function greet(name?: string): void;"))))

  (testing "Function with rest parameter"
    (is (= {:functions
            [{:name "sum",
              :params
              [{:name "numbers",
                :type {:array {:base "number"}},
                :optional false,
                :variadic true}],
              :return-type {:base "number"}}]}
           (parser/parse-declaration "function sum(...numbers: number[]): number;")))))

(deftest parse-type-alias-test
  (testing "Simple type alias"
    (is (= {:type-aliases
            [{:name "Point",
              :type
              {:object
               [{:kind :property,
                 :name "x",
                 :type {:base "number"},
                 :optional false,
                 :readonly false}
                {:kind :property,
                 :name "y",
                 :type {:base "number"},
                 :optional false,
                 :readonly false}]}}]}
           (parser/parse-declaration "type Point = { x: number; y: number; };"))))

  (testing "Type alias with union"
    (is (= {:type-aliases
            [{:name "StringOrNumber",
              :type {:union [{:base "string"} {:base "number"}]}}]}
           (parser/parse-declaration "type StringOrNumber = string | number;"))))

  (testing "Generic type alias"
    (is (= {:type-aliases
            [{:name "Box",
              :type
              {:object
               [{:kind :property,
                 :name "value",
                 :type {:base "T"},
                 :optional false,
                 :readonly false}]},
              :type-params [{:name "T"}]}]}
           (parser/parse-declaration "type Box<T> = { value: T; };")))))

(deftest parse-interface-test
  (testing "Simple interface"
    (is (= {:interfaces
            [{:name "User",
              :members
              [{:kind :property,
                :name "name",
                :type {:base "string"},
                :optional false,
                :readonly false}
               {:kind :property,
                :name "age",
                :type {:base "number"},
                :optional false,
                :readonly false}]}]}
           (parser/parse-declaration "interface User { name: string; age: number; }"))))

  (testing "Interface with optional property"
    (is (= {:interfaces
            [{:name "User",
              :members
              [{:kind :property,
                :name "name",
                :type {:base "string"},
                :optional false,
                :readonly false}
               {:kind :property,
                :name "email",
                :type {:base "string"},
                :optional true,
                :readonly false}]}]}
           (parser/parse-declaration "interface User { name: string; email?: string; }"))))

  (testing "Interface with method"
    (is (= {:interfaces
            [{:name "Greeter",
              :members
              [{:kind :method,
                :name "greet",
                :params
                [{:name "name",
                  :type {:base "string"},
                  :optional false,
                  :variadic false}],
                :return-type {:base "string"}}]}]}
           (parser/parse-declaration "interface Greeter { greet(name: string): string; }"))))

  (testing "Interface extending another"
    (is (= {:interfaces
            [{:name "Employee",
              :members
              [{:kind :property,
                :name "salary",
                :type {:base "number"},
                :optional false,
                :readonly false}],
              :extends [{:base "User"}]}]}
           (parser/parse-declaration "interface Employee extends User { salary: number; }")))))

(deftest parse-class-test
  (testing "Simple class"
    (is (= {:classes
            [{:name "Person",
              :members
              [{:kind :property,
                :name "name",
                :type {:base "string"},
                :optional false,
                :modifiers []}]}]}
           (parser/parse-declaration "class Person { name: string; }"))))

  (testing "Class with constructor"
    (is (= {:classes
            [{:name "Person",
              :members
              [{:kind :constructor,
                :params
                [{:name "name",
                  :type {:base "string"},
                  :optional false,
                  :variadic false}]}]}]}
           (parser/parse-declaration "class Person { constructor(name: string); }"))))

  (testing "Class with methods"
    (is (= {:classes
            [{:name "Calculator",
              :members
              [{:kind :method,
                :name "add",
                :params
                [{:name "x",
                  :type {:base "number"},
                  :optional false,
                  :variadic false}
                 {:name "y",
                  :type {:base "number"},
                  :optional false,
                  :variadic false}],
                :return-type {:base "number"},
                :modifiers []}]}]}
           (parser/parse-declaration "class Calculator { add(x: number, y: number): number; }")))))

(deftest parse-enum-test
  (testing "Simple enum"
    (is (= {:enums
            [{:name "Color",
              :members [{:name "Red"} {:name "Green"} {:name "Blue"}]}]}
           (parser/parse-declaration "enum Color { Red, Green, Blue }"))))

  (testing "Enum with values"
    (is (= {:enums
            [{:name "Status",
              :members
              [{:name "Active", :value "1"} {:name "Inactive", :value "0"}]}]}
           (parser/parse-declaration "enum Status { Active = 1, Inactive = 0 }")))))

(deftest parse-variable-declaration-test
  (testing "Const declaration"
    (is (= {:variables [{:name "PI", :kind "const", :type {:base "number"}}]}
           (parser/parse-declaration "const PI: number;"))))

  (testing "Let declaration"
    (is (= {:variables [{:name "count", :kind "let", :type {:base "number"}}]}
           (parser/parse-declaration "let count: number;")))))

;; ========== DefinitelyTyped Examples ==========

(deftest definitely-typed-setTimeout-test
  (testing "setTimeout from Node.js types (simplified)"
    (is (= {:functions
            [{:name "setTimeout",
              :params
              [{:name "callback",
                :type {:arrow {:params [], :return {:base "void"}}},
                :optional false,
                :variadic false}
               {:name "ms",
                :type {:base "number"},
                :optional false,
                :variadic false}],
              :return-type {:base "number"}}]}
           (parser/parse-declaration "function setTimeout(callback: () => void, ms: number): number;")))))

(deftest definitely-typed-promise-test
  (testing "Promise interface (simplified)"
    (is (= {:interfaces
            [{:name "Promise",
              :members
              [{:kind :method,
                :name "then",
                :params
                [{:name "onFulfilled",
                  :type
                  {:arrow
                   {:params
                    [{:name "value",
                      :type {:base "T"},
                      :optional false,
                      :variadic false}],
                    :return {:base "U"}}},
                  :optional false,
                  :variadic false}],
                :return-type {:generic "Promise", :args [{:base "U"}]},
                :type-params [{:name "U"}]}],
              :type-params [{:name "T"}]}]}
           (parser/parse-declaration "interface Promise<T> { then<U>(onFulfilled: (value: T) => U): Promise<U>; }")))))

(deftest definitely-typed-array-test
  (testing "Array interface methods (simplified)"
    (is (= {:interfaces
            [{:name "Array",
              :members
              [{:kind :method,
                :name "map",
                :params
                [{:name "callbackfn",
                  :type
                  {:arrow
                   {:params
                    [{:name "value",
                      :type {:base "T"},
                      :optional false,
                      :variadic false}],
                    :return {:base "U"}}},
                  :optional false,
                  :variadic false}],
                :return-type {:array {:base "U"}},
                :type-params [{:name "U"}]}],
              :type-params [{:name "T"}]}]}
           (parser/parse-declaration "interface Array<T> { map<U>(callbackfn: (value: T) => U): U[]; }")))))

(deftest multiple-declarations-test
  (testing "Multiple declarations in one input"
    (is (= {:functions [{:name "foo", :params [], :return-type {:base "void"}}],
            :type-aliases [{:name "Bar", :type {:base "string"}}],
            :interfaces
            [{:name "Baz",
              :members
              [{:kind :property,
                :name "x",
                :type {:base "number"},
                :optional false,
                :readonly false}]}]}
           (parser/parse-declaration "function foo(): void; type Bar = string; interface Baz { x: number; }")))))

(deftest generic-constraints-test
  (testing "Generic type parameters with constraints"
    (is (= {:type-aliases
            [{:name "Identity",
              :type {:base "T"},
              :type-params [{:name "T", :constraint {:base "string"}}]}]}
           (parser/parse-declaration "type Identity<T extends string> = T;")))))

(deftest object-type-literal-test
  (testing "Object type literals"
    (is (= (parser/parse-declaration "type Point = { x: number; y: number; };")
           {:type-aliases
            [{:name "Point",
              :type
              {:object
               [{:kind :property,
                 :name "x",
                 :type {:base "number"},
                 :optional false,
                 :readonly false}
                {:kind :property,
                 :name "y",
                 :type {:base "number"},
                 :optional false,
                 :readonly false}]}}]}))))

(deftest readonly-property-test
  (testing "Readonly properties in interfaces"
    (is (= {:interfaces
            [{:name "Config",
              :members
              [{:kind :property,
                :name "apiKey",
                :type {:base "string"},
                :optional false,
                :readonly true}]}]}
           (parser/parse-declaration "interface Config { readonly apiKey: string; }")))))

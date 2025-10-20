(ns typed-test.cljs.checker.ts-ast-to-cljs-types-test
  "Tests for TypeScript AST to Typed ClojureScript type conversion"
  (:require [clojure.test :refer [deftest is testing]]
            [typed.cljs.checker.ts-declaration-parser :as parser]
            [typed.cljs.checker.ts-ast-to-cljs-types :as ast->type]
            [typed.cljs.runtime.env :as env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]))

;; ========== Helper Functions ==========

(defn parse-ts-type
  "Helper function that takes a TypeScript type as a string and returns a Type object.
  Parses the TS type string and converts the AST to a Type."
  [ts-type-str]
  (let [opts (env/cljs-opts)]
    (-> ts-type-str
        parser/parse-type
        (ast->type/ast->Type opts))))

;; ========== Basic Type Conversion Tests ==========

(deftest keyword-types-test
  (testing "TypeScript keyword types map to Typed ClojureScript Type objects"
    ;; Test any -> (r/-unchecked nil)
    (let [any-type (parse-ts-type "any")]
      (is (r/Unchecked? any-type))
      (is (nil? (:vsym any-type))))
    
    ;; Test unknown -> r/-any
    (let [unknown-type (parse-ts-type "unknown")]
      (is (r/Top? unknown-type)))
    
    ;; Test void -> r/-nil
    (let [void-type (parse-ts-type "void")]
      (is (r/Value? void-type))
      (is (nil? (:val void-type))))
    
    ;; Test never -> r/-nothing
    (let [never-type (parse-ts-type "never")]
      (is (r/Bottom? never-type)))))

(deftest primitive-types-test
  (testing "TypeScript primitive types map to JS primitive Type objects"
    ;; Test string -> JSString
    (let [string-type (parse-ts-type "string")]
      (is (r/JSString? string-type)))
    
    ;; Test number -> JSNumber
    (let [number-type (parse-ts-type "number")]
      (is (r/JSNumber? number-type)))
    
    ;; Test boolean -> JSBoolean
    (let [boolean-type (parse-ts-type "boolean")]
      (is (r/JSBoolean? boolean-type)))
    
    ;; Test symbol -> js/Symbol Name
    (let [symbol-type (parse-ts-type "symbol")]
      (is (r/Name? symbol-type))
      (is (= 'js/Symbol (:id symbol-type))))))

(deftest null-undefined-test
  (testing "null and undefined map to distinct JS types"
    ;; Test null -> JSNull
    (let [null-type (parse-ts-type "null")]
      (is (r/JSNull? null-type)))
    
    ;; Test undefined -> JSUndefined
    (let [undefined-type (parse-ts-type "undefined")]
      (is (r/JSUndefined? undefined-type)))))

(deftest literal-types-test
  (testing "Literal types map to Value types"
    ;; String literals
    (let [str-lit-type (parse-ts-type "\"hello\"")]
      (is (r/Value? str-lit-type))
      (is (= "hello" (:val str-lit-type))))
    
    ;; Numeric literals  
    (let [num-lit-type (parse-ts-type "42")]
      (is (r/Value? num-lit-type))
      (is (= 42 (:val num-lit-type))))
    
    ;; Boolean literals (true/false are built-in base types)
    (let [true-type (parse-ts-type "true")]
      (is (r/Value? true-type))
      (is (= true (:val true-type))))
    
    (let [false-type (parse-ts-type "false")]
      (is (r/Value? false-type))
      (is (= false (:val false-type))))))

(deftest union-types-test
  (testing "Union types map to Union Type objects"
    ;; Simple union: string | number
    (let [union-type (parse-ts-type "string | number")]
      (is (r/Union? union-type))
      (is (= 2 (count (:types union-type))))
      ;; Check that the union contains JSString and JSNumber
      (let [types-set (:types union-type)]
        (is (some r/JSString? types-set))
        (is (some r/JSNumber? types-set))))
    
    ;; Union with null: string | null
    (let [nullable-union (parse-ts-type "string | null")]
      (is (r/Union? nullable-union))
      (is (= 2 (count (:types nullable-union))))
      (let [types-set (:types nullable-union)]
        (is (some r/JSString? types-set))
        (is (some r/JSNull? types-set))))))

(deftest array-types-test
  (testing "Array types map to ArrayCLJS (JavaScript arrays)"
    ;; Test number[]
    (let [array-type (parse-ts-type "number[]")]
      (is (r/ArrayCLJS? array-type)))
    
    ;; Test Array<string> (generic syntax)
    (let [generic-array-type (parse-ts-type "Array<string>")]
      (is (r/ArrayCLJS? generic-array-type)))))

(deftest function-types-test
  (testing "Function types map to FnIntersection Type objects"
    ;; Simple function: () => void
    (let [fn-type (parse-ts-type "() => void")]
      (is (r/FnIntersection? fn-type))
      (is (= 1 (count (:types fn-type))))
      (let [arity (first (:types fn-type))]
        (is (r/Function? arity))
        (is (= 0 (count (:dom arity))))
        (is (r/Value? (-> arity :rng :t)))
        (is (nil? (-> arity :rng :t :val)))))
    
    ;; Function with parameters: (x: number) => string
    (let [fn-type (parse-ts-type "(x: number) => string")]
      (is (r/FnIntersection? fn-type))
      (is (= 1 (count (:types fn-type))))
      (let [arity (first (:types fn-type))]
        (is (r/Function? arity))
        (is (= 1 (count (:dom arity))))
        (is (r/JSNumber? (first (:dom arity))))
        (is (r/JSString? (-> arity :rng :t)))))))

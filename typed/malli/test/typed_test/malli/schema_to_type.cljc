(ns ^:typed.clojure typed-test.malli.schema-to-type
  (:require [clojure.test :refer [deftest is]]
            [malli.util :as mu]
            [malli.error :as me]
            [typed.clojure :as t]
            [typed.malli :as tm]
            [typed.malli.parse-type :as t->s]
            [typed.malli.schema-to-type :as sut]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [clojure.core.typed.type-contract :as tcon]
            [typed.clj.checker.test-utils :refer [clj is-clj subtype? both-subtype? tc-e tc-err is-tc-e is-tc-err]]
            [malli.core :as m]))

(defmacro self-validate-parser-type [res t v]
  (let [m (t->s/type-syntax->malli-syntax t)
        tout (sut/malli-syntax->parser-type (m/schema m))
        mout (m/form (t->s/type-syntax->malli-syntax tout))]
    `(let [v# ~v]
       (is (= ~res
              (= v#
                 (m/unparse
                   ~mout
                   (tm/parse ~t v#))))))))

(defmacro self-validate-validator-type [res t v]
  (let [m (t->s/type-syntax->malli-syntax t)
        tout (sut/malli-syntax->validator-type (m/schema m))
        mout (m/form (t->s/type-syntax->malli-syntax tout))]
    `(let [v# ~v]
       (is (= ~res
              (= v#
                 (m/unparse
                   ~mout
                   (tm/parse ~t v#))))))))

(deftest malli-syntax->parser-type
  (is (= `(t/U (t/Val ::m/invalid)
               t/AnyInteger)
         (sut/malli-syntax->parser-type
           :int)))
  (is (= `(t/U (t/Val ::m/invalid)
               (t/Val :kw))
         (sut/malli-syntax->parser-type
           [:= :kw])))
  (is (= `(t/U (t/Val ::m/invalid)
               (t/Vec t/AnyInteger))
         (sut/malli-syntax->parser-type
           [:* :int])))
  (is (= `(t/U (t/Val ::m/invalid)
               '{:int t/AnyInteger})
         (sut/malli-syntax->parser-type
           [:catn [:int :int]])))
  #_
  (is-clj (sub/subtype?
            (prs/parse-clj
              (sut/malli-syntax->parser-type
                `Hiccup))
            (prs/parse-clj
              `(t/U '::m/invalid
                    '[':node '{:name t/Keyword
                               :props (t/Nilable (t/Map t/Keyword t/Any))
                               :children (t/Vec (t/Rec [hiccup]
                                                       (t/U '[':node '{:name ':div
                                                                       :props (t/Nilable (t/Map t/Keyword t/Any))
                                                                       :children (t/Vec hiccup)}]
                                                            '[':primitive (t/U '[':nil nil]
                                                                               '[':boolean t/Bool]
                                                                               '[':number Number]
                                                                               '[':text t/Str])])))}]
                    '[':primitive (t/U '[':nil nil]
                                       '[':boolean t/Bool]
                                       '[':number Number]
                                       '[':text t/Str])]))
            ))
  (self-validate-parser-type true t/Any 1)
  (self-validate-parser-type true t/Str "a")
  (self-validate-parser-type false t/Str nil)
  (self-validate-parser-type false (t/Val nil) 1)
  (self-validate-parser-type true (t/Val nil) nil)
  (self-validate-parser-type true (t/Val nil) nil)

  ;; Test new schemas for parser-type mode
  ;; :tuple - should return tuple with ::m/invalid union
  (is (= `(t/U (t/Val ::m/invalid)
               '[t/AnyInteger t/Str])
         (sut/malli-syntax->parser-type [:tuple :int :string])))
  
  ;; :double and :float - should return platform-specific types
  #?(:clj (is (= `(t/U (t/Val ::m/invalid) Double)
                 (sut/malli-syntax->parser-type :double))))
  #?(:cljs (is (= `(t/U (t/Val ::m/invalid) t/Num)
                  (sut/malli-syntax->parser-type :double))))
  
  #?(:clj (is (= `(t/U (t/Val ::m/invalid) (t/U Double Float))
                 (sut/malli-syntax->parser-type :float))))
  #?(:cljs (is (= `(t/U (t/Val ::m/invalid) t/Num)
                  (sut/malli-syntax->parser-type :float))))
  
  ;; :not - should return negation type
  (is (= `(t/U (t/Val ::m/invalid)
               (t/Not t/AnyInteger))
         (sut/malli-syntax->parser-type [:not :int])))
  
  ;; :+ - should return vector (same as :*)
  (is (= `(t/U (t/Val ::m/invalid)
               (t/Vec t/AnyInteger))
         (sut/malli-syntax->parser-type [:+ :int])))
  
  ;; :cat - should return quoted vector
  (is (= `(t/U (t/Val ::m/invalid)
               '[t/AnyInteger t/Str])
         (sut/malli-syntax->parser-type [:cat :int :string])))
  
  ;; :seqable and :every - should return seqable
  (is (= `(t/U (t/Val ::m/invalid)
               (t/Seqable t/AnyInteger))
         (sut/malli-syntax->parser-type [:seqable :int])))
  
  (is (= `(t/U (t/Val ::m/invalid)
               (t/Seqable t/AnyInteger))
         (sut/malli-syntax->parser-type [:every :int]))))

(deftest malli-syntax->validator-type
  (is (= `t/AnyInteger
         (sut/malli-syntax->validator-type
           :int)))
  (is (= `(t/Val :kw)
         (sut/malli-syntax->validator-type
           [:= :kw])))
  (is (= `(t/Not (t/Val :kw))
         (sut/malli-syntax->validator-type
           [:not= :kw])))

  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type
           [:* :int])))
  (is (m/validate [:* :int] [1]))
  (is (m/explain [:* :int] 1))
  (is (m/explain [:* :int] [[1]]))

  #_ ;;FIXME
  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type
           [:* [:* :int]])))
  (is (m/validate [:* [:* :int]] [1]))
  (is (m/explain [:* [:* :int]] 1))
  (is (m/explain [:* [:* :int]] [[1]]))

  (is (= `(t/Seqable (t/Seqable t/AnyInteger))
         (sut/malli-syntax->validator-type
           [:* [:schema [:* :int]]])))
  (is (m/validate [:* [:schema [:* :int]]] [[1]]))
  (is (m/explain [:* [:schema [:* :int]]] 1))
  (is (m/explain [:* [:schema [:* :int]]] [1]))

  #_ ;;FIXME
  (is (= `(t/Vec (t/Seqable t/AnyInteger))
         (sut/malli-syntax->validator-type
           [:* [:schema [:* :int]]])))
  (is (m/validate [:vector [:* :int]] [[1 2 3] [1 2 3]]))
  (is (m/explain [:vector [:* :int]] [1 2 3]))

  (is (= `'[t/AnyInteger]
         (sut/malli-syntax->validator-type
           [:catn [:int :int]])))
  (is (= `[t/AnyInteger :-> t/AnyInteger]
         (sut/malli-syntax->validator-type
           [:=> [:cat :int] :int])))
  (is (= `[t/AnyInteger t/AnyInteger :-> t/AnyInteger]
         (sut/malli-syntax->validator-type
           [:=> [:cat :int :int] :int])))
  (is (= `(t/IFn [t/AnyInteger :-> t/AnyInteger]
                 [t/AnyInteger t/AnyInteger :-> t/AnyInteger])
         (sut/malli-syntax->validator-type
           [:=> [:cat :int [:? :int]] :int])))
  (is (= `[t/AnyInteger t/AnyInteger :* :-> t/AnyInteger]
         (sut/malli-syntax->validator-type
           [:=> [:cat :int [:* :int]] :int])))
  (is (= `[t/AnyInteger :-> t/AnyInteger]
         (sut/malli-syntax->validator-type
           [:=> [:catn [:foo :int]] :int])))
  (is (= `t/AnyInteger
         (sut/malli-syntax->validator-type
           'integer?)))
  (is (= `(t/Val nil)
         (sut/malli-syntax->validator-type 'nil?)
         (sut/malli-syntax->validator-type :nil)))
  (is (= `(t/HMap)
         (sut/malli-syntax->validator-type [:map])))
  (is (= `'{:a t/AnyInteger}
         (sut/malli-syntax->validator-type [:map [:a :int]])))
  (is (= `(t/HMap :optional {:a t/AnyInteger})
         (sut/malli-syntax->validator-type [:map [:a {:optional true} :int]])))
  (is (= `(t/HMap :mandatory {:b t/Bool}
                  :optional {:a t/AnyInteger})
         (sut/malli-syntax->validator-type [:map [:a {:optional true} :int]
                                            [:b :boolean]])))
  (is (thrown? clojure.lang.ExceptionInfo (m/schema [:and])))
  (is (thrown? clojure.lang.ExceptionInfo (m/schema [:or])))
  (is (= `t/AnyInteger
         (sut/malli-syntax->validator-type
           [:and
            {:title "Age"
             :description "It's an age"
             :json-schema/example 20}
            :int [:> 18]])))
  ;; FIXME refine
  (is (= `(t/U t/Num
               t/Num
               t/Num)
         (sut/malli-syntax->validator-type
           '[:or int? pos-int? neg-int?])))
  (is (thrown? clojure.lang.ExceptionInfo
               (sut/malli-syntax->validator-type '[:function])))
  (is (= `[t/Num :-> t/Num]
         (sut/malli-syntax->validator-type
           '[:function
             [:=> [:cat number?] number?]])))
  (is (= `(t/IFn [t/Num :-> t/Num]
                 [t/Num t/Num :-> t/Num])
         (sut/malli-syntax->validator-type
           '[:function
             [:=> [:cat number?] number?]
             [:=> [:cat number? number?] number?]])))
  (is (= `(t/IFn [:-> t/Num]
                 [t/Num :-> t/Num]
                 [t/Num t/Num :-> t/Num])
         (sut/malli-syntax->validator-type
           '[:function
             [:=> [:cat [:? number?]] number?]
             [:=> [:cat number? number?] number?]])))
  (is (= `(t/Vec t/Num)
         (sut/malli-syntax->validator-type
           '[:vector number?])))
  (is (= `(t/Set t/Num)
         (sut/malli-syntax->validator-type
           '[:set number?])))
  (is (= `(t/SequentialColl t/Num)
         (sut/malli-syntax->validator-type
           '[:sequential number?])))
  (is (= `(t/Nilable t/Num)
         (sut/malli-syntax->validator-type
           '[:maybe number?])))
  (is (= `(t/Nilable t/Num)
         (sut/malli-syntax->validator-type
           '[:maybe number?])))
  (is (= `t/Str
         (sut/malli-syntax->validator-type '[:re "foo"])
         (sut/malli-syntax->validator-type '[:re #"foo"])))
  (is (= `t/UUID
         (sut/malli-syntax->validator-type :uuid)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/malli-syntax->validator-type :simple-keyword)))
  (is (= `t/Kw
         (sut/malli-syntax->validator-type :keyword)
         (sut/malli-syntax->validator-type 'keyword?)
         (sut/malli-syntax->validator-type 'simple-keyword?)
         (sut/malli-syntax->validator-type :qualified-keyword)
         (sut/malli-syntax->validator-type 'qualified-keyword?)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/malli-syntax->validator-type :simple-symbol)))
  (is (= `t/Sym
         (sut/malli-syntax->validator-type :symbol)
         (sut/malli-syntax->validator-type 'symbol?)
         (sut/malli-syntax->validator-type 'simple-symbol?)
         (sut/malli-syntax->validator-type :qualified-symbol)
         (sut/malli-syntax->validator-type 'qualified-symbol?)))

  (is (= `t/Str (sut/malli-syntax->validator-type '[:schema {:registry {'a :string}}
                                                    'a])))
  (is (= `t/Str (sut/malli-syntax->validator-type '[:schema {:registry {::a :string}}
                                                    ::a])))
  (is (= `t/Str (sut/malli-syntax->validator-type '[:schema {:registry {"a" :string}}
                                                    "a"])))
  (is (some? (prs/parse-clj
               (sut/malli-syntax->validator-type '[:schema {:registry {"a space" :string}}
                                                   "a space"]))))
  (is (some? (prs/parse-clj
               (sut/malli-syntax->validator-type '[:schema {:registry {"b space" [:vector [:ref "a space"]]
                                                                       "a space" [:vector [:ref "b space"]]}}
                                                   "a space"]))))
  (is (some? (prs/parse-clj
               (sut/malli-syntax->validator-type '[:schema {:registry {"2" [:vector [:ref "1"]]
                                                                       "1" [:vector [:ref "2"]]}}
                                                   "1"]))))
  (is (= `t/Any
         (sut/malli-syntax->validator-type '[:fn #(identity %)])))
  ;;TODO this tests the ::default of -malli->type -- if/when we support :fn, find a better way to test.
  (is (= `t/Any
         (sut/malli-syntax->validator-type 'ifn?)))

  (self-validate-validator-type true t/Any 1)
  (self-validate-validator-type true t/Str "a")
  (self-validate-validator-type false t/Str nil)
  (self-validate-validator-type false (t/Val nil) 1)
  (self-validate-validator-type true (t/Val nil) nil)
  (self-validate-validator-type true (t/Val nil) nil)

  ;; Test :tuple - heterogeneous vector type
  (is (= `'[t/AnyInteger t/Str]
         (sut/malli-syntax->validator-type [:tuple :int :string])))
  (is (= `'[t/AnyInteger t/Str t/Bool]
         (sut/malli-syntax->validator-type [:tuple :int :string :boolean])))
  (is (m/validate [:tuple :int :string] [1 "a"]))
  (is (m/explain [:tuple :int :string] [1]))
  (is (m/explain [:tuple :int :string] [1 "a" :extra]))

  ;; Test :double - double precision floating point
  #?(:clj (is (= `Double (sut/malli-syntax->validator-type :double))))
  #?(:cljs (is (= `t/Num (sut/malli-syntax->validator-type :double))))
  (is (m/validate :double 1.5))
  (is (m/validate :double (double 1.5)))
  (is (m/explain :double "not-a-double"))

  ;; Test :float - single precision floating point  
  #?(:clj (is (= `(t/U Double Float) (sut/malli-syntax->validator-type :float))))
  #?(:cljs (is (= `t/Num (sut/malli-syntax->validator-type :float))))
  (is (m/validate :float 1.5))
  (is (m/validate :float (float 1.5)))
  (is (m/explain :float "not-a-float"))

  ;; Test :not - negation type
  (is (= `(t/Not t/AnyInteger)
         (sut/malli-syntax->validator-type [:not :int])))
  (is (= `(t/Not t/Str)
         (sut/malli-syntax->validator-type [:not :string])))
  (is (m/validate [:not :int] "string"))
  (is (m/explain [:not :int] 42))

  ;; Test :+ - one or more (like :* but at least one element)
  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type [:+ :int])))
  (is (m/validate [:+ :int] [1]))
  (is (m/validate [:+ :int] [1 2 3]))
  (is (m/explain [:+ :int] []))

  ;; Test :alt - alternation in regex (similar to :or but for sequences)
  (is (= `(t/U t/AnyInteger t/Str)
         (sut/malli-syntax->validator-type [:alt :int :string])))
  (is (m/validate [:cat [:alt :int :string]] [1]))
  (is (m/validate [:cat [:alt :int :string]] ["a"]))

  ;; Test :altn - named alternation (like :alt but with names)
  (is (= `(t/U t/AnyInteger t/Str)
         (sut/malli-syntax->validator-type [:altn [:int :int] [:str :string]])))
  (is (m/validate [:cat [:altn [:int :int] [:str :string]]] [1]))
  (is (m/validate [:cat [:altn [:int :int] [:str :string]]] ["a"]))

  ;; Test :repeat - repeat with min/max constraints
  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type [:repeat {:min 1 :max 3} :int])))
  (is (m/validate [:repeat {:min 1 :max 3} :int] [1]))
  (is (m/validate [:repeat {:min 1 :max 3} :int] [1 2 3]))
  (is (m/explain [:repeat {:min 1 :max 3} :int] []))
  (is (m/explain [:repeat {:min 1 :max 3} :int] [1 2 3 4]))

  ;; Test :seqable - seqable collection type (like :sequential but validates with seqable?)
  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type [:seqable :int])))
  (is (m/validate [:seqable :int] [1 2 3]))
  (is (m/validate [:seqable :int] '(1 2 3)))
  (is (m/explain [:seqable :int] 42))

  ;; Test :every - like :vector but validates all seqables
  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type [:every :int])))
  (is (m/validate [:every :int] [1 2 3]))
  (is (m/validate [:every :int] '(1 2 3)))
  (is (m/explain [:every :int] [1 "not-int"]))

  ;; Test :multi - multimethod/dispatch type
  ;; Now converts to union of branch types for validator-type mode
  (is (= `(t/U '{:type (t/Val :int) :value t/AnyInteger}
               '{:type (t/Val :str) :value t/Str})
         (sut/malli-syntax->validator-type [:multi {:dispatch :type}
                                            [:int [:map [:type [:= :int]] [:value :int]]]
                                            [:str [:map [:type [:= :str]] [:value :string]]]])))
  (is (m/validate [:multi {:dispatch :type}
                   [:int [:map [:type [:= :int]] [:value :int]]]
                   [:str [:map [:type [:= :str]] [:value :string]]]]
                  {:type :int :value 42}))

  ;; Test extensive regex operator semantics with validation and parsing
  ;; Test nested regex contexts
  
  ;; Top-level regex in :cat
  (is (= `'[t/AnyInteger] 
         (sut/malli-syntax->validator-type [:cat :int])))
  (is (m/validate [:cat :int] [42]))
  (is (m/explain [:cat :int] [42 "extra"]))
  
  ;; Nested :* in :cat - :* matches zero or more elements in the sequence
  (is (= `'[(t/Seqable t/AnyInteger)]
         (sut/malli-syntax->validator-type [:cat [:* :int]])))
  (is (m/validate [:cat [:* :int]] [1 2 3]))  ;; :* consumes all ints in the sequence
  (is (m/validate [:cat [:* :int]] []))       ;; :* can match zero elements
  
  ;; Nested :+ in :cat - :+ matches one or more elements
  (is (= `'[(t/Seqable t/AnyInteger)]
         (sut/malli-syntax->validator-type [:cat [:+ :int]])))
  (is (m/validate [:cat [:+ :int]] [1 2 3]))  ;; :+ matches one or more ints
  (is (m/explain [:cat [:+ :int]] []))        ;; :+ requires at least one element
  
  ;; Nested :alt in :cat - :alt matches one element that satisfies any alternative
  (is (= `'[(t/U t/AnyInteger t/Str)]
         (sut/malli-syntax->validator-type [:cat [:alt :int :string]])))
  (is (m/validate [:cat [:alt :int :string]] [42]))       ;; matches int alternative
  (is (m/validate [:cat [:alt :int :string]] ["hello"]))  ;; matches string alternative
  
  ;; Complex nested regex: cat with alt containing repeats
  ;; This is invalid - :alt with :* inside :cat doesn't work as expected
  ;; Let's use a simpler valid example
  (is (= `'[t/AnyInteger t/Str]
         (sut/malli-syntax->validator-type [:cat :int :string])))
  (is (m/validate [:cat :int :string] [42 "hello"]))
  (is (m/explain [:cat :int :string] [42]))  ;; missing string

  ;; Test some additional edge cases
  ;; Nested vectors
  (is (= `(t/Vec (t/Vec t/AnyInteger))
         (sut/malli-syntax->validator-type [:vector [:vector :int]])))
  
  ;; Map with enum
  (is (= `'{:type (t/U (t/Val :a) (t/Val :b) (t/Val :c))}
         (sut/malli-syntax->validator-type [:map [:type [:enum :a :b :c]]])))
  
  ;; Union with maybe
  (is (= `(t/U (t/Nilable t/AnyInteger) t/Str)
         (sut/malli-syntax->validator-type [:or [:maybe :int] :string])))
  
  ;; Tuple with different types
  (is (= `'[t/Kw t/Str t/AnyInteger]
         (sut/malli-syntax->validator-type [:tuple :keyword :string :int])))
  
  ;; Set of tuples
  (is (= `(t/Set '[t/AnyInteger t/Str])
         (sut/malli-syntax->validator-type [:set [:tuple :int :string]])))

  ;; Numeric comparison operators
  (is (= `t/Num (sut/malli-syntax->validator-type [:> 5])))
  (is (= `t/Num (sut/malli-syntax->validator-type [:< 10])))
  (is (= `t/Num (sut/malli-syntax->validator-type [:>= 0])))
  (is (= `t/Num (sut/malli-syntax->validator-type [:<= 100])))

  ;; Additional predicates
  (is (= `(t/List t/Any) (sut/malli-syntax->validator-type 'list?)))
  (is (= `(t/Coll t/Any) (sut/malli-syntax->validator-type 'coll?)))
  (is (= `(t/Indexed t/Any) (sut/malli-syntax->validator-type 'indexed?)))
  (is (= `(t/Associative t/Any t/Any) (sut/malli-syntax->validator-type 'associative?)))
  (is (= `(t/Val true) (sut/malli-syntax->validator-type 'true?)))
  (is (= `(t/Val false) (sut/malli-syntax->validator-type 'false?)))
  (is (= `t/EmptySeqable (sut/malli-syntax->validator-type 'empty?)))
  (is (= `(t/U t/Kw t/Sym) (sut/malli-syntax->validator-type 'ident?)))
  (is (= `(t/U t/Kw t/Sym) (sut/malli-syntax->validator-type 'simple-ident?)))
  (is (= `(t/U t/Kw t/Sym) (sut/malli-syntax->validator-type 'qualified-ident?)))
  ;; Platform-specific tests 
  ;; bytes? - only available on JVM  
  #?(:clj (is (= '(Array byte) (sut/malli-syntax->validator-type 'bytes?))))
  ;; char? - different on JVM vs CLJS
  #?(:clj (is (= 'java.lang.Character (sut/malli-syntax->validator-type 'char?))))
  #?(:cljs (is (= 'typed.clojure/Str (sut/malli-syntax->validator-type 'char?))))
  #?(:clj (is (= `(t/U t/Int clojure.lang.Ratio BigDecimal) (sut/malli-syntax->validator-type 'ratio?))))
  #?(:cljs (is (= `t/Num (sut/malli-syntax->validator-type 'ratio?))))
  #?(:clj (is (= `(t/U t/Int clojure.lang.Ratio BigDecimal) (sut/malli-syntax->validator-type 'rational?))))
  #?(:cljs (is (= `t/Num (sut/malli-syntax->validator-type 'rational?))))
  #?(:clj (is (= `BigDecimal (sut/malli-syntax->validator-type 'decimal?))))
  #?(:cljs (is (= `t/Num (sut/malli-syntax->validator-type 'decimal?))))
  
  ;; Test new specific numeric predicates
  #?(:clj (is (= `(t/U Long Integer Short Byte) (sut/malli-syntax->validator-type 'nat-int?))))
  #?(:cljs (is (= `(t/U t/CLJSInteger goog.math.Integer goog.math.Long) (sut/malli-syntax->validator-type 'nat-int?))))
  (is (= `(t/Val 0) (sut/malli-syntax->validator-type 'zero?)))
  
  ;; Test seqable? predicate
  (is (= `t/AnySeqable (sut/malli-syntax->validator-type 'seqable?))))

(def Over
  (m/-simple-schema
    {:compile
     (fn [{:keys [value]} _ _]
       (assert (int? value))
       {:type ::over
        :pred #(and (int? %) (> % value))
        :type-properties {:error/fn (fn [error _] (str "should be over " value ", was " (:value error)))
                          ;:decode/string mt/-string->long
                          :json-schema/type "integer"
                          :json-schema/format "int64"
                          :json-schema/minimum value
                          ;:gen/gen (gen/large-integer* {:min (inc value)})
                          }})}))

(sut/register-malli->type-extension ::over [m _] `t/Int)

(deftest extensible-schema-test
  (is (= ["should be over 12, was 10"]
         (-> [Over {:value 12}]
             (m/explain 10)
             (me/humanize))))
  (is (= `t/Int
         (sut/malli-syntax->validator-type
           `[Over {:value 12}]))))

;; s <: t <==> (m/validate s v) implies (m/validate t v).
(defn malli-validator-sub? [s t]
  (subtype?
    (prs/parse-clj (sut/malli-syntax->validator-type (m/schema s {:registry (merge (m/default-schemas) (mu/schemas))})))
    (prs/parse-clj (sut/malli-syntax->validator-type (m/schema t {:registry (merge (m/default-schemas) (mu/schemas))})))))

(deftest validator-subtype-test
  (is (malli-validator-sub? :int :any))
  (is (not (malli-validator-sub? :any :int)))
  (is (malli-validator-sub? :int number?))
  (is (not (malli-validator-sub? number? :int)))
  (is (not (malli-validator-sub? vector? :int)))
  (is (not (malli-validator-sub? :int vector?)))
  (is (not (malli-validator-sub? [:= 1] [:= 2])))
  (is (malli-validator-sub? [:= 1] [:enum 1 2]))
  (is (not (malli-validator-sub? [:enum 1 2] [:= 1])))
  (is (malli-validator-sub? [:map-of :int :int] [:map-of :any :any]))
  (is (malli-validator-sub? [:map-of :int :int] [:map-of number? number?]))
  (is (malli-validator-sub? :map :map))
  (is (malli-validator-sub? [:map [:a :int]] :map))
  (is (not (malli-validator-sub? :map [:map [:a :int]])))
  (is (malli-validator-sub? [:map [:a :int]] [:map [:a number?]]))
  (is (malli-validator-sub? [:map [:a :int]] [:map [:a number?]]))
  (is (malli-validator-sub? [:merge [:map [:a :any]] [:map [:a :int]]] [:map [:a number?]]))
  (is (not (malli-validator-sub? [:merge [:map [:a :int]] [:map [:a :any]]] [:map [:a number?]]))))

(ns typed-test.malli.schema-to-type
  (:require [clojure.test :refer [deftest is]]
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
  (self-validate-parser-type true (t/Val nil) nil))

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
  (is (= `(t/I t/AnyInteger t/Num)
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
  (is (= `(t/Maybe t/Num)
         (sut/malli-syntax->validator-type
           '[:maybe number?])))
  (is (= `(t/Maybe t/Num)
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
  (self-validate-validator-type true (t/Val nil) nil))

(def Over
  (m/-simple-schema
    (fn [{:keys [value]} _]
      (assert (int? value))
      {:type ::over
       :pred #(and (int? %) (> % value))
       :type-properties {:error/fn (fn [error _] (str "should be over " value ", was " (:value error)))
                         ;:decode/string mt/-string->long
                         :json-schema/type "integer"
                         :json-schema/format "int64"
                         :json-schema/minimum value
                         ;:gen/gen (gen/large-integer* {:min (inc value)})
                         }})))

(sut/register-malli->type-extension ::over [m _] `t/Int)

(deftest extensible-schema-test
  (is (= ["should be over 12, was 10"]
         (-> [Over {:value 12}]
             (m/explain 10)
             (me/humanize))))
  (is (= `t/Int
         (sut/malli-syntax->validator-type
           `[Over {:value 12}]))))

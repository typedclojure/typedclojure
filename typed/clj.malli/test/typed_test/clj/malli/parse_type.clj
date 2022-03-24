(ns typed-test.clj.malli.parse-type
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [typed.clj.malli :as tm]
            [typed.clj.malli.parse-type :as sut]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [clojure.core.typed.type-contract :as tcon]
            [typed.clj.checker.test-utils :refer [clj is-clj subtype? both-subtype? tc-e tc-err is-tc-e is-tc-err]]
            [malli.core :as m]))

(deftest type-syntax->malli-syntax-test
  (is (= (sut/type-syntax->malli-syntax
           `t/Any)
         :any))
  (is (= (sut/type-syntax->malli-syntax
           `t/AnyInteger)
         :int))
  (is (= (sut/type-syntax->malli-syntax
           nil)
         :nil))
  (is (= (sut/type-syntax->malli-syntax
           `Number)
         `number?))
  (is (= (sut/type-syntax->malli-syntax
           `String)
         :string))
  (is (= (sut/type-syntax->malli-syntax
           `Boolean)
         `boolean?))
  (is (= (sut/type-syntax->malli-syntax
           `clojure.lang.IPersistentMap)
         `map?))
  (is (= (sut/type-syntax->malli-syntax
           `(clojure.lang.IPersistentMap
              String
              Boolean))
         [:map :string `boolean?]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/Map String Boolean))
         [:map :string `boolean?]))
  (is (= (sut/type-syntax->malli-syntax
           `clojure.lang.IPersistentVector)
         `vector?))
  (is (= (sut/type-syntax->malli-syntax
           `(t/Vec String))
         [:vector :string]))
  (is (= (sut/type-syntax->malli-syntax
           `'{})
         [:map]))
  (is (= (sut/type-syntax->malli-syntax
           `'{:a t/AnyInteger :b Boolean})
         [:map [:a :int] [:b `boolean?]]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/HMap :mandatory {:a t/AnyInteger :b Boolean}))
         [:map [:a :int] [:b `boolean?]]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/HMap :mandatory {:a t/AnyInteger :b Boolean}
                    :optional {:c String}))
         [:map [:a :int] [:b `boolean?] [:c {:optional true} :string]]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/HMap :mandatory {:a t/AnyInteger :b Boolean}
                    :optional {:c String}
                    :complete? true))
         [:map {:closed true} [:a :int] [:b `boolean?] [:c {:optional true} :string]]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/HMap :mandatory {:a t/AnyInteger :b Boolean}
                    :optional {:c String}
                    :absent-keys #{:e}))
         [:map {:closed true} [:a :int] [:b `boolean?] [:c {:optional true} :string]]))
  (is (= (sut/type-syntax->malli-syntax
           `'[String])
         [:tuple :string]))
  (is (= (sut/type-syntax->malli-syntax
           `'[String '[t/AnyInteger]])
         [:tuple :string [:tuple :int]]))
  (is (= (sut/type-syntax->malli-syntax
           `'[String t/Any :*])
         [:cat :string [:* :any]]))
  (is (= (sut/type-syntax->malli-syntax
           `'[String t/Any ~'*])
         [:cat :string [:* :any]]))
  (is (= (sut/type-syntax->malli-syntax
           `'[String t/Any :*])
         [:cat :string [:* :any]]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/U String Boolean))
         [:or :string `boolean?]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/U ^{::tm/name :string} String
                 ^{::tm/name :bool} Boolean))
         [:orn [:string :string] [:bool `boolean?]]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/U ^{::tm/name :int} t/AnyInteger
                 ^{::tm/name :bool} t/Bool))
         [:orn [:int :int] [:bool `boolean?]]))
  (is (= (sut/type-syntax->malli-syntax
           `':a)
         [:= :a]))
  (is (= (sut/type-syntax->malli-syntax
           `(t/U '{:a ':foo} '{:a ':bar}))
         [:multi {:dispatch :a}
          [:foo [:map [:a [:= :foo]]]]
          [:bar [:map [:a [:= :bar]]]]])))

(defn self-validate-parser-type* [res t v]
  (let [m (sut/type-syntax->malli-syntax t)
        tout (sut/malli-syntax->parser-type (m/schema m))
        mout (m/form (sut/type-syntax->malli-syntax tout))]
    (is (= res
           (m/validate
             mout
             (m/parse m v))))))

(defmacro self-validate-parser-type [res t v]
  (let [m (sut/type-syntax->malli-syntax t)
        tout (sut/malli-syntax->parser-type (m/schema m))
        mout (m/form (sut/type-syntax->malli-syntax tout))]
    `(let [v# ~v]
       (is (= ~res
              (= v#
                 (m/unparse
                   ~mout
                   (tm/parse ~t v#))))))))

(defmacro self-validate-validator-type [res t v]
  (let [m (sut/type-syntax->malli-syntax t)
        tout (sut/malli-syntax->validator-type (m/schema m))
        mout (m/form (sut/type-syntax->malli-syntax tout))]
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
  (is (= `(t/Seqable t/AnyInteger)
         (sut/malli-syntax->validator-type
           [:* :int])))
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
  (self-validate-validator-type true t/Any 1)
  (self-validate-validator-type true t/Str "a")
  (self-validate-validator-type false t/Str nil)
  (self-validate-validator-type false (t/Val nil) 1)
  (self-validate-validator-type true (t/Val nil) nil)
  (self-validate-validator-type true (t/Val nil) nil))

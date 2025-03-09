(ns ^:typed.clojure typed-test.malli.parse-type
  (:require [clojure.test :refer [deftest is]]
            [malli.error :as me]
            [typed.clojure :as t]
            [typed.malli :as tm]
            [typed.malli.parse-type :as sut]
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

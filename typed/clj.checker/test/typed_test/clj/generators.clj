(ns ^:typed.clojure typed-test.clj.generators
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [typed.clj.generators :as sut]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]])
  (:import [clojure.lang ExceptionInfo]))

(deftest generator-test
  (checking
    "Any"
    [x (sut/generator `t/Any)]
    (is (any? x)))
  (checking
    "Value"
    [v (gen/one-of [gen/keyword
                    gen/keyword-ns
                    gen/string-ascii
                    (gen/return nil)
                    gen/boolean
                    gen/large-integer])
     x (sut/generator `(t/Value ~v))]
    (is (= v x)))
  (checking
    "Union"
    [x (sut/generator `(t/U (t/Value :a) (t/Value :b)))]
    (is (#{:a :b} x)))
  (checking
    "t/Int"
    [x (sut/generator `t/Int)]
    (is (integer? x)))
  (checking
    "t/Bool"
    [x (sut/generator `t/Bool)]
    (is (boolean? x)))
  (checking
    "t/Str"
    [x (sut/generator `t/Str)]
    (is (string? x)))
  (checking
    "t/Kw"
    [x (sut/generator `t/Kw)]
    (is (keyword? x)))
  (checking
    "t/Sym"
    [x (sut/generator `t/Sym)]
    (is (symbol? x)))
  (checking
    "t/Num"
    [x (sut/generator `t/Num)]
    (is (number? x)))
  (checking
    "t/HVec"
    [x (sut/generator `'[t/Num t/Bool])]
    (is (vector? x))
    (is (= 2 (count x)))
    (is (number? (first x)))
    (is (boolean? (second x))))
  (checking
    "t/HSequential"
    [x (sut/generator `(t/HSequential [t/Num t/Bool]))]
    (is (sequential? x))
    (is (= 2 (count x)))
    (is (number? (first x)))
    (is (boolean? (second x))))
  (checking
    "t/HList"
    [x (sut/generator `(t/HList [t/Num t/Bool]))]
    (is (list? x))
    (is (= 2 (count x)))
    (is (number? (first x)))
    (is (boolean? (second x))))
  (checking
    "t/HSeq"
    [x (sut/generator `(t/HSeq [t/Num t/Bool]))]
    (is (seq? x))
    (is (= 2 (count x)))
    (is (number? (first x)))
    (is (boolean? (second x))))
  (checking
    "t/HVec + rest"
    [x (sut/generator `(t/HVec [t/Num t/Bool] :rest t/Int))]
    (is (vector? x))
    (is (<= 2 (count x)))
    (is (number? (first x)))
    (is (boolean? (second x)))
    (is (every? integer? (drop 2 x))))
  (checking
    "HMap"
    [x (sut/generator `'{:a t/Int})]
    (is (map? x))
    (is (integer? (:a x))))
  (checking
    "t/Sym"
    [x (sut/generator `t/Sym)]
    (is (symbol? x)))
  (checking
    "t/Set"
    [x (sut/generator `(t/Set t/Sym))]
    (is (set? x))
    (is (every? symbol? x))))

(declare Pong)
(t/defalias Ping
  (t/U nil '[Pong]))
(t/defalias Pong
  (t/U nil '[Ping]))

(comment
  (time
    (count-branches
      (gen/sample
        (sut/generator `Ping)
        90)))
  (drop 950
        (gen/sample
          (gen/recursive-gen
            (fn [Ping]
              (gen/tuple (gen/recursive-gen
                           (fn [Pong]
                             (gen/tuple Ping))
                           (gen/return nil))))
            (gen/return nil))
          1000))
  )

(declare Pong2)
(t/defalias Ping2
  (t/U nil '[Pong2 Pong2]))
(t/defalias Pong2
  (t/U nil '[Ping2 Ping2]))
(comment
  (gen/generate
    (sut/generator `Ping2)
    100)
  ;; reuse Ping
  (gen/generate
    (gen/recursive-gen
      (fn [Ping]
        (gen/tuple (gen/scale
                     #(/ % (inc (rand-int 2)))
                     (gen/recursive-gen
                       (fn [Pong]
                         (gen/tuple Ping))
                       (gen/return nil)))
                   (gen/scale
                     #(/ % (inc (rand-int 2)))
                     (gen/recursive-gen
                       (fn [Pong]
                         (gen/tuple Ping))
                       (gen/return nil)))))
      (gen/return nil))
    10000)
  )

(deftest recursive-generators-test
  ;; Mu
  (let [pred (fn pred [x]
               (or (nil? x)
                   (and (vector? x)
                        (= 1 (count x))
                        (pred (first x)))))]
    (checking
      "Mu"
      [x (sut/generator `(t/Rec [x#] (t/U nil '[x#])))]
      (is (pred x))))
  ;; Name
  (letfn [(ping? [x]
            (or (nil? x)
                (and (vector? x)
                     (= 1 (count x))
                     (pong? (first x)))))
          (pong? [x]
            (or (nil? x)
                (and (vector? x)
                     (= 1 (count x))
                     (ping? (first x)))))]
    (checking
      "Ping Name"
      [x (sut/generator `Ping)]
      (is (ping? x)))
    (checking
      "Pong Name"
      [x (sut/generator `Pong)]
      (is (pong? x)))))

(declare T P)
(t/defalias E
  "Expressions"
  (t/U '{:E ':var, :name t/Sym}
       '{:E ':if, :test E, :then E, :else E}
       '{:E ':lambda, :arg t/Sym, :arg-type T, :body E}
       '{:E ':app, :fun E, :args (t/Vec E)}
       '{:E ':false}
       '{:E ':n?}
       '{:E ':add1}))
(t/defalias T
  "Types"
  (t/U '{:T ':fun, :params (t/Vec '{:name t/Sym :type T}), :return T}
       '{:T ':not, :type T}
       '{:T ':union, :types (t/Set T)}
       '{:T ':intersection, :types (t/Set T)}
       '{:T ':false}
       '{:T ':num}
       '{:T ':refine, :name t/Sym, :prop P}))
(t/defalias P
  "Propositions"
  (t/U '{:P ':is, :exp E, :type T}
       '{:P ':=, :exps (t/Set E)}
       '{:P ':or, :ps (t/Set P)}
       '{:P ':and, :ps (t/Set P)}
       '{:P ':not, :p P}))

(defn count-branches [v]
  (let [a (atom 0)]
    (walk/postwalk
      (fn [v]
        (swap! a inc)
        v)
      v)
    @a))

#_
(deftest mini-occ-generators-test
  (time
    (count-branches
      (gen/generate
        (sut/generator `P)
        1000)))
  (time
    (count-branches
      (gen/generate
        gen/any
        10000)))
  )

#_ ;;TODO
(deftest fn-intersection-test
  (let [f (gen/generate (sut/generator `[t/Int :-> t/Int]))]
    (checking
      "Int arg"
      100
      [i gen/large-integer]
      (is (integer? (f i))))
    (doseq [v [nil :a 7.2]]
      (is (thrown? ExceptionInfo
                   (f v)))))
  (let [f (gen/generate (sut/generator `[[:-> t/Int] :-> t/Int]))]
    (is (f (constantly 1))))
  (let [f (gen/generate (sut/generator `[[:-> t/Bool] :-> t/Int]))]
    (is (thrown? ExceptionInfo (f (constantly 1)))))
  (let [f (gen/generate (sut/generator `[[t/Int :-> t/Int] :-> t/Int]))]
    (is (f identity)))
  (let [f (gen/generate (sut/generator `[[t/Int t/Bool t/Str :-> '[t/Str t/Bool t/Int]]
                                         :-> t/Int]))]
    (is (f (fn [a b c] [c b a])))))

#_ ;;TODO
(deftest poly-fn-test
  #_
  (let [f (gen/generate (sut/generator `(t/All [x#] [x# :-> x#])))]
    (is (f 1)))
  (is (sut/check `(t/All [x#] [x# :-> x#]) identity))
  (is (not (sut/check `(t/All [x# y#] [x# :-> y#]) identity)))
  (is (not (sut/check `(t/All [x#] [x# :-> '[x# x#]]) identity)))
  (is (sut/check `[t/Any :-> t/Any] identity))
  (is (sut/check `[t/Nothing :-> t/Nothing] identity))
  (is (sut/check `[t/Int :-> t/Any] identity))
  (is (not (sut/check `[t/Int :-> t/Nothing] identity)))
  (is (not (sut/check `[t/Any :-> t/Int] identity)))
  (is (sut/check `(t/IFn [t/Nothing :-> t/Nothing]
                         [t/Nothing :-> t/Any]
                         [t/Any :-> t/Any])
                 identity))
  (is (sut/check `(t/IFn [t/Int :-> t/Int]
                         [t/Bool :-> t/Bool]
                         [t/Any :-> t/Any])
                 identity))
  (is (not (sut/check `(t/IFn [t/Nothing :-> t/Nothing]
                              [t/Nothing :-> t/Any]
                              [t/Any :-> t/Nothing])
                      identity)))


  (is (sut/check `(t/All [x# y#] [[x# :-> y#] (t/Nilable (t/Coll x#)) :-> (t/Seq y#)]) map))
  (is (sut/check `[[t/Nothing :-> nil] (t/Nilable (t/Coll t/Nothing)) :-> (t/Seq nil)] map))
  (is (sut/check `[[nil :-> t/Nothing] (t/Nilable (t/Coll nil)) :-> (t/Seq t/Nothing)] map))
  (is (sut/check `[[nil :-> t/Nothing] (t/Nilable (t/Coll nil)) :-> t/Any] map))
  (is (sut/check `[[nil :-> t/Nothing] (t/Nilable (t/Coll nil)) :-> t/Any] map))
  ;;FIXME t/Seqable doesn't parse for some reason
  (is (sut/check `(t/All [x# y#] [[x# :-> y#] (t/Nilable (t/Seqable x#)) :-> (t/Seq y#)]) map))
  (is (sut/check `[[t/Nothing :-> nil] (t/Nilable (t/Seqable t/Nothing)) :-> (t/Seq nil)] map))
  (is (sut/check `(t/Seqable t/Int) [1]))
  (is (sut/check `(t/Coll t/Int) [1]))
  (is (sut/check `(t/Vec t/Int) [1]))
  )

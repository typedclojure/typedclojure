(ns clojure.core.typed.test.pred
  (:require 
    [clojure.test :refer :all]
    [clojure.core.typed :as t]))

(def Number? (t/pred Number))
(def Str? (t/pred t/Str))

(t/defalias NumberAlias 
  Number)

(deftest class-pred-test
  (is (Number? 1))
  (is (not (Number? nil)))
  (is (Str? "a"))
  (is (not (Str? 'sym))))

(deftest hmap-pred-test
  (is ((every-pred
         (t/pred 
           (t/HMap)))
       {}
       {:a 'blah}))
  (is ((complement
          (t/pred 
            (t/HMap :mandatory {:a Number})))
        {}))
  (is ((t/pred 
         (t/HMap :mandatory {:a Number})) 
       {:a 1}))
  (is ((every-pred
         (t/pred 
           (t/HMap :optional {:a Number})))
       {:a 1}
       {}
       {:b 'a}))
  (is ((every-pred
         (t/pred 
           (t/HMap :absent-keys #{:a})))
       {:b 'a}))
  (is (not
        ((every-pred
           (t/pred 
             (t/HMap :absent-keys #{:a})))
         {:a 'a})))
  )

(deftest hvec-pred-test
  (is ((t/pred '[Number Number])
       [1 2]))
  (is ((every-pred
         (complement 
           (t/pred '[Number Number])))
       ['a 2]
       []
       [1]
       [1 2 3]))
  (is ((every-pred 
         (t/pred 
           '[Number Number Number *]))
       [1 2]
       [1 2 3]
       [1 2 3 4 5 6 6 7 4 2 1]))
  (is ((every-pred 
         (t/pred 
           '[Number Number Number :*]))
       [1 2]
       [1 2 3]
       [1 2 3 4 5 6 6 7 4 2 1]))
  (is ((every-pred 
         (complement
           (t/pred 
             '[Number Number Number *])))
       []
       [1]
       [1 2 'a]
       [1 2 3 4 5 'a 6 7 4 2 1])))

(deftest rec-pred-test
  (is ((every-pred
         (t/pred (t/Rec [x] (t/U '[x] Number))))
       1
       '[1]
       '[[1]]
       '[[[[[2.2]]]]]))
  (is ((every-pred
         (t/pred (t/Rec [x] (t/U '{:a x} Number))))
       1
       '{:a 1}
       '{:a {:a 1}}
       '{:a {:a {:a {:a {:a 1}}}}}))
  (is ((every-pred
         (complement
           (t/pred (t/Rec [x] (t/U '[x] Number)))))
       '[1 1]
       '[[1] [1]])))

(t/defalias A
  (t/U '[A] Number))
(t/defalias B
  (t/U '{:a B} Number))
(t/defalias C
  (t/TFn [[x :variance :invariant]] (t/U '{:a (C x)} x)))

(deftest self-name-test
  (is ((every-pred
         (t/pred A))
       1
       '[1]
       '[[1]]
       '[[[[[2.2]]]]]))
  (is ((every-pred
         (t/pred B))
       1
       '{:a 1}
       '{:a {:a 1}}
       '{:a {:a {:a {:a {:a 1}}}}}))
  (is ((every-pred
         (complement
           (t/pred A)))
       '[1 1]
       '[[1] [1]]))
  #_ ;;FIXME infinite TApp types
  (is ((every-pred
         (complement
           (t/pred (C Number))))
       '[1 1]
       '[[1] [1]]))
)

(deftest singleton-pred-test
  (is ((t/pred true)
       true))
  (is ((t/pred (t/Value true))
       true))
  (is ((t/pred false)
       false))
  (is ((t/pred (t/Value false))
       false))
  (is ((t/pred 'sym)
       'sym))
  (is ((t/pred (t/Value sym))
       'sym))
  (is ((t/pred ':sym)
       ':sym))
  (is ((complement (t/pred ':sym))
       ':not-sym))
  (is ((t/pred (t/Value :sym))
       ':sym))
  (is ((complement (t/pred (t/Value :sym)))
       ':not-sym))
  (is ((t/pred (t/Val "a"))
       "a"))
  (is ((complement (t/pred (t/Val "a")))
       ':sym))
  (is ((t/pred nil)
       nil))
  (is ((t/pred (t/Value nil))
       nil))
  (is ((t/pred '1)
       1))
  (is ((every-pred
         (complement
           (t/pred '1)))
       1.0))
  (is (true? ((t/pred (t/Value 1))
              1)))
  (is (false? ((t/pred (t/Value 1))
               nil))))

(t/defalias String1<=10 (t/I t/Str (t/CountRange 1 10)))
(def string1<=10? (t/pred String1<=10))

;;not counted?
(defn lazy-len [n]
  (lazy-seq
    (when (pos? n)
      (cons n (lazy-len (dec n))))))

(deftest countrange-pred-test
  (is ((every-pred
         (t/pred (t/CountRange 0)))
       nil
       []
       {}
       '()))
  (is ((every-pred
         (complement 
           (t/pred (t/CountRange 0))))
       ; only supports clojure collections
       (into-array [])))
  (is (not (string1<=10? "")))
  (is (string1<=10? "012345"))
  (is (string1<=10? "012345678"))
  (is (string1<=10? "0123456789"))
  (is (not (string1<=10? "0123456789ten")))
  (is ((t/pred (t/CountRange 1)) (range)))
  (is ((t/pred (t/CountRange 1 10)) (range 10)))
  (is ((t/pred (t/CountRange 1 10)) (lazy-len 10)))
  (is (not ((t/pred (t/CountRange 1 9)) (lazy-len 10))))
  ;; eductions not supported, not immutable
  (is (not ((t/pred (t/CountRange 0)) (eduction)))))

(deftest intersect-pred-test
  (is ((every-pred
         (t/pred (t/I Number Long)))
       1))
  (is ((every-pred
         (complement
           (t/pred (t/I Number Long))))
       1.1))
  (is ((every-pred
         (complement
           (t/pred (t/I Number Long))))
       1.1)))
       
(deftest union-pred-test
  (is ((every-pred
         (t/pred (t/U Number Long)))
       1
       1.1))
  (is ((every-pred
         (complement
           (t/pred (t/U Number Long))))
       'a))
  (is ((every-pred
         (complement
           (t/pred t/Nothing))
         (complement
           (t/pred (t/U))))
       'a)))

(deftest tfn-name-test
  (is ((every-pred
         (t/pred (clojure.core.typed/Option Number)))
       nil
       1
       1.1)))

(deftest iseq-pred-test
  (is ((every-pred
         (t/pred (t/Seq Number)))
       '(1 2)))
  (is ((every-pred
         (complement
           (t/pred (t/Seq Number))))
       [1 2])))

(deftest any-pred-test
  (is ((every-pred
         (t/pred t/Any))
       1 2 nil [1]))
  (is ((every-pred
         (t/pred (t/U t/Any t/Num)))
       1 2 nil [1])))

(ns typed.clj.annotator.test.runtime-infer.polymorphic
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}
   }
  (:refer-clojure :exclude [identity memoize])
  (:require [typed.clojure :as t]
            [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]))

;; Start: Generated by clojure.core.typed - DO NOT EDIT
(declare AMap)
(t/defalias AMap '{:a (t/U t/Int t/Sym)})
(t/ann
  get-a
  [(t/U AMap (t/Map t/Nothing t/Nothing)) :-> (t/U nil t/Int t/Sym)])
(comment (t/ann get-a (t/All [x] ['{:a x} :-> x])))
(comment (t/ann get-a (t/All [x] ['{:a x} :-> x])))
(t/ann identity [t/Int :-> t/Int])
(comment (t/ann identity (t/All [x] [x :-> x])))
(comment (t/ann identity (t/All [x] [x :-> x])))
(t/ann memoize AnyFunction)
(t/ann
  mymap
  [[(t/U t/Int t/Sym) :-> (t/U t/Str t/Int)]
   (t/Vec (t/U t/Int t/Sym))
   :->
   (t/Coll (t/U t/Str t/Int))])
(comment (t/ann mymap (t/All [x] [[x :-> ?] (t/Vec x) :-> ?])))
(comment (t/ann mymap (t/All [x] [[x :-> ?] '[? ? x] :-> ?])))
(comment (t/ann mymap (t/All [x] [[? :-> x] ? :-> (t/Coll x)])))
(comment (t/ann mymap (t/All [x] [[x :-> ?] '[x ? ?] :-> ?])))
(comment (t/ann mymap (t/All [x] [[x :-> ?] '[? x ?] :-> ?])))
(comment (t/ann mymap (t/All [x] [[? :-> x] ? :-> (t/Coll x)])))
(comment (t/ann mymap (t/All [x] [[x :-> ?] '[x] :-> ?])))
(t/ann plus1 [t/Int :-> t/Int])
;; End: Generated by clojure.core.typed - DO NOT EDIT
(s/def identity ifn?)

(defn identity [x]
  x)

(defn plus1 [x]
  (inc x))

(defn memoize [f]
  (fn [a]
    (f a)))

(defn mymap [f c]
  (map f c))

(defn get-a [m]
  (:a m))

(identity 1)
;(identity 'a)
;(identity :a)

;(memoize identity)
;((memoize identity) 1)
;(identity 1)
;(identity 2)

;((memoize plus1) 1)

(mymap plus1 [1])
(mymap name ['a 'b 'c])

(get-a {:a 1})
(get-a {:a 'a})
(get-a {})
(get-a {})

;
;((memoize identity) 'a)
;((nth (iterate memoize identity) 100) :a)

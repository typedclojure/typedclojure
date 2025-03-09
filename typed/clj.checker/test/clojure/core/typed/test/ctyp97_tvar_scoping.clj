(ns ^:typed.clojure clojure.core.typed.test.ctyp97-tvar-scoping
  (:import (clojure.lang ASeq LazySeq))
  (:require [clojure.core.typed :as t :refer [ann]]))

(ann reduce_ (t/All [a b] 
               (t/IFn 
                 [[a b -> b] b (t/Seqable a) -> b]
                 [[a (t/Seqable (t/U a b)) -> (t/Seqable (t/U a b))]
                  (t/Seqable (t/U a b)) (t/Seqable a) -> (t/Seqable (t/U a b))])))
(defn reduce_
  [f zero coll]
  (if-let [s (seq coll)]
    (f (first s)
       (reduce_ f zero (rest s)))
    zero))

(ann append_ (t/All [a b] [(t/Seqable a) (t/Seqable b) -> (t/U (t/Seqable (t/U a b))
                                                           (ASeq (t/U a b)))]))
(defn append_
  [coll1 coll2]
  (reduce_ (t/inst cons (t/U a b)) coll2 coll1))


; -------- main

; if you comment out this function, you will get :ok
(ann map-1 (t/All [a b] [[a -> b] (t/Seqable a)
                       -> (LazySeq b)]))
(defn map-1 [f coll]
  (lazy-seq
   (if-let [s (seq coll)]
     (cons (f (first s))
           (map-1 f (rest s))))))

(ann map-2 (t/All [a b] [[a -> b] (t/Seqable a) -> (LazySeq b)]))
(defn map-2
  [f coll]
  (lazy-seq
   (reduce_ (t/fn [x :- a ; ERROR!! -- Cannot resolve type: a
                   y :- (t/Seqable b)]
              (append_ [(f x)] y))
            []
            coll)))

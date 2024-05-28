(ns ^:no-doc typed.cljc.runtime.perf-utils
  (:refer-clojure :exclude [some repeatedly]))

(defn some
  "Like `clojure.core/some`, but uses an iterator over `lst`."
  [f ^Iterable lst]
  (let [it (.iterator lst)]
    (loop []
      (when (.hasNext it)
        (or (f (.next it))
            (recur))))))

(defn repeatedly
  "Like `clojure.core/repeatedly`, but eager and more efficient."
  [^long n f]
  (let [arr (object-array n)]
    (dotimes [i n]
      (aset arr i (f)))
    (vec arr)))

(defn reduce2
  "Like `clojure.core/reduce`, but goes over two collections at once and accepts a
  3-arity `f`. Uses iterators to iterate the collections."
  [f init coll1 coll2]
  ;; NB: Ignores reduced? !
  (if (or (nil? coll1) (nil? coll2))
    init
    (let [it1 (.iterator ^Iterable coll1)
          it2 (.iterator ^Iterable coll2)]
      (loop [res init]
        (if (and (.hasNext it1) (.hasNext it2))
          (recur (f res (.next it1) (.next it2)))
          res)))))

(defn reduce4
  "Like `clojure.core/reduce`, but goes over four collections at once and accepts
  a 5-arity `f`. Uses iterators to iterate the collections."
  [f init coll1 coll2 coll3 coll4]
  ;; NB: Ignores reduced? !
  (if (or (nil? coll1) (nil? coll2) (nil? coll3) (nil? coll4))
    init
    (let [it1 (.iterator ^Iterable coll1)
          it2 (.iterator ^Iterable coll2)
          it3 (.iterator ^Iterable coll3)
          it4 (.iterator ^Iterable coll4)]
      (loop [res init]
        (if (and (.hasNext it1) (.hasNext it2)  (.hasNext it3) (.hasNext it4))
          (recur (f res (.next it1) (.next it2) (.next it3) (.next it4)))
          res)))))
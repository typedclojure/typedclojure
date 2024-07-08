(ns ^:no-doc typed.cljc.runtime.perf-utils
  (:refer-clojure :exclude [every? some repeatedly reduce]))

(defn some
  "Like `clojure.core/some`, but uses an iterator over `lst`."
  [f ^Iterable lst]
  (let [it (.iterator lst)]
    (loop []
      (when (.hasNext it)
        (or (f (.next it))
            (recur))))))

(defn every?
  "Like `clojure.core/every?`, but uses an iterator over `lst`."
  [f ^Iterable lst]
  (let [it (.iterator lst)]
    (loop []
      (if (.hasNext it)
        (if (f (.next it))
          (recur)
          false)
        true))))

(defn repeatedly
  "Like `clojure.core/repeatedly`, but eager and more efficient."
  [^long n f]
  (let [arr (object-array n)]
    (dotimes [i n]
      (aset arr i (f)))
    (vec arr)))

(defn reduce
  "Like `clojure.core/reduce`, but goes over several collections at once. The
  provided `f` should accept the same number of arguments as N of collections +
  1. Uses iterators to iterate the collections."
  ([f init coll1 coll2]
   (if (or (nil? coll1) (nil? coll2))
     init
     (let [it1 (.iterator ^Iterable coll1)
           it2 (.iterator ^Iterable coll2)]
       (loop [res init]
         (if (and (.hasNext it1) (.hasNext it2))
           (let [res (f res (.next it1) (.next it2))]
             (if (reduced? res)
               @res
               (recur res)))
           res)))))
  ([f init coll1 coll2 coll3]
   (if (or (nil? coll1) (nil? coll2) (nil? coll3))
     init
     (let [it1 (.iterator ^Iterable coll1)
           it2 (.iterator ^Iterable coll2)
           it3 (.iterator ^Iterable coll3)]
       (loop [res init]
         (if (and (.hasNext it1) (.hasNext it2) (.hasNext it3))
           (let [res (f res (.next it1) (.next it2) (.next it3))]
             (if (reduced? res)
               @res
               (recur res)))
           res)))))
  ([f init coll1 coll2 coll3 coll4]
   (if (or (nil? coll1) (nil? coll2) (nil? coll3) (nil? coll4))
     init
     (let [it1 (.iterator ^Iterable coll1)
           it2 (.iterator ^Iterable coll2)
           it3 (.iterator ^Iterable coll3)
           it4 (.iterator ^Iterable coll4)]
       (loop [res init]
         (if (and (.hasNext it1) (.hasNext it2) (.hasNext it3) (.hasNext it4))
           (let [res (f res (.next it1) (.next it2) (.next it3) (.next it4))]
             (if (reduced? res)
               @res
               (recur res)))
           res))))))

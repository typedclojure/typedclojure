(ns ^:typed.clojure ^:no-doc typed.cljc.runtime.perf-utils
  (:refer-clojure :exclude [every? some repeatedly reduce]))

(defn some
  "Like `clojure.core/some`, but uses an iterator over `lst`."
  [f lst]
  (when (some? lst)
    (let [it #?(:cljr (.GetEnumerator lst)
                :default (.iterator ^Iterable lst))]
      (loop []
        (when #?(:cljr (.MoveNext it)
                 :default (.hasNext it))
          (or (f #?(:cljr (.Current it)
                    :default (.next it)))
              (recur)))))))

(defn every?
  "Like `clojure.core/every?`, but uses an iterator over `lst`."
  [f lst]
  (if (some? lst)
    (let [it #?(:cljr (.GetEnumerator lst)
                :default (.iterator ^Iterable lst))]
      (loop []
        (if #?(:cljr (.MoveNext it)
               :default (.hasNext it))
          (if (f #?(:cljr (.Current it)
                    :default (.next it)))
            (recur)
            false)
          true)))
    true))

;(t/ann vrepeatedly (t/All [x] [t/Int [:-> x] :-> (t/Vec x)]))
(defn vrepeatedly
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
     (let [it1 #?(:cljr (.GetEnumerator coll1)
                  :default (.iterator ^Iterable coll1))
           it2 #?(:cljr (.GetEnumerator coll2)
                  :default (.iterator ^Iterable coll2))]
       (loop [res init]
         (if (and #?(:cljr (.MoveNext it1)
                     :default (.hasNext it1))
                  #?(:cljr (.MoveNext it2)
                     :default (.hasNext it2)))
           (let [res (f res
                        #?(:cljr (.Current it1)
                           :default (.next it1))
                        #?(:cljr (.Current it2)
                           :default (.next it2)))]
             (if (reduced? res)
               @res
               (recur res)))
           res)))))
  ([f init coll1 coll2 coll3]
   (if (or (nil? coll1) (nil? coll2) (nil? coll3))
     init
     (let [it1 #?(:cljr (.GetEnumerator coll1)
                  :default (.iterator ^Iterable coll1))
           it2 #?(:cljr (.GetEnumerator coll2)
                  :default (.iterator ^Iterable coll2))
           it3 #?(:cljr (.GetEnumerator coll3)
                  :default (.iterator ^Iterable coll3))]
       (loop [res init]
         (if (and #?(:cljr (.MoveNext it1)
                     :default (.hasNext it1))
                  #?(:cljr (.MoveNext it2)
                     :default (.hasNext it2))
                  #?(:cljr (.MoveNext it3)
                     :default (.hasNext it3)))
           (let [res (f res
                        #?(:cljr (.Current it1)
                           :default (.next it1))
                        #?(:cljr (.Current it2)
                           :default (.next it2))
                        #?(:cljr (.Current it3)
                           :default (.next it3)))]
             (if (reduced? res)
               @res
               (recur res)))
           res)))))
  ([f init coll1 coll2 coll3 coll4]
   (if (or (nil? coll1) (nil? coll2) (nil? coll3) (nil? coll4))
     init
     (let [it1 #?(:cljr (.GetEnumerator coll1)
                  :default (.iterator ^Iterable coll1))
           it2 #?(:cljr (.GetEnumerator coll2)
                  :default (.iterator ^Iterable coll2))
           it3 #?(:cljr (.GetEnumerator coll3)
                  :default (.iterator ^Iterable coll3))
           it4 #?(:cljr (.GetEnumerator coll4)
                  :default (.iterator ^Iterable coll4))]
       (loop [res init]
         (if (and #?(:cljr (.MoveNext it1)
                     :default (.hasNext it1))
                  #?(:cljr (.MoveNext it2)
                     :default (.hasNext it2))
                  #?(:cljr (.MoveNext it3)
                     :default (.hasNext it3))
                  #?(:cljr (.MoveNext it4)
                     :default (.hasNext it4)))
           (let [res (f res
                        #?(:cljr (.Current it1)
                           :default (.next it1))
                        #?(:cljr (.Current it2)
                           :default (.next it2))
                        #?(:cljr (.Current it3)
                           :default (.next it3))
                        #?(:cljr (.Current it4)
                           :default (.next it4)))]
             (if (reduced? res)
               @res
               (recur res)))
           res))))))

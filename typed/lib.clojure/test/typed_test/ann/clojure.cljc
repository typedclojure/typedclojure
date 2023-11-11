(ns typed-test.ann.clojure
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :as clj]
            [typed.cljs.checker.test-utils :as cljs]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.java.io :as io]))

(def ^:dynamic *platforms* #{:clj :cljs})

(defmacro is-tc-e [& args]
  `(do (when (:clj *platforms*)
         (clj/is-tc-e ~@args))
       (when (:cljs *platforms*)
         (cljs/is-tc-e ~@args))))

(defmacro is-tc-err [& args]
  `(do (when (:clj *platforms*)
         (clj/is-tc-err ~@args))
       (when (:cljs *platforms*)
         (cljs/is-tc-err ~@args))))

(defmacro without-cljs [& body]
  `(binding [*platforms* (disj *platforms* :cljs)]
     (do ~@body)))

(defn check-deftest-ordering []
  (let [filename "typed_test/ann/clojure.cljc"]
    (with-open [rdr (io/reader (io/resource filename))]
      (let [pbr (readers/indexing-push-back-reader
                  (java.io.PushbackReader. rdr) 1 filename)
            eof (Object.)
            read-opts {:eof eof :features #{:clj}}
            read-opts (if (.endsWith filename "cljc")
                        (assoc read-opts :read-cond :allow)
                        read-opts)
            deftests (binding [*ns* *ns*
                               *file* filename]
                       (loop [deftests []]
                         (let [form (reader/read read-opts pbr)]
                           (if (identical? form eof)
                             deftests
                             (recur (cond-> deftests
                                      (and (seq? form)
                                           (= 'deftest (first form)))
                                      (conj (second form))))))))
            _ (assert (seq deftests))
            _ (assert (apply distinct? deftests)
                      (str "The following deftests are repeated: "
                           (vec (keys (remove (comp #{1} val) (frequencies deftests))))))
            sorted-deftests (vec (sort deftests))
            good-pos (zipmap sorted-deftests (range))
            bad-pos (zipmap deftests (range))]
        (when-some [bad-deftest (some #(when (not= (good-pos %)
                                                   (bad-pos %))
                                         %)
                                      deftests)]
          (throw (ex-info (str bad-deftest
                               " should go after " (->> bad-deftest
                                                        good-pos
                                                        dec
                                                        (nth sorted-deftests)))
                          {})))))))

(check-deftest-ordering)

(deftest *-test
  (is-tc-e (* 1 2) t/Int)
  (is-tc-err (* 1.5 2) t/Int))

(deftest +-test
  (is-tc-e (+ 1 2) t/Int)
  (is-tc-err (+ 1.5 2) t/Int))

(deftest --test
  (is-tc-e (- 1 2) t/Int)
  (is-tc-err (- 1.5 2) t/Int))

(deftest apply-test
  (is-tc-e (apply merge [{:a 1}])
           (t/Nilable (t/Map t/Any t/Any))))

(deftest boolean-test
  (is-tc-e (boolean [])     t/Bool)
  (is-tc-e (boolean "true") t/Bool)
  (is-tc-e (boolean :false) t/Bool))

(deftest butlast-test
  (is-tc-e (butlast [1 2 3]) (t/NilableNonEmptySeq t/Num))
  ;;FIXME
  (without-cljs
    (is-tc-e (butlast (seq [1 2 3])) (t/NilableNonEmptySeq t/Num)))
  (is-tc-e (butlast [])  t/AnyNilableNonEmptySeq))

(deftest coll?-test
  (is-tc-e (coll? {:a 1}) t/Bool)
  (is-tc-e (coll? #{1 2}) t/Bool)
  (is-tc-e (coll? [2 3])  t/Bool)
  (is-tc-e (coll? "foo")  t/Bool)
  (is-tc-e (coll? 2)      t/Bool))

(deftest empty?-test
  (is-tc-e (empty? [])  t/Bool)
  (is-tc-e (empty? #{}) t/Bool)
  (is-tc-e (empty? [1]) t/Bool)
  (is-tc-e (empty? {:a 1}) t/Bool))

(deftest false?-test
  (is-tc-e (false? [])    t/Bool)
  (is-tc-e (false? false) t/Bool)
  (is-tc-e (false? true)  t/Bool)
  (is-tc-e (false? 1)     t/Bool))

(deftest find-test
  (is-tc-e (when-some [e (find nil :a)]
             [(key e) (val e)])
           nil)
  (is-tc-e (when-some [e (find {:a 1} :a)]
             [(key e) (val e)])
           (t/Nilable '[':a t/Int]))
  (is-tc-e (when-some [e (find (when (< (rand) 0.5) {:a 1}) :a)]
             [(key e) (val e)])
           (t/Nilable '[':a t/Int]))
  ;;FIXME
  (without-cljs
    (is-tc-e (fn [a :- (t/Map t/Int t/Str)] :- (t/U nil '[t/Int t/Str])
               (find a 1)))))

(deftest first-test 
  (is-tc-e (first [8]) t/Num)
  (is-tc-e (first [1 2 3]) t/Num)
  (clj/is-tc-e (first (seq [1 2 3])) t/Num)
  ;;FIXME
  (without-cljs
    (is-tc-e (first (seq [1 2 3])) t/Num))
  (is-tc-e (first nil) nil)
  (is-tc-e (first [1]) '1)
  (is-tc-e (first []) nil)
  (is-tc-e (first []) nil)
  (is-tc-e (let [l [1 2 3]]
             (if (seq l)
               (first l)
               (throw (Exception. "Error"))))
           t/Num))

(deftest fn?-test
  (is-tc-e (fn? (fn [x y] y))  t/Bool)
  (is-tc-e (fn? map) t/Bool))

(deftest fnil-test
  (is-tc-e ((fnil + (ann-form 0 t/Num)) 2))
  ;;FIXME probably related to how we handle :invariant variables in subst-gen
  #_(is-tc-e ((fnil + 0) 2))
  (is-tc-e ((fnil + 0) nil))
  ;;FIXME probably related to how we handle :invariant variables in subst-gen
  #_
  (is-tc-e ((fnil (t/ann-form + [t/Int :* :-> t/Int])
                  0)
            2.2))
  ; can Typed Racket do better here?
  (is-tc-e ((fnil (t/ann-form + [t/Num :* :-> t/Num])
                  (t/ann-form 0 t/Num))
            2.2)))

(deftest future-test
  (clj/is-tc-e @(future 'a) t/Sym)
  (clj/is-tc-e (future 'a) java.util.concurrent.Future))

(deftest group-by-test
  ;;FIXME
  (without-cljs
    (is-tc-e (group-by even? [1 2 3 4]))
    ;;non-empty groups
    (is-tc-e (let [vs (some-> (group-by even? [1 2 3 4])
                              first
                              val)]
               (assert vs)
               (t/ann-form (first vs) t/Int)))))

(deftest identity-test
  (is-tc-e (identity 8)       t/Num)
  (is-tc-e (identity "hello") t/Str)
  (is-tc-e (identity [0])     (t/Vec t/Num)))

(deftest ifn?-test
  (is-tc-e (ifn? (fn [x] x)) t/Bool)
  (is-tc-e (ifn? "foo")      t/Bool))

(deftest indexed?-test
  (is-tc-e (indexed? 1)))

(deftest int-test
  (is-tc-e (int 1) t/Int))

(deftest integer?-test
  (is-tc-e (integer? 2)   t/Bool)
  (is-tc-e (integer? 1.3) t/Bool)
  (is-tc-e (integer? "8") t/Bool))

(deftest last-test
  (is-tc-e (last [1 2 3])       t/Num)
  ;;FIXME
  (without-cljs
    (is-tc-e (last (seq [1 2 3])) t/Num))
  (is-tc-e (last [])            (t/Option t/Any)))

(deftest map-indexed-test
  (is-tc-e (map-indexed (t/inst vector t/Any t/AnyInteger t/Int) 
                        [1 2])
           (t/Seqable '[t/AnyInteger t/Int])))

(deftest map-test
  (is-tc-e (map + [1 2])
           (t/Seqable t/Num))
  ;;FIXME
  (without-cljs
    (is-tc-e (map + [1 2] [1 2] [4 5] [6 7] [4 4] [3 4]))
    (is-tc-e (map + [1 2] [1 2] [4 5] [6 7] [4 4] [3 4])
             (t/Seqable t/Num)))
  (is-tc-err (map + [1 2] [1 2] [4 5] [6 7] [4 4] {3 4}))
  (is-tc-err (map + [1 2] [1 2] [4 5] [6 7] [4 4] #{'a 4})))


(deftest map?-test
  (is-tc-e (map? {:a 1}) t/Bool)
  (is-tc-e (map? {})     t/Bool)
  (is-tc-e (map? [0 1])  t/Bool))

(deftest nil?-test
  (is-tc-e (nil? nil)   t/Bool)
  (is-tc-e (nil? "nil") t/Bool))

(deftest nth-test
  ;;FIXME
  (without-cljs
    (is-tc-err #(nth #{1 2} 0))
    (is-tc-err #(nth (ann-form #{1 2} (t/Seqable t/Any)) 0))
    (is-tc-err #(nth (ann-form [1 2] (t/Seqable t/Any)) 0))
    (is-tc-err #(nth {1 0} 0))
    (is-tc-e (nth [1] 0) t/Int)
    (is-tc-e (nth '(1) 0) t/Int)
    (is-tc-e (nth (ann-form "a" CharSequence) 0) Character)
    (is-tc-e (nth "a" 0) Character)
    (is-tc-e (let [nth (ann-form nth 
                                 (t/All [x y] 
                                        [(t/U (clojure.lang.Indexed x) 
                                              (t/I clojure.lang.Sequential (t/Seqable x))) 
                                         t/Int -> t/Any]))]
               (nth "a" 0)))
    (clj/is-tc-e (nth (ann-form (java.util.ArrayList. [1 2 3]) (java.util.RandomAccess t/Any)) 0))
    (clj/is-tc-e (nth (java.util.ArrayList. [1 2 3]) 0))
    ; this used to fail randomly
    (is-tc-e (let [nth (ann-form nth (t/All [x y] 
                                            [(t/U (t/Indexed x) 
                                                  (t/I t/Sequential (t/Seqable x))) 
                                             t/Int -> t/Any]))]
               (nth "a" 0)))))

(deftest number?-test
  (is-tc-e (number? 8)     t/Bool)
  (is-tc-e (number? [1 2]) t/Bool))

(deftest peek-test
  (is-tc-e (peek [{:foo "bar" :baz "zot"} {:foo "bar"}]) (t/Map t/Kw t/Str)))

(deftest pop-test
  (is-tc-e (pop [1 2 3]) (t/Vec t/Num)))

(deftest rand-int-test
  (is-tc-e (rand-int 1)    t/Int)
  (is-tc-e (rand-int 1000) t/Int))

(deftest requiring-resolve-test
  (clj/is-tc-e ((requiring-resolve 'clojure.core/+)
                1
                2))
  (clj/is-tc-err ((requiring-resolve 'clojure.core/+)
                  nil)))

(deftest reset!-test
  (is-tc-e (reset!
             (ann-form (atom 1) (t/Atom t/Num))
             10.1)
           t/Num))

(deftest rest-test
  (is-tc-e (rest [1 2 3])       (t/Seq t/Num))
  ;;FIXME
  (without-cljs
    (is-tc-e (rest (seq [1 2 3])) (t/Seq t/Num)))
  (is-tc-e (rest [])            (t/Seq t/Any)))

(deftest reverse-test
  (is-tc-e (reverse [1 2 3]) (t/Seq t/Int))
  (is-tc-e (reverse {:a 1 :b 2}) (t/Seq t/Any))
  (is-tc-e (reverse #{1 2 3})  (t/Seq t/Int)))

(deftest rseq-test
  (is-tc-e (rseq [1 2 3]) (t/Option (t/NonEmptyASeq t/Int))))

(deftest second-test
  (is-tc-e (second [1 2 3]) t/Num)
  (is-tc-e (second [])      nil)
  (is-tc-e (second nil)     nil)
  ;; FIXME
  (without-cljs
    (is-tc-e (second (seq [1 2 3])) (t/Option t/Num))))

(deftest seq-test
  (is-tc-e (seq [1 2 3]) (t/NilableNonEmptySeq t/Num))
  (is-tc-e (seq []) (t/Option (t/NonEmptyASeq t/Any))))

(deftest seq?-test
  (is-tc-e (seq? [])           t/Bool)
  (is-tc-e (seq? [1 2 3])      t/Bool)
  (is-tc-e (seq? #{2 3})       t/Bool)
  (is-tc-e (seq? (seq [1 2]))  t/Bool)
  (is-tc-e (seq? (seq #{2 3})) t/Bool))

(deftest seqable?-test
  (is-tc-e (seqable? 1)))

(deftest shuffle-test
  (is-tc-e (shuffle [1 2 3 4]) (t/Vec t/Any))
  ;;FIXME
  (without-cljs
    (is-tc-e (shuffle [1 2 3 4]) (t/Vec t/Num)))
  (is-tc-e (shuffle #{4 8 2}) (t/Vec t/Int)))

(deftest some?-test
  (is-tc-e (let [x (ann-form 1 (t/U nil t/Int))]
             (when (some? x)
               (inc x)))))

(deftest sorted-map-test
  (is-tc-e (sorted-map 1 2)
           (t/SortedMap t/Int t/Any))
  (is-tc-e (let [m (sorted-map 1 2)]
             (t/ann-form m (t/SortedMap '1 t/Any))))
  (is-tc-err (let [m (sorted-map 1 2)]
               ;;invariant keys
               (t/ann-form m (t/SortedMap t/Int t/Any))))
  #_;;FIXME
  (is-tc-err #(assoc (t/ann-form (sorted-map 1 2) (t/Map t/Int t/Int)) :a 1))
  #_;;FIXME
  (is-tc-err #(get (t/ann-form (sorted-map 1 2) (t/Map t/Int t/Int)) :a))
  )

(deftest sorted-set-test
  (is-tc-e (sorted-set 1 2)
           (t/SortedSet t/Int))
  (is-tc-e (let [m (sorted-set 1)]
             (t/ann-form m (t/SortedSet '1))))
  (is-tc-err (let [m (sorted-set 1)]
               ;;invariant keys
               (t/ann-form m (t/SortedSet t/Int))))
  #_;;FIXME
  (is-tc-err #(conj (t/ann-form (sorted-set 1) (t/Set t/Int)) :a))
  #_;;FIXME
  (is-tc-err #(get (sorted-set 1) :a))
  #_;;FIXME
  (is-tc-err #(get (sorted-set 1) :a nil))
  #_;;FIXME
  (is-tc-err #(get (t/ann-form (sorted-set 1) (t/Set t/Int)) :a)))

(deftest string?-test
  (is-tc-e (string? "hello") t/Bool))

(deftest subseq-test
  (is-tc-e (subseq (sorted-map) = 0))
  (is-tc-e (subseq (sorted-map 1 2) = 0))
  (is (= [[0 1]] (subseq (sorted-map 0 1 1 2) = 0)))
  (is-tc-e (subseq (sorted-map 0 1 1 2) = 0))
  (is-tc-e (subseq (sorted-map 1 2) = 0) (t/Seqable (t/MapEntry t/Int t/Int)))
  (is-tc-err (subseq (sorted-map 1 2) = 0) (t/Seqable (t/MapEntry t/Bool t/Int)))
  (is-tc-err (subseq (sorted-map) (fn [_ _]) 0))
  (is (= [0] (subseq (sorted-set 0 1) = 0)))
  (is-tc-e (subseq (sorted-set 0 1 1 2) = 0))
  (is-tc-e (subseq (sorted-set 1 2) = 0) (t/Seqable t/Int))
  (is-tc-err (subseq (sorted-set 1 2) = 0) (t/Seqable t/Bool))
  (is-tc-err (subseq (sorted-set 1 2) (fn [_ _]) 0)))

(deftest true?-test
  (is-tc-e (true? [])    t/Bool)
  (is-tc-e (true? false) t/Bool)
  (is-tc-e (true? true)  t/Bool)
  (is-tc-e (true? 1)     t/Bool))

(deftest vector?-test
  (is-tc-e (vector? [])     t/Bool)
  (is-tc-e (vector? [1])    t/Bool)
  (is-tc-e (vector? {:a 1}) t/Bool)
  (is-tc-e (vector? "foo")  t/Bool))

;;TODO cljs support
(deftest volatile-test
  (clj/is-tc-e (volatile! nil))
  (clj/is-tc-e (volatile! nil) (t/Volatile nil))
  (clj/is-tc-e (volatile! nil) (t/Volatile t/Any))
  (clj/is-tc-err (volatile! nil) (t/Volatile t/Int))
  (clj/is-tc-e (let [v (volatile! (t/ann-form 1 t/Int))]
                 (vreset! v 1)))
  (clj/is-tc-err (let [v (volatile! (t/ann-form 1 t/Int))]
                   (vreset! v nil)))
  #_ ;;TODO add support similar to supressing :inline expansion where
  ;; a macro can be checked as a fn
  (clj/is-tc-e (let [v (volatile! (t/ann-form 1 t/Int))]
                 (vswap! v inc))))

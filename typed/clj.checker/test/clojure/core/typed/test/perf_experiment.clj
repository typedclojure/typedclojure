(ns clojure.core.typed.test.perf-experiment
  (:refer-clojure :exclude [run!])
  (:require [clojure.test :refer :all]
            ;[criterium.core :as bench]
            [clojure.core.typed :as t]
            [clj-java-decompiler.core :refer [decompile]])
  (:import [java.util IdentityHashMap]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn run! [f coll]
  (clojure.core/run! (fn [e]
                       (f e)
                       nil)
                     coll))

(def cls-prefix "A")

(def ^IdentityHashMap impl-hashes (IdentityHashMap. 80))

(let [next-int (atom -1)]
  (defn add-class [cls]
    {:pre [(class? cls)]}
    (let [i (int (swap! next-int unchecked-inc))]
      (assert (<= 0 i 98))
      (locking impl-hashes
        (.put impl-hashes cls i)))))

(defmacro gen-classes [n]
  (list*
    'do
    (map (fn [i]
           (let [cls (symbol (str cls-prefix i))]
             `(do (deftype ~cls [])
                  (add-class ~cls))))
         (range n))))

(let [ih impl-hashes]
  (defn smash-classes [t1 t2]
    ;; assumption: impl-hashes contains ints 0 <= i < 99
    (unchecked-add
      (unchecked-multiply 100 (unchecked-inc (.get ih t1))) ;; add two zeros on the right
      (unchecked-inc (.get ih t2)))))

(comment
  (decompile
    )
  )

(defmacro multi-dispatch-tests [n]
  {:pre [(integer? n)]}
  (let [[t1 t2] (repeatedly 2 #(with-meta (gensym) {:tag 'java.lang.Object}))
        rand-cls #(symbol (str cls-prefix (rand-int n)))
        branches (into {}
                       (comp (distinct)
                             (take 100)
                             (map #(vector % (rand-int n))))
                       (repeatedly (juxt rand-cls rand-cls)))
        default (rand-int n)]
    `(do (defn ~'count-with-instance [~t1 ~t2]
           (cond
             ~@(mapcat (fn [[[l r] res]]
                         [`(and (instance? ~l ~t1)
                                (instance? ~r ~t2))
                          res])
                       branches)
             :else ~default))
         (let [smsh# smash-classes]
           (defn ~'count-with-case [~t1 ~t2]
             (case (int (smsh# (.getClass ~t1) (.getClass ~t2)))
               ~@(mapcat (fn [[[l r] res]]
                           [(smash-classes (resolve l) (resolve r))
                            res])
                         branches)
               ~default)))
         )))

(defmacro single-dispatch-tests [n]
  {:pre [(integer? n)]}
  (let [[t1] (repeatedly 1 #(with-meta (gensym) {:tag 'java.lang.Object}))
        branches (into {}
                       (map #(vector (symbol (str cls-prefix (rand-int %))) (rand-int n)))
                       (range n))
        default (rand-int n)]
    `(do (defn ~'count-with-instance-single [~t1]
           (cond
             ~@(mapcat (fn [[l res]]
                         [`(instance? ~l ~t1)
                          res])
                       branches)
             :else ~default))
         (let [imp# impl-hashes]
           (defn ~'count-with-case-single [~t1]
             (case (int (.get imp# (.getClass ~t1)))
               ~@(mapcat (fn [[l res]]
                           [(int (.get impl-hashes (resolve l)))
                            res])
                         branches)
               ~default))))))

(defmacro single-dispatch-hashcode-tests [n]
  {:pre [(integer? n)]}
  (let [[t1] (repeatedly 1 #(with-meta (gensym) {:tag 'java.lang.Object}))
        branches (into {}
                       (map #(vector (symbol (str cls-prefix (rand-int %))) (rand-int n)))
                       (range n))
        default (rand-int n)]
    `(do (defn ~'count-with-instance-single-hashcode [~t1]
           (cond
             ~@(mapcat (fn [[l res]]
                         [`(instance? ~l ~t1)
                          res])
                       branches)
             :else ~default))
         (let [imp# impl-hashes]
           (defn ~'count-with-case-single-hashcode [~t1]
             (case (.hashCode (.getClass ~t1))
               ~@(mapcat (fn [[l res]]
                           [(.hashCode ^Object (resolve l))
                            res])
                         branches)
               ~default))))))

(defmacro gen-all [n]
  (let [n (if (symbol? n)
            (some-> n resolve deref)
            n)]
    (assert (integer? n) (pr-str n))
    `(do (gen-classes ~n)
         ;; expansion relies on the evaluation of gen-classes
         (multi-dispatch-tests ~n)
         (single-dispatch-tests ~n)
         (single-dispatch-hashcode-tests ~n))))

(def ngen-classes 98)

(gen-all ngen-classes)

(comment
(let [ctors (mapv #(resolve (symbol (str "->" cls-prefix %))) (range ngen-classes))
      [l r] [((rand-nth ctors)) ((rand-nth ctors))]]
  (assert (= (count-with-instance l r)
             (count-with-case l r))))
;;multiple dispatch
(let [ctors (mapv #(deref (resolve (symbol (str "->" cls-prefix %)))) (range ngen-classes))]
  (bench/quick-bench
    ;; 1 us
    (count-with-case ((rand-nth ctors)) ((rand-nth ctors)))))
(let [ctors (mapv #(deref (resolve (symbol (str "->" cls-prefix %)))) (range ngen-classes))]
  (bench/quick-bench
    ;; 622 ns
    (count-with-instance ((rand-nth ctors)) ((rand-nth ctors)))))

;;single dispatch
(let [ctors (mapv #(deref (resolve (symbol (str "->" cls-prefix %)))) (range ngen-classes))]
  (bench/quick-bench
    ;; 256 ns
    (count-with-instance-single ((rand-nth ctors)))))
(let [ctors (mapv #(deref (resolve (symbol (str "->" cls-prefix %)))) (range ngen-classes))]
  (bench/quick-bench
    ;; 275 ns
    (count-with-case-single ((rand-nth ctors)))))

;;single dispatch using hashcode
(let [ctors (mapv #(deref (resolve (symbol (str "->" cls-prefix %)))) (range ngen-classes))]
  (bench/quick-bench
    ;; 297 ns
    (count-with-instance-single-hashcode ((rand-nth ctors)))))
(let [ctors (mapv #(deref (resolve (symbol (str "->" cls-prefix %)))) (range ngen-classes))]
  (bench/quick-bench
    ;; 288 ns
    (count-with-case-single-hashcode ((rand-nth ctors)))))
)

;; YourKit on macos
; Ambroses-MBP clj.analyzer$ ./script/repl -A:perf
(comment
  (do
    (defn yk-cpu-profile-fn [mode f]
      (let [yk (com.yourkit.api.Controller.)]
        (.stopCPUProfiling yk)
        (.clearCPUData yk)
        (case mode
          :sample (.startCPUSampling yk nil)
          :trace (.startCPUTracing yk nil))
        (time (f))
        (.stopCPUProfiling yk)
        nil))
    (defmacro with-cpu-profiling [mode & body]
      `(yk-cpu-profile-fn ~mode (fn [] (do ~@body)))))


  ;; 1384 ms
  ;; 46% check-ns time spent in subtypeA* (control flow?)
  ;; 14% check-ns time spent in subtype/fail! (uninlined exceptional control flow)
  ;;
  ;; after removing fail!: 844ms (40% speedup)
  (with-cpu-profiling :trace
    (t/check-ns 'clojure.core.typed.test.mm
                :max-parallelism 1))

  (with-cpu-profiling :sample
    (t/check-ns2 'typed.cljc.checker.type-ctors))

  (with-cpu-profiling :trace
    (t/check-ns 'typed-test.lib.clojure.core.async.rps-async-test))

  (with-cpu-profiling :sample
    (t/load-if-needed))

  (with-cpu-profiling :trace
    (require 'typed.clj.checker.parse-unparse :reload))
  )

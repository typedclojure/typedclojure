(ns typed-test.clj.analyzer.perf
  (:refer-clojure :exclude [run!])
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as env]
            [typed.clj.analyzer :as jana2]
            [clojure.test :refer :all]
            [clojure.tools.analyzer.jvm :as taj]
            [criterium.core :as bench])
  (:import [typed.clj.analyzer UnanalyzedExpr]))

(defn run! [f coll]
  (clojure.core/run! (fn [e]
                       (f e)
                       nil)
                     coll))

(declare check-expr)

(defprotocol ICheck
  (-check [expr expected]))

(extend-protocol ICheck
  Object
  (-check [expr expected]
    (-> expr
        (ast/update-children #(check-expr % nil)))))

(defn check-expr
  ([expr] (check-expr expr nil))
  ([expr expected]
   (let [expr (assoc-in expr [:env :ns] (ns-name *ns*))]
     (assert (record? expr))
     (if (instance? UnanalyzedExpr expr)
       (let [{:keys [form env]} expr]
         (case (jana2/resolve-op-sym form env)
           (recur (ana2/analyze-outer expr) expected)))
       (-> expr
           ana2/run-pre-passes
           (-check expected)
           ana2/run-post-passes
           ana2/eval-top-level)))))

(defn check-top-level
  ([form] (check-top-level form nil {}))
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env] :as opts}]
   (let [env (or env (jana2/empty-env))]
     (with-bindings (jana2/default-thread-bindings env)
       (env/ensure (jana2/global-env)
         (-> form
             (ana2/unanalyzed-top-level env)
             (check-expr expected)))))))

(defn check-top-levels
  ([forms] (check-top-levels forms nil {}))
  ([forms expected] (check-top-levels forms expected {}))
  ([forms expected {:keys [env] :as opts}]
   (let [env (or env (jana2/empty-env))]
     (with-bindings (jana2/default-thread-bindings env)
       (env/ensure (jana2/global-env)
         (run! #(do
                  (-> %
                      (ana2/unanalyzed-top-level env)
                      (check-expr expected))
                  nil)
               forms))))))

(defmacro with-fresh-ns [f]
  `(binding [*ns* (create-ns (gensym '~'test-ns))]
     (refer-clojure)
     ~f))

(def forms1
  (repeat
    100
    `(for [a# ~(vec (repeat 10 1))] {:a #{[a#]}})))

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
      `(yk-cpu-profile-fn ~mode (fn [] ~@body))))

  (with-cpu-profiling :trace
    (with-fresh-ns
      (time
        (run! eval forms1))))
  ;; ~400ms

  (with-cpu-profiling :trace
    (with-fresh-ns
      (time
        (run! check-top-level forms1))))
  ;; ~1600ms

  (with-fresh-ns
    (with-cpu-profiling :trace
      (bench/bench
        (do
          (check-top-level (first forms1))
          nil))))
  (def foo1
    (bench/with-progress-reporting 
      (bench/quick-bench
        (check-top-level (first forms1))
        :verbose)))
  (def foo1
    (bench/with-progress-reporting 
      (bench/quick-bench
        (check-top-level `(+ 1 2))
        :verbose)))

    (bench/bench
      (with-fresh-ns
        (check-top-level (first forms1))))

  (time
    (with-fresh-ns
      (check-top-levels forms1)))
  ;; ~1600ms

  (time
    (with-fresh-ns
      (let [env (jana2/empty-env)]
        (with-bindings (jana2/default-thread-bindings env)
          (env/ensure (jana2/global-env)
                      (run! #(-> %
                                 (ana2/unanalyzed-top-level env)
                                 ana2/run-passes)
                            forms1))))))
  ;; 1500ms

  (with-fresh-ns
    (time
      (run! #(-> %
                 taj/analyze+eval)
            forms1)))
  ;; 1300ms

  (with-fresh-ns
    (do
      (time
        (taj/analyze+eval
          (first forms1)))
      nil))

  (do (require '[clojure.core.typed :as t])
      (with-cpu-profiling :trace

        (time
          (t/check-ns2 'typed.cljc.checker.type-ctors))))
  )


(defrecord Foo [a b c d e])

(defn ctor-update-a-b [^Foo m a b]
  (Foo. a b
        (.-c m)
        (.-d m)
        (.-e m)
        (.-__meta m)
        (.-__extmap m)))

(defn ctor-update-a-b-c-d [^Foo m a b c d]
  (Foo. a b c d
        (.-e m)
        (.-__meta m)
        (.-__extmap m)))

  (let [^Foo m (map->Foo {})]
    (Foo. 2 1
          (.-c m)
          (.-d m)
          (.-e m)
          (.-__meta m)
          (.-__extmap m)))

(defrecord Bar [a b c d e f g h i j k l m n o p q r s t u v w x y z])

(defn Bar-update-a [^Bar m a]
  (Bar. a
        (.-b m)
        (.-c m)
        (.-d m)
        (.-e m)
        (.-f m)
        (.-g m)
        (.-h m)
        (.-i m)
        (.-j m)
        (.-k m)
        (.-l m)
        (.-m m)
        (.-n m)
        (.-o m)
        (.-p m)
        (.-q m)
        (.-r m)
        (.-s m)
        (.-t m)
        (.-u m)
        (.-v m)
        (.-w m)
        (.-x m)
        (.-y m)
        (.-z m)
        (.-__meta m)
        (.-__extmap m)))

(defn Bar-update-z [^Bar m z]
  (Bar. (.-a m)
        (.-b m)
        (.-c m)
        (.-d m)
        (.-e m)
        (.-f m)
        (.-g m)
        (.-h m)
        (.-i m)
        (.-j m)
        (.-k m)
        (.-l m)
        (.-m m)
        (.-n m)
        (.-o m)
        (.-p m)
        (.-q m)
        (.-r m)
        (.-s m)
        (.-t m)
        (.-u m)
        (.-v m)
        (.-w m)
        (.-x m)
        (.-y m)
        z
        (.-__meta m)
        (.-__extmap m)))

(defn Bar-update-a-b-c-d-e-f [^Bar m a b c d e f]
  (Bar. a
        b
        c
        d
        e
        f
        (.-g m)
        (.-h m)
        (.-i m)
        (.-j m)
        (.-k m)
        (.-l m)
        (.-m m)
        (.-n m)
        (.-o m)
        (.-p m)
        (.-q m)
        (.-r m)
        (.-s m)
        (.-t m)
        (.-u m)
        (.-v m)
        (.-w m)
        (.-x m)
        (.-y m)
        (.-z m)
        (.-__meta m)
        (.-__extmap m)))

(set! *warn-on-reflection* true)

(comment
  (let [m (map->Foo {})]
    (bench/with-progress-reporting 
      (bench/bench
        (assoc m :a 2 :b 1)
        :verbose)))
  (let [^Foo m (map->Foo {})]
    (bench/with-progress-reporting 
      (bench/bench
        (Foo. 2 1
              (.-c m)
              (.-d m)
              (.-e m)
              (.-__meta m)
              (.-__extmap m))
        :verbose)))
  (let [^Foo m (map->Foo {})]
    (bench/with-progress-reporting 
      (bench/bench
        (ctor-update-a-b m 2 1)
        :verbose)))
  (let [^Foo m (map->Foo {})]
    (time
      (dotimes [_ 10000]
        (ctor-update-a-b m 2 1))))


  ;; # Foo bench (5 fields)
  (let [^Foo m (map->Foo {})]
    (bench/bench
      ;; 150 ns
      (assoc m :a 2 :b 1)))
  (let [^Foo m (map->Foo {})]
    (bench/bench
      ;; 393 ns
      (assoc m :a 2 :b 1 :c 5 :d 3)))
  (let [^Foo m (map->Foo {})
        u ctor-update-a-b]
    (bench/bench
      ;; 20 ns
      (u m 2 1)))
  (let [^Foo m (map->Foo {})
        u ctor-update-a-b-c-d]
    (bench/bench
      ;; 30 ns
      (u m 2 1 6 7)))

  ;; # Bar bench (26 fields)
  (let [m (zipmap (map char (range (int \a) (int \z))) (repeat nil))]
    (bench/bench
      ;; 138 ns
      (assoc m :a 2)))
  (let [m (zipmap (map char (range (int \a) (int \z))) (repeat nil))]
    (bench/bench
      ;; 153 ns
      (assoc m :z 2)))
  (let [m (zipmap (map char (range (int \a) (int \z))) (repeat nil))]
    (bench/bench
      ;; 1.690321 µs
      (assoc m :a 2 :b 5 :c 3 :d 5 :e 1 :f 2)))
  (let [m (map->Bar {})]
    (bench/bench
      ;; 143 ns
      (assoc m :a 2)))
  (let [m (map->Bar {})]
    (bench/bench
      ;; 164 ns
      (assoc m :z 2)))
  (let [m (map->Bar {})]
    (bench/bench
      ;; 1.384386 µs
      (assoc m :a 2 :b 5 :c 3 :d 5 :e 1 :f 2)))
  (let [m (map->Bar {})
        u Bar-update-a]
    (bench/bench
      ;; 129 ns
      (u m 2)))
  (let [m (map->Bar {})
        u Bar-update-a-b-c-d-e-f]
    (bench/bench
      ;; 143 ns
      (u m 2 5 3 5 1 2)))

  (let [^Foo m (map->Foo {})]
    (Foo. 2 1
          (.-c m)
          (.-d m)
          (.-e m)
          (.-__meta m)
          (.-__extmap m)))
  (let [^Foo m (map->Foo {})]
    (bench/with-progress-reporting 
      (bench/bench
        (->Foo 1 2
               (.-c m)
               (.-d m)
               (.-e m))
        :verbose)))
  (let [m (zipmap [:a :b :c :d :e] (repeat nil))]
    (bench/with-progress-reporting 
      (bench/quick-bench
        (assoc m :a 1 :b 2)
        :verbose)))
)

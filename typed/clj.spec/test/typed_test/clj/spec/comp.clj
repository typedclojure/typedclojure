(ns ^:typed.clojure typed-test.clj.spec.comp
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

; best-effort attempt to get this to work without type variables
(s/def ::comp-fspec-fn
  (s/fspec :args (s/cat :f (s/fspec :args (s/cat :x any?)
                                    :ret any?)
                        :g (s/fspec :args (s/cat :x any?)
                                    :ret any?))
           :fn (fn [{{:keys [f g]} :args, :keys [ret]}]
                 (-> (gen/for-all* [(gen/any)]
                                   #(= (ret %)
                                       (f (g %))))
                     gen/quick-check
                     :pass?))
           :ret (s/fspec :args (s/cat :x any?)
                         :ret any?)))

;doesn't work at all
(deftest comp-fspec-fn-test
  (tu/is-invalid ::comp-fspec-fn comp)
  (tu/is-invalid ::comp-fspec-fn (fn [f g] #(f (g %)))))

; simulate the gensym strategy
(s/def ::comp-fspec-fn-gensym
  (s/fspec :args (s/cat :f (s/fspec :args (s/cat :x #{G2})
                                    :ret #{G3})
                        :g (s/fspec :args (s/cat :x #{G1})
                                    :ret #{G2}))
           :ret (s/fspec :args (s/cat :x #{G1})
                         :ret #{G3})))

; works but tests exactly one case
(deftest comp-fspec-fn-test
  (tu/is-valid ::comp-fspec-fn-gensym comp)
  (tu/is-valid ::comp-fspec-fn-gensym (fn [f g]
                                        #(f (g %))))
  (tu/is-invalid ::comp-fspec-fn-gensym (fn [f g] #(g (f %)))))

(s/def ::comp2
  (t/all :binder (t/binder
                   :a (t/bind-tv)
                   :b (t/bind-tv)
                   :c (t/bind-tv))
         :body
         (s/fspec :args (s/cat :f (s/fspec :args (s/cat :b (t/tv :b))
                                           :ret (t/tv :c))
                               :g (s/fspec :args (s/cat :a (t/tv :a))
                                           :ret (t/tv :b)))
                  :ret (s/fspec :args (s/cat :a (t/tv :a))
                                :ret (t/tv :c)))))

(deftest comp2-test
  (tu/is-valid ::comp2 comp)
  (tu/is-valid ::comp2 (fn [f g] #(f (g %))))
  (tu/is-invalid ::comp2 (fn [f g] #(g (f %)))))

(s/def ::comp2-varargs
  (t/all :binder (t/binder
                   :a (t/bind-tv :kind (s/* (t/binder
                                              :a (t/bind-tv))))
                   :b (t/bind-tv)
                   :c (t/bind-tv))
         :body
         (s/fspec :args (s/cat :f (s/fspec :args (s/cat :b (t/tv :b))
                                           :ret (t/tv :c))
                               :g (s/fspec :args (s/cat :as (t/fold-binders (t/tv :a) :a))
                                           :ret (t/tv :b)))
                  :ret (s/fspec :args (s/cat :as (t/fold-binders (t/tv :a) :a))
                                :ret (t/tv :c)))))

(deftest comp2-varargs-test
  (is (s/describe (t/inst ::comp2-varargs {:a []})))
  (tu/is-valid ::comp2-varargs comp)
  (tu/is-valid (t/inst ::comp2-varargs {:a []}) comp)
  (tu/is-valid (t/inst ::comp2-varargs {:a [{:a integer?}]}) comp)
  (tu/is-valid (t/inst ::comp2-varargs {:a [{:a integer?} {:a boolean?}]}) comp)
  (tu/is-valid ::comp2-varargs (fn [f g]
                                 (fn [& args]
                                   #_(prn "args" args)
                                   (f (apply g args))))))

(s/def ::comp0
  (t/all :binder (t/binder
                   :a (t/bind-tv))
         :body
         (s/fspec :args (s/cat)
                  :ret (s/fspec :args (s/cat :a (t/tv :a))
                                :ret (t/tv :a)))))

(deftest comp0-test
  (tu/is-valid ::comp0 comp)
  (tu/is-valid ::comp0 (fn []
                         (fn [x]
                           #_(prn "x" x)
                           x))))

(s/def ::comp3-varargs
  (t/all :binder (t/binder
                   :a (t/bind-tv :kind (s/* (t/binder
                                              :a (t/bind-tv))))
                   :b (t/bind-tv)
                   :c (t/bind-tv)
                   :d (t/bind-tv))
         :body
         (s/fspec :args (s/cat :f2 (s/fspec :args (s/cat :c (t/tv :c))
                                            :ret (t/tv :d))
                               :f1 (s/fspec :args (s/cat :b (t/tv :b))
                                            :ret (t/tv :c))
                               :f0 (s/fspec :args (s/cat :as (t/fold-binders (t/tv :a) :a))
                                            :ret (t/tv :b)))
                  :ret (s/fspec :args (s/cat :as (t/fold-binders (t/tv :a) :a))
                                :ret (t/tv :d)))))

(deftest comp3-varargs-test
  (tu/is-valid ::comp3-varargs comp)
  (tu/is-valid ::comp3-varargs (fn [f2 f1 f0]
                                 (fn [& args]
                                   #_(prn "args" args)
                                   (f2 (f1 (apply f0 args)))))))

(defn comp-fs-wrap [bs]
  `(s/cat
     ~@(apply
         concat
         (rseq
           (reduce
             (fn [cats [n b0 b1]]
               (conj
                 cats
                 [(keyword (str "f" n))
                  `(s/fspec :args (s/cat ~(keyword (str "b" n))
                                         ~b0)
                            :ret ~b1)]))
             []
             (map vector
                  (range)
                  bs
                  (next bs)))))))

(deftest comp-fs-wrap-test
  (is (= `(s/cat)
         (comp-fs-wrap '[])))
  (is (= `(s/cat
            :f3
            (s/fspec :args (s/cat :b3 3) :ret 4)
            :f2
            (s/fspec :args (s/cat :b2 2) :ret 3)
            :f1
            (s/fspec :args (s/cat :b1 1) :ret 2)
            :f0
            (s/fspec :args (s/cat :b0 0) :ret 1))
         (comp-fs-wrap (range 5)))))

(s/def ::compN-varargs
  (t/all :binder (t/binder
                   :x (t/bind-tv)
                   :a (t/bind-tv :kind (s/* (t/binder
                                              :a (t/bind-tv))))
                   :b (t/bind-tv :kind (s/+ (t/binder
                                              :b (t/bind-tv)))))
         :body
         (t/fcase
           0 (s/fspec :args (s/cat)
                      :ret (s/fspec :args (s/cat :x (t/tv :x))
                                    :ret (t/tv :x)))
           (s/fspec :args (s/cat :fs (t/fold-binders
                                       (t/tv :b) :b
                                       :wrap comp-fs-wrap)
                                 :g (s/fspec :args (s/cat :as (t/fold-binders (t/tv :a) :a))
                                             :ret (t/fold-binders (t/tv :b) :b
                                                                  :wrap first)))
                    :ret (s/fspec :args (s/cat :as (t/fold-binders (t/tv :a) :a))
                                  :ret (t/fold-binders (t/tv :b) :b
                                                       :wrap peek))))))

(deftest compN-varargs-test
  (testing "comp 0-arity"
    (tu/is-invalid
      ::compN-varargs
      (fn
        ([] nil)
        ([& args] (apply comp args)))))
  (tu/is-valid ::compN-varargs comp)
  (tu/is-invalid ::compN-varargs (fn [& fs]
                                   #_(prn "fs" fs)
                                   ))
  (tu/is-invalid ::compN-varargs (fn [f3 f2 f1 f0]
                                   (fn [& args]
                                     #_(prn "args" args)
                                     (f3 (f2 (f1 (apply f0 args))))))))

(comment
(gen/generate
  (s/gen
    (s/cat)))
(gen/generate
  (s/gen
    (s/cat :fs (s/cat))))
(
(gen/generate
  (s/gen
    (s/fspec :args (s/cat :fs (s/cat :f integer?))
             :ret integer?)))
1)
((gen/generate
  (s/gen
    (s/fspec :args (s/cat :fs (s/cat))
             :ret integer?))))
(gen/generate
  (s/gen
    (s/fspec :args (s/cat :f integer?)
             :ret integer?)))
)

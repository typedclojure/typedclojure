(ns typed-test.clj.spec.comp
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [clojure.test :refer :all]))

(s/def ::comp2
  (all :binder (binder
                 :a (tvar-spec)
                 :b (tvar-spec)
                 :c (tvar-spec))
       :body
       (s/fspec :args (s/cat :f (s/fspec :args (s/cat :b (tvar :b))
                                         :ret (tvar :c))
                             :g (s/fspec :args (s/cat :a (tvar :a))
                                         :ret (tvar :b)))
                :ret (s/fspec :args (s/cat :a (tvar :a))
                              :ret (tvar :c)))))

(deftest comp2-test
  (is (s/valid? ::comp2 comp))
  (is (s/valid? ::comp2 (fn [f g] #(f (g %)))))
  (is (not (s/valid? ::comp2 (fn [f g] #(g (f %)))))))

(s/def ::comp2-varargs
  (all :binder (binder
                 :a (tvar-spec :kind (s/* (binder
                                            :a (tvar-spec))))
                 :b (tvar-spec)
                 :c (tvar-spec))
       :body
       (s/fspec :args (s/cat :f (s/fspec :args (s/cat :b (tvar :b))
                                         :ret (tvar :c))
                             :g (s/fspec :args (s/cat :as (dotted-pretype (tvar :a) :a))
                                         :ret (tvar :b)))
                :ret (s/fspec :args (s/cat :as (dotted-pretype (tvar :a) :a))
                              :ret (tvar :c)))))

(deftest comp2-varargs-test
  (is (s/valid? ::comp2-varargs comp))
  (is (s/valid? ::comp2-varargs (fn [f g]
                                  (fn [& args]
                                    #_(prn "args" args)
                                    (f (apply g args)))))))

(s/def ::comp0
  (all :binder (binder
                 :a (tvar-spec))
       :body
       (s/fspec :args (s/cat)
                :ret (s/fspec :args (s/cat :a (tvar :a))
                              :ret (tvar :a)))))

(deftest comp0-test
  (is (s/valid? ::comp0 comp))
  (is (s/valid? ::comp0 (fn []
                          (fn [x]
                            #_(prn "x" x)
                            x)))))

(s/def ::comp3-varargs
  (all :binder (binder
                 :a (tvar-spec :kind (s/* (binder
                                            :a (tvar-spec))))
                 :b (tvar-spec)
                 :c (tvar-spec)
                 :d (tvar-spec))
       :body
       (s/fspec :args (s/cat :f2 (s/fspec :args (s/cat :c (tvar :c))
                                          :ret (tvar :d))
                             :f1 (s/fspec :args (s/cat :b (tvar :b))
                                          :ret (tvar :c))
                             :f0 (s/fspec :args (s/cat :as (dotted-pretype (tvar :a) :a))
                                         :ret (tvar :b)))
                :ret (s/fspec :args (s/cat :as (dotted-pretype (tvar :a) :a))
                              :ret (tvar :d)))))

(deftest comp3-varargs-test
  (is (s/valid? ::comp3-varargs comp))
  (is (s/valid? ::comp3-varargs (fn [f2 f1 f0]
                                  (fn [& args]
                                    #_(prn "args" args)
                                    (f2 (f1 (apply f0 args))))))))

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

; doesn't handle (comp) (but handles all other args!)
(s/def ::compN-varargs
  (all :binder (binder
                 :a (tvar-spec :kind (s/* (binder
                                            :a (tvar-spec))))
                 :b (tvar-spec :kind (s/+ (binder
                                            :b (tvar-spec)))))
       :body
       (s/fspec :args (s/cat :fs (dotted-pretype
                                   (tvar :b) :b
                                   :wrap comp-fs-wrap)
                             :g (s/fspec :args (s/cat :as (dotted-pretype (tvar :a) :a))
                                         :ret (dotted-pretype (tvar :b) :b
                                                              :wrap first)))
                :ret (s/fspec :args (s/cat :as (dotted-pretype (tvar :a) :a))
                              :ret (dotted-pretype (tvar :b) :b
                                                   :wrap peek)))))

(deftest compN-varargs-test
  (is (s/valid? ::compN-varargs comp))
  (is (not (s/valid? ::compN-varargs (fn [& fs]
                                       #_(prn "fs" fs)
                                       ))))
  (is (not (s/valid? ::compN-varargs (fn [f3 f2 f1 f0]
                                       (fn [& args]
                                         #_(prn "args" args)
                                         (f3 (f2 (f1 (apply f0 args))))))))))

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

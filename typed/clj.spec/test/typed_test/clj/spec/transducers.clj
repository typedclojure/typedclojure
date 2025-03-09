(ns ^:typed.clojure typed-test.clj.spec.transducers
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

(s/def
  ::Reducer
  (t/tfn :binder
         (t/binder :acc (t/bind-tv :position #{:input})
                   :x (t/bind-tv))
         :body
         (fcase
           ;init (0 arg) & complete (1 arg)
           #{0 1}
           (s/fspec
             :args (s/cat :x (s/? (t/tv :x)))
             :ret (t/tv :x))
           ;step
           (s/fspec
             :args (s/cat :x (t/tv :x)
                          :acc (t/tv :acc))
             :ret (s/or :reduced (t/reduced-of (t/tv :x))
                        :x (t/tv :x))))))

(deftest Reducer-test
  (tu/is-valid (t/tapp ::Reducer {:acc integer?
                                  :x integer?})
               (fn
                 ([] 0)
                 ([x] x)
                 ([x acc] (+ x acc))))
  (tu/is-invalid (t/tapp ::Reducer {:acc integer?
                                    :x integer?})
                 (fn
                   ([] 'a)
                   ([x] x)
                   ([x acc] (+ x acc))))
  (tu/is-invalid (t/tapp ::Reducer {:acc integer?
                                    :x integer?})
                 (fn
                   ([] 0)
                   ([x] (gensym))
                   ([x acc] (+ x acc))))
  (tu/is-invalid (t/tapp ::Reducer {:acc integer?
                                    :x integer?})
                 (fn
                   ([] 0)
                   ([x] x)
                   ([x acc]
                    (prn "x acc" x acc)
                    (gensym))))
  (tu/is-invalid (t/tapp ::Reducer {:acc integer?
                                    :x integer?})
                 1)
  ;FIXME flaky
  #_
  (is (every? integer?
              ((juxt #(%) #(% 1) #(% 1 2))
               (gen/generate
                 (s/gen
                   (t/tapp ::Reducer {:acc integer?
                                      :x integer?}))))))
  (is (thrown? AssertionError
               ((gen/generate
                  (s/gen
                    (t/tapp ::Reducer {:acc integer?
                                       :x integer?})))
                1 2 3 4))))

(s/def
  ::Transducer
  (t/tfn :binder
         (t/binder :in (t/bind-tv :position #{:input})
                   :out (t/bind-tv :position #{:output}))
         :body
         (all :binder
              (t/binder :r (t/bind-tv))
              :body
              (s/fspec :args (s/cat :reducer (t/tapp ::Reducer {:acc (t/tv :out)
                                                                :x (t/tv :r)}))
                       :ret (t/tapp ::Reducer {:acc (t/tv :in)
                                               :x (t/tv :r)})))))

;very slow
(deftest Transducer-test
  (binding [s/*fspec-iterations* 5]
    (tu/is-valid (t/tapp ::Transducer {:in integer?
                                       :out integer?})
                 (map inc)))
  (binding [s/*fspec-iterations* 5]
    (tu/is-valid (t/tapp ::Transducer {:in integer?
                                       :out integer?})
                 (fn [reducer]
                   (fn [& args]
                     #_(prn "args" args)
                     (apply reducer args)))))
  (binding [s/*fspec-iterations* 5]
    (tu/is-valid (t/tapp ::Transducer {:in integer?
                                       :out integer?})
                 identity))
  (tu/is-invalid (t/tapp ::Transducer {:in integer?
                                       :out symbol?})
                 identity)
  (tu/is-invalid (t/tapp ::Transducer {:in integer?
                                       :out symbol?})
                 (map str))
  (binding [s/*fspec-iterations* 5]
    (tu/is-valid (t/tapp ::Transducer {:in integer?
                                       :out symbol?})
                 (map (fn [i] (symbol (str i))))))
  ;TODO
  #_
  (is (every? symbol?
              (into []
                    (gen/generate
                      (s/gen (t/inst
                               (t/tapp ::Transducer {:in integer?
                                                     :out symbol?})
                               {:r integer?})))
                    [1 2 3 4])))
  )

(ns ^:typed.clojure clojure.core.typed.test.multimethod-trace
  "Tracing multimethod checking procedure to understand its inference."
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err is-tc-err-messages]]))

;; Here we simulate how the checker infers the parameter types of the defmethod.
;;   (defmulti foo (fn [a b] [(:op a) (:op b)]))
;;   (defmethod foo [:test1 :test2] [a b] ...)
;;
;; Internally, the type checker:
;; 1. Type-checks the dispatch function to get its return type, in this case a HVec with
;;    embedded symbolic objects for each entry.
;; 2. When checking the defmethod body, simulates (isa? dispatch-result [:test1 :test2]),
;;    by extracting the embedded objects from dispatch-result's type and simulating
;;    (and (= (:op a) :test1) (= (:op b) :test2))
;; 3. Uses the positive branch of isa? to refine parameter types in the defmethod body

(deftest simulate-mm-multi-dispatch-test
  (is-tc-e (t/fn [a :- '{:op (t/U ':test1 ':test2)}
                  b :- '{:op (t/U ':test1 ':test2)}]
             (if (isa? [(:op a) (:op b)] [:test1 :test2])
               (do
                 ;; In the positive branch, isa? should have refined a and b
                 (t/ann-form a '{:op ':test1})
                 (t/ann-form b '{:op ':test2})
                 :then)
               :else)))
  ;; can let-bind the dispatch value (important to check because of the alias env)
  (is-tc-e (t/fn [a :- '{:op (t/U ':test1 ':test2)}
                  b :- '{:op (t/U ':test1 ':test2)}]
             (let [dispatch-result [(:op a) (:op b)]]
               (if (isa? dispatch-result [:test1 :test2])
                 (do
                   ;; In the positive branch, isa? should have refined a and b
                   (t/ann-form a '{:op ':test1})
                   (t/ann-form b '{:op ':test2})
                   :then)
                 :else))))
  ;; ensure then branch is actually checked
  (is (= '{:type-errors
           [{:type-error :typed.clojure/type-mismatch-error,
             :form a,
             :data {:expected-type (t/HMap :mandatory {:op (t/Val :test2)}),
                    :actual-type (t/HMap :mandatory {:op (t/Val :test1)})}}
            {:type-error :typed.clojure/type-mismatch-error,
             :form b,
             :data {:expected-type (t/HMap :mandatory {:op (t/Val :test1)}),
                    :actual-type (t/HMap :mandatory {:op (t/Val :test2)})}}
            {:type-error :typed.clojure/type-mismatch-error,
             :form :then,
             :data {:expected-type (t/Val :else),
                    :actual-type (t/Val :then)}}]}
         (-> (is-tc-err (t/fn [a :- '{:op (t/U ':test1 ':test2)}
                               b :- '{:op (t/U ':test1 ':test2)}]
                          ;; type mismatch only if then branch is reachable
                          :- ':else
                          (if (isa? [(:op a) (:op b)] [:test1 :test2])
                            (do
                              ;; type mismatch by swapping the expected types
                              (t/ann-form a '{:op ':test2})
                              (t/ann-form b '{:op ':test1})
                              :then)
                            :else)))
             (update :type-errors #(mapv (fn [m] (dissoc m :message :env)) %)))))
  ;; ensure else branch is not falsly updated
  (is (= '{:type-errors
           [{:type-error :typed.clojure/type-mismatch-error,
             :form a,
             :data {:expected-type (t/HMap :mandatory {:op (t/Val :test1)}),
                    :actual-type   (t/HMap :mandatory {:op (t/U (t/Val :test1) (t/Val :test2))})}}
            {:type-error :typed.clojure/type-mismatch-error,
             :form a,
             :data {:expected-type (t/HMap :mandatory {:op (t/Val :test2)}),
                    :actual-type   (t/HMap :mandatory {:op (t/U (t/Val :test1) (t/Val :test2))})}}
            {:type-error :typed.clojure/type-mismatch-error,
             :form b,
             :data {:expected-type (t/HMap :mandatory {:op (t/Val :test1)}),
                    :actual-type   (t/HMap :mandatory {:op (t/U (t/Val :test1) (t/Val :test2))})}}
            {:type-error :typed.clojure/type-mismatch-error,
             :form b,
             :data {:expected-type (t/HMap :mandatory {:op (t/Val :test2)}),
                    :actual-type   (t/HMap :mandatory {:op (t/U (t/Val :test1) (t/Val :test2))})}}]}
         (-> (is-tc-err-messages
               (t/fn [a :- '{:op (t/U ':test1 ':test2)}
                      b :- '{:op (t/U ':test1 ':test2)}]
                 (let [dispatch-result [(:op a) (:op b)]]
                   (if (isa? dispatch-result [:test1 :test2])
                     nil
                     ;; The negative branch should have no refinements
                     (do (t/ann-form a '{:op ':test1})
                         (t/ann-form a '{:op ':test2})
                         (t/ann-form b '{:op ':test1})
                         (t/ann-form b '{:op ':test2}))))))
             (update :type-errors #(mapv (fn [m] (dissoc m :message :env)) %))))))

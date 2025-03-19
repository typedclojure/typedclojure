(ns ^:typed.clojure typed-test.clj.ext.clojure.core
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.template :refer [do-template]]
            [clojure.core.typed :as t]
            [typed.clojure :as tc]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.ext.clojure.core__let :as ext-let]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljs.checker.test-utils :as cljs]))

(defn eval-in-ns [form]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (refer-clojure)
    (form)))

(deftest ns-test
  ; type checked
  (let [form `(ns ~'foo)
        expected nil
        res (binding [*ns* *ns*]
              (t/check-form-info form
                                 :expected expected
                                 :type-provided? true))]
    (is (-> res :type-errors empty?))
    (is (not (:ex res))))
  ; type error
  (let [form `(ns ~'foo)
        expected `t/Str
        res (binding [*ns* *ns*]
              (t/check-form-info form
                                 :expected expected
                                 :type-provided? true))]
    (is (= form (-> res :type-errors first :form))
        res))
  ; eval
  (binding [*ns* *ns*]
    (let [form `(ns ~'foo)
          res (t/check-form-info form)
          _ (is (= *ns* (find-ns 'foo)))]
      (is
        (-> res
            (find :result)
            #{[:result nil]})))))

(deftest defmacro-test
  ; type checked
  (let [form `(defmacro ~'foo [] 1)
        res (eval-in-ns
              #(t/check-form-info form))]
    (is (-> res :type-errors empty?))
    (is (not (:ex res))))
  ; type error
  (let [form `(defmacro ~'foo [] 1)
        expected `t/Str
        res (eval-in-ns
              #(t/check-form-info form
                                  :expected expected
                                  :type-provided? true))]
    (is (= form (-> res :type-errors first :form))
        res))
  ; eval
  (eval-in-ns
    (fn []
      (let [form `(do (defmacro ~'foo [] 1)
                      (~'foo))
            res (t/check-form-info form)]
        (is
          (= [:result 1]
             (-> res
                 (find :result)))))))
  (is-tc-e (defmacro foo [a] (inc a)))
  ;;FIXME how should defmacro handled in cljs?
  #_
  (cljs/is-tc-e (defmacro foo [a] (inc a))))

;; tests for clojure.core.typed macros in typed-test.clj.ext.clojure.core.typed
(deftest vector-destructure-error-msg-test
  (testing "let"
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 15, :file "core.clj"},
               :form (clojure.core/let [[a] #{1}] a),
               :data nil,
               :message
               "The type `(t/HSet #{1})` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             #(let [[a] #{1}]
                a))))
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 15, :file "core.clj"},
               :form (clojure.core/let [{[a] :foo} {:foo #{1}}] a),
               :data nil,
               :message
               "The type `(t/HSet #{1})` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             #(let [{[a] :foo} {:foo #{1}}]
                a)))))
  (testing "for"
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 15, :file "core.clj"},
               :form (cc/for [[a] [#{1}]] a),
               :data nil,
               :message
               "The type `(t/HSet #{1})` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             #(cc/for [[a] [#{1}]]
                a))))
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 15, :file "core.clj"},
               :form (cc/for [{[a] :foo} [{:foo #{1}}]] a),
               :data nil,
               :message
               "The type `(t/HSet #{1})` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             #(cc/for [{[a] :foo} [{:foo #{1}}]]
                a)))))
  (testing "doseq"
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 15, :file "core.clj"},
               :form (cc/doseq [[a] [#{1}]] a),
               :data nil,
               :message
               "The type `(t/HSet #{1})` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             #(cc/doseq [[a] [#{1}]]
                a))))
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 15, :file "core.clj"},
               :form (cc/doseq [{[a] :foo} [{:foo #{1}}]] a),
               :data nil,
               :message
               "The type `(t/HSet #{1})` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             #(cc/doseq [{[a] :foo} [{:foo #{1}}]]
                a)))))
  (testing "fn"
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 24, :file "core.clj"},
               :form (cc/fn [[a]]),
               :data nil,
               :message
               "The type `(IPersistentSet t/Any)` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             (ann-form (cc/fn [[a]])
                       [(t/Set t/Any) -> t/Any])))))
  (testing "defn"
    (is (= '{:type-errors
             [{:type-error :clojure.core.typed.errors/type-error,
               :env {:line "REMOVED_LINE", :column 18, :file "core.clj"},
               :form (cc/defn foo [[a]]),
               :data nil,
               :message
               "The type `(IPersistentSet t/Any)` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             (do (t/ann foo [(t/Set t/Any) :-> t/Any])
                 (cc/defn foo [[a]]))))))
  (testing "defmethod"
    (is (= '{:type-errors
            [{:type-error :clojure.core.typed.errors/type-error,
              :env {:line "REMOVED_LINE", :column 18, :file "core.clj"},
              :form (defmethod f nil [[a]]),
              :data nil,
              :message
              "The type `(IPersistentSet t/Any)` cannot be destructured via syntax `[a]` because the type cannot be passed as the first argument of `nth`.`"}]}
           (is-tc-err-messages
             (do (t/ann f [(t/Set t/Any) -> t/Any])
                 (defmulti f (fn [_] nil))
                 (defmethod f nil [[a]])))))))

(deftest inlined-nth-error-msg-test
  (is (= '{:type-errors
           [{:type-error :typed.clojure/app-type-error,
             :env {:line "REMOVED_LINE", :column 31, :file "core.clj"},
             :form (clojure.core/nth :a :a),
             :data
             {:fn-type
              (t/All [x y] (t/IFn [(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x)) t/AnyInteger :-> x] [(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x) nil) t/AnyInteger y :-> (t/U x y)] [(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x) nil) t/AnyInteger :-> (t/U x nil)])),
              :args-results
              [{:type (t/Val :a), :filter-set {:then tt, :else ff}}
               {:type (t/Val :a), :filter-set {:then tt, :else ff}}],
              :expected-result {:type t/Infer}},
             :message
             "Polymorphic function clojure.core/nth could not be applied to arguments:\nPolymorphic Variables:\n\tx\n\ty\n\nDomains:\n\t(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x) nil) t/AnyInteger\n\nArguments:\n\t(t/Val :a) (t/Val :a)\n\nRanges:\n\t(t/U x nil)\n\n"}]}
         (is-tc-err-messages #(clojure.core/nth :a :a))))
  (is (= '{:ex
           {:message
            "class clojure.lang.Keyword cannot be cast to class java.lang.Number (clojure.lang.Keyword is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')"},
           :type-errors
           [{:type-error :typed.clojure/app-type-error,
             :env {:line "REMOVED_LINE", :column 16, :file "core.clj"},
             :form (cc/nth (and :a :b) :a),
             :data
             {:fn-type
              (t/All [x y] (t/IFn [(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x)) t/AnyInteger :-> x] [(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x) nil) t/AnyInteger y :-> (t/U x y)] [(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x) nil) t/AnyInteger :-> (t/U x nil)])),
              :args-results
              [{:type (t/Val :b), :filter-set {:then tt, :else ff}}
               {:type (t/Val :a), :filter-set {:then tt, :else ff}}]},
             :message
             "Polymorphic function cc/nth could not be applied to arguments:\nPolymorphic Variables:\n\tx\n\ty\n\nDomains:\n\t(t/U (t/I (Seqable (t/NilableNonEmptySeq x)) Sequential) (t/I Sequential nil) (Indexed x) nil) t/AnyInteger\n\nArguments:\n\t(t/Val :b) (t/Val :a)\n\nRanges:\n\t(t/U x nil)\n\n"}]}
         (is-tc-err-messages
           (do (alias 'cc 'clojure.core)
               (cc/nth (and :a :b) :a))))))

;; note: the following fns intentionally omitted as they are annotated to accept all arguments
;; - clojure.core/get 
(deftest misc-inline-error-msg-test
  (testing "annotated inlined fns"
    (do-template [FORM] (testing 'FORM
                          (let [res (is-tc-err-messages
                                      (do (alias 'cc 'clojure.core)
                                          (fn [] FORM)))]
                            (is (= 'FORM
                                   (-> res
                                       :type-errors
                                       first
                                       :form))
                                (pr-str res))))
                 (cc/zero? :a)
                 (cc/count :a)
                 (cc/int :a)
                 (cc/< :a :a)
                 (cc/inc :a)
                 (cc/inc' :a)
                 (cc/+ :a :a)
                 (cc/+' :a :a)
                 (cc/+ :a :a :a :a)
                 (cc/+' :a :a :a :a)
                 (cc/* :a :a)
                 (cc/*' :a :a)
                 (cc/* :a :a :a :a)
                 (cc/*' :a :a :a :a)
                 (cc// :a :a)
                 (cc// :a :a :a :a)
                 (cc/- :a :a)
                 (cc/-' :a :a)
                 (cc/- :a :a :a :a)
                 (cc/-' :a :a :a :a)
                 (cc/<= :a :a)
                 (cc/> :a :a)
                 (cc/>= :a :a)
                 (cc/== :a :a)
                 (cc/max :a :a)
                 (cc/max :a :a :a :a)
                 (cc/min :a :a)
                 (cc/min :a :a :a :a)
                 (cc/dec :a)
                 (cc/dec' :a)
                 (cc/unchecked-inc-int :a)
                 (cc/unchecked-inc :a)
                 (cc/unchecked-dec-int :a)
                 (cc/unchecked-dec :a)
                 (cc/unchecked-negate-int :a)
                 (cc/unchecked-negate :a)
                 (cc/unchecked-add-int :a :a)
                 (cc/unchecked-add :a :a)
                 (cc/unchecked-subtract-int :a :a)
                 (cc/unchecked-subtract :a :a)
                 (cc/unchecked-multiply-int :a :a)
                 (cc/unchecked-multiply :a :a)
                 (cc/unchecked-divide-int :a :a)
                 (cc/unchecked-remainder-int :a :a)
                 (cc/pos? :a)
                 (cc/neg? :a)
                 (cc/quot :a :a)
                 (cc/rem :a :a)
                 (cc/bit-not :a)
                 (cc/bit-and :a :a)
                 (cc/bit-and :a :a :a :a)
                 (cc/bit-or :a :a)
                 (cc/bit-or :a :a :a :a)
                 (cc/bit-xor :a :a)
                 (cc/bit-xor :a :a :a :a)
                 (cc/bit-and-not :a :a)
                 (cc/bit-and-not :a :a :a :a)
                 (cc/bit-shift-left :a :a)
                 (cc/bit-shift-right :a :a)
                 (cc/unsigned-bit-shift-right :a :a)
                 (cc/num :a)
                 (cc/long :a)
                 (cc/double :a)
                 (cc/char :a)
                 (cc/alength :a)
                 (cc/aclone :a)
                 (cc/aget :a :a)
                 (cc/aset :a :a)
                 (cc/byte-array :a)
                 (cc/byte-array :a :a)
                 (cc/char-array :a)
                 (cc/char-array :a :a)
                 (cc/short-array :a)
                 (cc/short-array :a :a)
                 (cc/double-array :a)
                 (cc/double-array :a :a)
                 (cc/int-array :a)
                 (cc/int-array :a :a)
                 (cc/boolean-array :a)
                 (cc/boolean-array :a :a)))
  (testing "unannotated inlined fns"
    (do-template [FORM] (testing 'FORM
                          (let [res (is-tc-err-messages
                                      (do (alias 'cc 'clojure.core)
                                          (fn [] FORM)))]
                            (is (= (first 'FORM)
                                   (-> res
                                       :type-errors
                                       first
                                       :form))
                                (pr-str res))))
                 (cc/unchecked-char :a)
                 (cc/unchecked-short :a)
                 (cc/unchecked-double :a)
                 (cc/unchecked-byte :a)
                 (cc/unchecked-long :a)
                 (cc/unchecked-float :a)
                 (cc/unchecked-int :a)
                 (cc/float-array :a)
                 (cc/float-array :a :a)
                 (cc/object-array :a)
                 (cc/object-array :a :a)
                 (cc/long-array :a)
                 (cc/long-array :a :a)
                 (cc/booleans :a)
                 (cc/bytes :a)
                 (cc/chars :a)
                 (cc/shorts :a)
                 (cc/floats :a)
                 (cc/ints :a)
                 (cc/doubles :a)
                 (cc/longs :a))))

(deftest defprotocol-test
  (is-tc-e (cc/defprotocol A))
  (is-tc-err (t/ann-form (cc/defprotocol A) t/Int))
  (cljs/is-tc-e (cc/defprotocol A))
  (cljs/is-tc-err (t/ann-form (cc/defprotocol A) t/Int))
  (cljs/is-tc-e (clojure.core/defprotocol A))
  (cljs/is-tc-err (t/ann-form (clojure.core/defprotocol A) t/Int))
  (cljs/is-tc-e (cljs.core/defprotocol A))
  (cljs/is-tc-err (t/ann-form (cljs.core/defprotocol A) t/Int)))

(deftest defn-test
  (is-tc-e (cc/defn -registry {:arglists '([] [{:keys [registry]}])}
             ([] nil)
             ([opts] nil)))
  (is-tc-e (cc/defn -registry
             "foo"
             {:arglists '([] [{:keys [registry]}])}
             ([] nil)
             ([opts] nil))))

(deftest fn-test
  (is-tc-e (cc/fn [^{::tc/- t/Int} a] (inc a)))
  (is-tc-e (cc/fn ([^{::tc/- t/Int} a] (inc a))))
  (is-tc-e (cc/fn ^{::tc/- t/Int} [^{::tc/- t/Int} a] (inc a)))
  (is-tc-e (cc/fn (^{::tc/- t/Int} [^{::tc/- t/Int} a] (inc a))))
  (is-tc-err (cc/fn ^{::tc/- t/Bool} [^{::tc/- t/Int} a] (inc a)))
  (is-tc-err (cc/fn ^{::tc/- t/Int} [^{::tc/- t/Bool} a] (inc a)))
  (is-tc-e (cc/fn [^{::tc/- '{:a t/Int}} {:keys [a]}] (inc a)))
  #_ ;;TODO
  (is-tc-e (cc/fn ^{::tc/forall [x y]} _a
             [^{::tc/- x} a
              ^{::tc/- [x :-> y]} f]
             (f a)))
  (is-tc-e (cc/fn ^{::tc/- (t/All [x y] [x [x :-> y] :-> y])} _a
             [a f]
             (f a)))
  (is-tc-err (cc/fn ^{::tc/- (t/All [x y] [x [x :-> y] :-> x])} _a
               [a f]
               (f a)))
  ;; cannot mix name annotation with others
  (is-tc-err (cc/fn ^{::tc/- (t/All [x y] [x [x :-> y] :-> x])} _a
               [^{::tc/- x} a f]
               (f a))))

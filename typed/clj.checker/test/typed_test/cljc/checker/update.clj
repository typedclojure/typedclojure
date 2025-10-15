(ns ^:typed.clojure typed-test.cljc.checker.update
  (:require [typed.clojure :as t]
            [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :refer [parse-clj]]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.proposition-rep :as fr]
            [typed.cljc.checker.lex-env :refer [-PropEnv]]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.update :as update :refer [env+ update-with-proposition]])
  (:import [clojure.lang IPersistentMap]))

(defmacro with-validator [v & body]
  `(let [v# (volatile! true)
         b# (let [~v v#] ~@body)]
     (assert @v#)
     b#))

(defn prop-sub? [{l1 :l props1 :props} {l2 :l props2 :props}]
  (and (apply = (map keys [l1 l2]))
       (every? identity
               (let [ks (keys l1)]
                 (map subtype?
                      (map #(get %1 %2) (repeat l1) ks)
                      (map #(get %1 %2) (repeat l2) ks))))
       (= props1 props2)))

(deftest simple-env+
  (testing "updating empty env with tt"
    (is (with-validator v
          (prop-sub? (env+ (-PropEnv {} #{}) [fr/-top] v (clj-opts))
                     (-PropEnv {} #{})))))
  (testing "updating non-empty env with tt"
    (is (let [l {'x r/-nil}]
          (with-validator v
            (prop-sub? (env+ (-PropEnv l #{})
                             [fr/-top] v (clj-opts))
                       (-PropEnv l #{}))))))
  (testing "updating a typical `and` conjunction

           (let [v (get-vector)
                 s (seq v)]
             (when s
               ...))"
    (is-clj 
      (let [l {'v (parse-clj `(t/Vec t/Num))
               's (parse-clj `(t/NilableNonEmptySeq t/Num))}]
        (with-validator v
          (prop-sub?
            (env+ (-PropEnv l #{(fo/-imp (fo/-not-proposition (c/Un [r/-nil r/-false] (clj-opts)) 's)
                                         (fo/-proposition (r/make-CountRange 1) 'v))})
                  [(fo/-not-proposition (c/Un [r/-nil r/-false] (clj-opts)) 's)]
                  v
                  (clj-opts))
            (-PropEnv {'v (parse-clj `(t/NonEmptyVec t/Num))
                       's (parse-clj `(t/NonEmptySeq t/Num))}
                      #{(fo/-not-proposition (c/Un [r/-nil r/-false] (clj-opts)) 's)
                        (fo/-proposition (r/make-CountRange 1) 'v)})))))))

(deftest update-with-proposition-test
  (is-clj (= (update-with-proposition
               (c/Un [(c/make-HMap (clj-opts) {:mandatory {(r/-val :type) (r/-val :Map1)}})
                      (c/make-HMap (clj-opts) {:mandatory {(r/-val :type) (r/-val :Map2)}})]
                     (clj-opts))
               (fo/-proposition (r/-val :Map1) 'tmap [(pr/-kpe :type)])
               (clj-opts))
             (c/make-HMap (clj-opts) {:mandatory {(r/-val :type) (r/-val :Map1)}})))
  ;test that update-with-proposition resolves Names properly
  (is-with-aliases (= (update-with-proposition
                        (r/Name-maker 'clojure.core.typed.test.util-aliases/MapStruct2)
                        (fo/-proposition (r/-val :MapStruct1) 'tmap [(pr/-kpe :type)])
                        (clj-opts))
                      (r/Bottom)))
  ;test that update-with-proposition resolves Names properly
  ; here we refine the type of tmap with the equivalent of following the then branch 
  ; with test (= :MapStruct1 (:type tmap))
  (is-with-aliases (= (update-with-proposition
                        (r/Name-maker 'clojure.core.typed.test.util-aliases/UnionName)
                        (fo/-proposition (r/-val :MapStruct1) 'tmap [(pr/-kpe :type)])
                        (clj-opts))
                      (c/make-HMap (clj-opts)
                                   {:mandatory {(r/-val :type) (r/-val :MapStruct1) 
                                                (r/-val :a) (r/Name-maker 'clojure.core.typed.test.util-aliases/MyName)}})))
  (is-with-aliases (= (update-with-proposition
                        (r/Name-maker 'clojure.core.typed.test.util-aliases/UnionName)
                        (fo/-not-proposition (r/-val :MapStruct1) 'tmap [(pr/-kpe :type)])
                        (clj-opts))
                      (c/make-HMap (clj-opts)
                                   {:mandatory {(r/-val :type) (r/-val :MapStruct2) 
                                                (r/-val :b) (r/Name-maker 'clojure.core.typed.test.util-aliases/MyName)}})))
  (is-clj (= (update-with-proposition (c/Un [r/-true r/-false] (clj-opts)) (fo/-proposition (c/Un [r/-false r/-nil] (clj-opts)) 'a nil)
                                 (clj-opts))
             r/-false)))

(deftest or-proposition-update-with-proposition-test
  (is-clj (= (update-with-proposition
               r/-any
               (fo/-or [(fo/-proposition (c/RClass-of clojure.lang.Symbol (clj-opts)) 'id)
                        (fo/-proposition (c/RClass-of String (clj-opts)) 'id)]
                       (clj-opts))
               (clj-opts))
             (c/Un [(c/RClass-of clojure.lang.Symbol (clj-opts))
                    (c/RClass-of String (clj-opts))]
                   (clj-opts)))))

(deftest path-update-test
  (is-clj 
    (both-subtype? (update-with-proposition
                     (c/Un [r/-nil (c/make-HMap (clj-opts) {:mandatory {(r/-val :foo) (c/RClass-of Number (clj-opts))}})] (clj-opts))
                     (fo/-proposition (c/Un [r/-false r/-nil] (clj-opts)) 'id [(pr/-kpe :foo)])
                     (clj-opts))
                   r/-nil))
  (is-clj 
    (both-subtype? (update-with-proposition
                     (c/Un [r/-nil (c/make-HMap (clj-opts) {:mandatory {(r/-val :foo) (c/RClass-of Number (clj-opts))}})] (clj-opts))
                     (fo/-not-proposition (c/Un [r/-false r/-nil] (clj-opts)) 'id [(pr/-kpe :foo)])
                     (clj-opts))
                   (c/make-HMap (clj-opts) {:mandatory {(r/-val :foo) (c/RClass-of Number (clj-opts))}})))
  ; if (:foo a) is nil, either a has a :foo entry with nil, or no :foo entry
  (is-clj (both-subtype? (update-with-proposition
                           (c/make-HMap (clj-opts) {})
                           (fo/-proposition r/-nil 'id [(pr/-kpe :foo)])
                           (clj-opts))
                         (c/make-HMap (clj-opts) {:optional {(r/-val :foo) r/-nil}}))))

(deftest keys-vals-update-test
  (is-clj (both-subtype? 
            (update-with-proposition
              (c/RClass-of IPersistentMap [r/-any r/-any] (clj-opts))
              (fo/-proposition (c/-name `t/Seqable (c/RClass-of Number (clj-opts)))
                          'a [(pr/KeysPE-maker)])
              (clj-opts))
            (c/RClass-of IPersistentMap [(c/RClass-of Number (clj-opts)) r/-any] (clj-opts))))
  (is-tc-e (fn [every? :- (t/All [x y] [[x :-> t/Any :filters {:then (is y 0)}] (t/Coll x) :-> t/Bool :filters {:then (is (t/Coll y) 1)}])
                m :- (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)]
             (every? number? (keys m))))
  ; test with = instead of subtype to catch erroneous downcast to (IPersistentMap clojure.core.typed/Nothing clojure.core.typed/Any)
  (is-clj (both-subtype?
            (-> 
              (tc-t (let [m (clojure.core.typed/ann-form
                              {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))]
                      (assert (every? number? (keys m)))
                      m))
              r/ret-t)
            (parse-clj `(clojure.lang.IPersistentMap Number t/Any))))
  (is-clj (both-subtype? 
            (-> (tc-t (let [m (clojure.core.typed/ann-form
                                {}
                                (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))]
                        (assert (every? number? (keys m)))
                        (assert (every? number? (vals m)))
                        m))
                r/ret-t)
            (parse-clj `(IPersistentMap Number Number))))
  (is-cf (fn [m]
            {:pre [(every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)
           -> (clojure.lang.IPersistentMap clojure.core.typed/Any Number)])
  (is-cf (fn [m]
            {:pre [(every? symbol? (keys m))
                   (every? number? (vals m))]}
            m)
          [(clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)
           -> (clojure.lang.IPersistentMap clojure.lang.Symbol Number)]))

(deftest nested-keyword-update-test
  ; ordinary IPersistentMap does not get updated
  (is-tc-e (fn []
             (let [a :- (t/Map t/Any t/Any) {}]
               (if (number? (-> a :a :b))
                 a
                 (assert nil))))
           [-> (t/Map t/Any t/Any)])
  ; HMaps can gain "one level" of known entries.
  (is-tc-e (fn []
             (let [a :- '{} {}]
               (if (number? (-> a :a :b))
                 a
                 (assert nil))))
           [-> (t/HMap :optional {:a t/Any})])
  ; update-with-proposition a (t/HMap) with (is clojure.core.typed/Any a [(Key :a) (Key :b)])
  ; returns a (t/HMap :optional {:a clojure.core.typed/Any})
  ; Only one level is updated, we can't say any more about the inner
  ; :b key.
  (is-clj (let [t (parse-clj `(t/HMap))
                path [(pr/-kpe :a) (pr/-kpe :b)]
                lo+ (fo/-proposition (parse-clj `Number) 'a path)
                lo- (fo/-not-proposition (parse-clj `Number) 'a path)
                expected+ (parse-clj `(t/HMap :optional {:a t/Any}))
                expected- (parse-clj `(t/HMap :optional {:a t/Any}))]
            (and (both-subtype? (update-with-proposition t lo+ (clj-opts)) expected+)
                 (both-subtype? (update-with-proposition t lo- (clj-opts)) expected+))))
  ; negative absent keys. The absent entry :a is not a Number (KeyPE does not support defaults), so we
  ; just return the original type
  (is-clj (let [t (parse-clj `(t/HMap :absent-keys #{:a}))]
            (= t
               (update-with-proposition t (fo/-not-proposition (c/RClass-of Number (clj-opts)) 'a [(pr/-kpe :a) (pr/-kpe :b)]) (clj-opts)))))

  ; When we update-with-proposition a (t/HMap) that has no information about an :a key, sometimes we can prove
  ; the updated type always has an :a key.
  ;
  ; Here we restrict to a '{:a Number} because the path is a Number, which is never nil. We assume
  ; nil is the not-found type.
  (is-clj (let [t (parse-clj `(t/HMap))]
            (both-subtype? (parse-clj `(t/HMap :mandatory {:a t/Num}))
                           (update-with-proposition t (fo/-proposition (c/RClass-of Number (clj-opts)) 'a [(pr/-kpe :a)]) (clj-opts)))))

  ; We restrict (t/HMap) to (t/HMap :optional {:a clojure.core.typed/Any}), which is slightly less accurate, because
  ; we can't prove that the t/HMap :a entry is never nil. 
  (is-clj (let [t (parse-clj `(t/HMap))]
            (both-subtype? (parse-clj `(t/HMap :optional {:a t/Any}))
                           (update-with-proposition t (fo/-not-proposition (c/RClass-of Number (clj-opts)) 'a [(pr/-kpe :a)]) (clj-opts))))))

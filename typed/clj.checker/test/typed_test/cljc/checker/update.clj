(ns typed-test.cljc.checker.update
  (:require [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :refer [parse-clj]]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.lex-env :refer [-PropEnv]]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.update :as update :refer [env+ update-with-filter]])
  (:import [clojure.lang IPersistentMap Seqable]))

(defmacro with-validator [v & body]
  `(let [~v (atom true :validator (constantly true))
         b# ~@body]
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
          (prop-sub? (env+ (-PropEnv {} #{}) [fr/-top] v)
                     (-PropEnv {} #{})))))
  (testing "updating non-empty env with tt"
    (is (let [l {'x r/-nil}]
          (with-validator v
            (prop-sub? (env+ (-PropEnv l #{})
                             [fr/-top] v)
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
            (env+ (-PropEnv l #{(fo/-imp (fo/-not-filter (c/Un r/-nil r/-false) 's)
                                      (fo/-filter (r/make-CountRange 1) 'v))})
                  [(fo/-not-filter (c/Un r/-nil r/-false) 's)]
                  v)
            (-PropEnv {'v (parse-clj `(t/NonEmptyVec t/Num))
                       's (parse-clj `(t/NonEmptySeq t/Num))}
                      #{(fo/-not-filter (c/Un r/-nil r/-false) 's)
                        (fo/-filter (r/make-CountRange 1) 'v)})))))))

(deftest update-with-filter-test
  (is-clj (= (update-with-filter
               (c/Un (c/make-HMap :mandatory {(r/-val :type) (r/-val :Map1)})
                   (c/make-HMap :mandatory {(r/-val :type) (r/-val :Map2)}))
               (fo/-filter (r/-val :Map1) 'tmap [(pr/-kpe :type)]))
             (c/make-HMap :mandatory {(r/-val :type) (r/-val :Map1)})))
  ;test that update-with-filter resolves Names properly
  (is-with-aliases (= (update-with-filter
                        (r/Name-maker 'clojure.core.typed.test.util-aliases/MapStruct2)
                        (fo/-filter (r/-val :MapStruct1) 'tmap [(pr/-kpe :type)]))
                      (c/Un)))
  ;test that update-with-filter resolves Names properly
  ; here we refine the type of tmap with the equivalent of following the then branch 
  ; with test (= :MapStruct1 (:type tmap))
  (is-with-aliases (= (update-with-filter
                        (r/Name-maker 'clojure.core.typed.test.util-aliases/UnionName)
                        (fo/-filter (r/-val :MapStruct1) 'tmap [(pr/-kpe :type)]))
                      (c/make-HMap :mandatory {(r/-val :type) (r/-val :MapStruct1) 
                                             (r/-val :a) (r/Name-maker 'clojure.core.typed.test.util-aliases/MyName)})))
  (is-with-aliases (= (update-with-filter
                        (r/Name-maker 'clojure.core.typed.test.util-aliases/UnionName)
                        (fo/-not-filter (r/-val :MapStruct1) 'tmap [(pr/-kpe :type)]))
                      (c/make-HMap :mandatory {(r/-val :type) (r/-val :MapStruct2) 
                                             (r/-val :b) (r/Name-maker 'clojure.core.typed.test.util-aliases/MyName)})))
  (is-clj (= (update-with-filter (c/Un r/-true r/-false) (fo/-filter (c/Un r/-false r/-nil) 'a nil)) 
             r/-false)))

(deftest or-filter-update-with-filter-test
  (is-clj (= (update-with-filter
               r/-any
               (fo/-or (fo/-filter (c/RClass-of clojure.lang.Symbol) 'id)
                       (fo/-filter (c/RClass-of String) 'id)))
             (c/Un (c/RClass-of clojure.lang.Symbol)
                 (c/RClass-of String)))))

(deftest path-update-test
  (is-clj 
    (both-subtype? (update-with-filter
                     (c/Un r/-nil (c/make-HMap :mandatory {(r/-val :foo) (c/RClass-of Number)}))
                     (fo/-filter (c/Un r/-false r/-nil) 'id [(pr/-kpe :foo)]))
                   r/-nil))
  (is-clj 
    (both-subtype? (update-with-filter
                     (c/Un r/-nil (c/make-HMap :mandatory {(r/-val :foo) (c/RClass-of Number)}))
                     (fo/-not-filter (c/Un r/-false r/-nil) 'id [(pr/-kpe :foo)]))
                   (c/make-HMap :mandatory {(r/-val :foo) (c/RClass-of Number)})))
  ; if (:foo a) is nil, either a has a :foo entry with nil, or no :foo entry
  (is-clj (both-subtype? (update-with-filter
                           (c/make-HMap)
                           (fo/-filter r/-nil 'id [(pr/-kpe :foo)]))
                         (c/make-HMap :optional {(r/-val :foo) r/-nil}))))

(deftest keys-vals-update-test
  (is-clj (both-subtype? 
            (update-with-filter
              (c/RClass-of IPersistentMap [r/-any r/-any])
              (fo/-filter (c/RClass-of Seqable [(c/RClass-of Number)])
                       'a [(pr/KeysPE-maker)]))
            (c/RClass-of IPersistentMap [(c/RClass-of Number) r/-any])))
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
             (let [a :- (Map Any Any) {}]
               (if (number? (-> a :a :b))
                 a
                 (assert nil))))
           [-> (Map Any Any)])
  ; HMaps can gain "one level" of known entries.
  (is-tc-e (fn []
             (let [a :- '{} {}]
               (if (number? (-> a :a :b))
                 a
                 (assert nil))))
           [-> (HMap :optional {:a Any})])
  ; update-with-filter a (HMap) with (is clojure.core.typed/Any a [(Key :a) (Key :b)])
  ; returns a (HMap :optional {:a clojure.core.typed/Any})
  ; Only one level is updated, we can't say any more about the inner
  ; :b key.
  (is-clj (let [t (parse-clj `(t/HMap))
                path [(pr/-kpe :a) (pr/-kpe :b)]
                lo+ (fo/-filter (parse-clj `Number) 'a path)
                lo- (fo/-not-filter (parse-clj `Number) 'a path)
                expected+ (parse-clj `(t/HMap :optional {:a t/Any}))
                expected- (parse-clj `(t/HMap :optional {:a t/Any}))]
            (and (both-subtype? (update-with-filter t lo+) expected+)
                 (both-subtype? (update-with-filter t lo-) expected+))))
  ; negative absent keys. The absent entry :a is not a Number (KeyPE does not support defaults), so we
  ; just return the original type
  (is-clj (let [t (parse-clj `(t/HMap :absent-keys #{:a}))]
            (= t
               (update-with-filter t (fo/-not-filter (c/RClass-of Number) 'a [(pr/-kpe :a) (pr/-kpe :b)])))))

  ; When we update-with-filter a (HMap) that has no information about an :a key, sometimes we can prove
  ; the updated type always has an :a key.
  ;
  ; Here we restrict to a '{:a Number} because the path is a Number, which is never nil. We assume
  ; nil is the not-found type.
  (is-clj (let [t (parse-clj `(t/HMap))]
            (both-subtype? (parse-clj `(t/HMap :mandatory {:a t/Num}))
                           (update-with-filter t (fo/-filter (c/RClass-of Number) 'a [(pr/-kpe :a)])))))

  ; We restrict (HMap) to (HMap :optional {:a clojure.core.typed/Any}), which is slightly less accurate, because
  ; we can't prove that the HMap :a entry is never nil. 
  (is-clj (let [t (parse-clj `(t/HMap))]
            (both-subtype? (parse-clj `(t/HMap :optional {:a t/Any}))
                           (update-with-filter t (fo/-not-filter (c/RClass-of Number) 'a [(pr/-kpe :a)]))))))

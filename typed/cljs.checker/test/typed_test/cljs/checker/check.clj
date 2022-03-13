(ns
  ;;FIXME
  ^:typed/skip-from-repo-root
  typed-test.cljs.checker.check
  (:require [cljs.core.typed :as t]
            [clojure.core.typed.analyzer-api-intercept :as fake-ana-api]
            [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljs.checker.test-utils :refer :all]))

(deftest throw-test
  (is-tc-e (throw (js/JSError. "foo"))
           t/Nothing)
  (is-tc-e (throw 1)
           t/Nothing))

(deftest ann-test
  (is-tc-e (do (t/ann foo t/JSnumber)
               (def foo 1)
               foo)
           t/JSnumber)
  (is-tc-err (do (t/ann foo t/JSnumber)
                 (def foo 1)
                 foo)
             nil)
  (is-tc-e (do (t/ann ^:no-check foo t/JSnumber)
               (def foo nil)
               foo)
           t/JSnumber)
  (is-tc-e (do (t/ann ^:no-check foo t/JSnumber)
               (def foo (fn [] (inc nil)))
               foo)
           t/JSnumber)
  (is-tc-err (do (t/ann foo t/JSnumber)
                 (def foo nil))))

(deftest check-ns-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.ann)))

(deftest parse-protocol-test 
  (is-cljs (prs/parse-cljs '(cljs.core/IMapEntry cljs.core.typed/JSnumber cljs.core.typed/JSnumber))))

(deftest Protocol-of-test
  (is-cljs (c/Protocol-of 'cljs.core/IMapEntry [(r/JSNumber-maker)
                                                (r/JSNumber-maker)])))

(deftest do-test
  (is-tc-e (do) nil)
  (is-tc-e (do nil) nil)
  (is-tc-e (do #{1})
           (t/Set t/JSnumber)))

(deftest heterogeneous-ds-test
  (is-tc-e [1 2]
           '[t/JSnumber t/JSnumber])
  (is-tc-e [1 2]
           (IVector t/JSnumber))
  (is-tc-e {:a 1}
           '{:a t/JSnumber})
  (is-tc-e {1 1}
           (t/Map t/JSnumber t/JSnumber))
  (is-tc-e #{1}
           (t/HSet #{1}))
  (is-tc-e #{1}
           (t/Set t/JSnumber))
  (is-tc-e (let [a 1] #{1 a})
           (t/Set t/JSnumber)))

(deftest js*-test
  (is-tc-e (js* "(~{})" 1)))

(deftest fn-test
  (is-tc-e (fn a [b] a))
  (is-tc-e (t/fn a [b] a))
  (is-tc-e (cljs.core.typed/fn a [b] a))
  (is-tc-e (core/fn a [b] a))
  (is-tc-e (fn [a] a)
           (t/All [x] [x -> x])))

(deftest ann-form-test
  (is-tc-e (t/ann-form 1 t/Num))
  (is-tc-e (t/ann-form 1 t/Num)
           t/Num)
  (is-tc-e (t/ann-form
             (t/ann-form 1 t/Num)
             t/Num))
  (is-tc-err (t/ann-form
               (t/ann-form 1 t/Num)
               nil))
  (is-tc-err (t/ann-form 1 t/Num)
             nil)
  (is-tc-err (t/ann-form 1 nil))
  (is-tc-err (t/ann-form 1 nil)
             t/Num))

(deftest inst-test
  (is-tc-e (let [f (-> (fn [a] a)
                       (t/ann-form (t/All [x] [x -> x])))
                 res ((t/inst f t/JSnumber) 1)]
             res)
           t/JSnumber))

#_ ;;TODO
(deftest letfn-test
  (is-tc-e (t/letfn> [a :- (t/All [x] [x -> x])
                      (a [b] b)]
             (a 1))))

(deftest ann-protocol-test
  (is-tc-e
    (cljs.core.typed/ann-protocol
      [[r :variance :covariant]]
      cljs.core.async.impl.protocols/ReadPort)))

;;commenting-out this test because just :require -ing cljs.core.async fails with internal error
#_(deftest async-test
  (is-cljs (t/check-ns* 'typed.lib.cljs.core.async)))

#_
(deftest inline-annotation-test
  ; code from David Nolen's blog
  ;FIXME
  #_(is-tc-e
    (defn ^{:ann '[(t/U nil (ISeqable t/Any)) t/Any -> cljs.core.typed/CLJSInteger]}
      index-of [xs x]
      (let [len (count xs)]
        (t/loop>
         [i :- cljs.core.typed/CLJSInteger, 0]
         (if (< i len)
           (if (= (nth xs i) x)
             i
             (recur (inc i)))
           -1))))))

(deftest simple-polymorphic-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.identity)))

(deftest value-supertype-test
  (is-tc-e 'a Symbol)
  (is-tc-e :a Keyword)
  (is-tc-e 1 t/CLJSInteger)
  (is-tc-e 1.1 t/JSnumber)
  (is-tc-e 1 t/JSnumber)
  (is-tc-e true t/JSboolean)
  (is-tc-e "a" t/JSstring))

(deftest ns-deps-test
  (is (t/check-ns* 'cljs.core.typed.test.dep-one))
  (is (t/check-ns* 'cljs.core.typed.test.dep-two)))

(deftest hvec-infer
  (is-tc-e (fn [a]
             (a [1 2]))
           [[(cljs.core/IVector t/Any) -> t/Any]
            -> t/Any])
  (is-tc-e (fn [a]
             (a [1 2]))
           [(t/All [x] [(cljs.core/IVector x) -> x])
            -> t/Any]))

(deftest seq-test
  (is-tc-e [1 2 3] (t/Coll cljs.core.typed/CLJSInteger))
  (is-tc-e [1 2 3] (t/Seqable cljs.core.typed/CLJSInteger))
  (is-tc-e (seq [1 2 3]) (t/NonEmptyASeq cljs.core.typed/CLJSInteger)))

;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.dom)
;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.reactive)
;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.helpers)
;(t/check-ns* 'cljs.core.typed.async)


;;FIXME
#_
(deftest core-fns-test
  (t/check-ns* 'cljs.core.typed.test.ympbyc.test-base-env))

(deftest ctyp-255-cljs-test
  (testing "unparsing protocols is fully qualified in :unknown"
    (is-cljs (= (prs/unparse-type (c/Protocol-of 'cljs.core/ISet))
                'cljs.core/ISet))))


(def nodes #{:binding :case :case-node :case-test :case-then :const :def :defrecord :deftype :do :fn :fn-method :host-call :host-field :if :invoke :js :js-array :js-object :js-var :let :letfn :local :loop :map :new :no-op :ns :ns* :quote :recur :set :set! :the-var :throw :try :var :vector :with-meta
            })

#_ ;;TODO
(deftest check-case-coverage-test
  (fake-ana-api/reset-found)

  ;;let
  (is-tc-e (let [x 0
                 y x]
             y)
           t/JSnumber)

  ;;FIXME
  ;;case
  #_
  (is-tc-e (fn [x] (case x
                    0 "zero"
                    "non-zero"))
           [t/JSnumber -> cljs.core.typed/JSString])

  ;;def
  (is-tc-e (def x 1))

  ;;fn
  (is-tc-e (fn [x] x))
  
  
  ;;const
  (is-tc-e 1 t/JSnumber)

  ;;if
  (is-tc-e (if 1 1 0))

  ;;FIXME
  ;;letfn
  #_
  (is-tc-e (t/letfn> [foo :- [t/JSnumber -> t/JSnumber]
                      (foo [x] x)]
             (foo 2)))

  ;;loop
  (is-tc-e (t/loop [a :- t/JSnumber 1
                    b :- (t/U nil t/JSnumber) nil]
             (if b (str a)
               (recur 1 1)))
           t/Str)

  ;;map
  (is-tc-e {:name "Bob" :job "unemployed"})

  ;;set
  (is-tc-e #{1 2 3})

  ;;quote
  (is-tc-e '(1 2 3))

  (print "MISSING NODES (fake ERROR): ")
  (doseq [op (sort (clojure.set/difference nodes @fake-ana-api/ops-found))]
    (print (str op " ")))
  (println))

(deftest HSequential-parse-test
  (is-tc-e [] (t/HSequential [t/Any *]))
  (is-tc-e '() (t/HSeq [t/Any *]))
  (is-tc-e #{:kw} (t/HSet [:kw]))
  ;; FIXME Value/Val parsing
  #_(is-tc-e {:a 1} (t/HMap :mandatory {:a (t/Value 1)}))
  #_(is-tc-e {:a 1} (t/HMap :mandatory {:a (t/Val 1)}))
  (is-tc-e {:a 1}
           (t/Rec [x] (t/U nil (t/HMap :mandatory {:a t/JSnumber} :optional {:b x}))))
  )

(deftest undefined-test
  (is-tc-err nil t/JSundefined)
  (is-tc-e nil t/JSnull)
  (is-tc-e nil nil)
  (is (not (sub? nil cljs.core.typed/JSnull)))
  (is (not (sub? nil cljs.core.typed/JSundefined)))
  (is (sub? cljs.core.typed/JSundefined nil))
  (is (sub? cljs.core.typed/JSnull nil))
  (is-tc-e (t/fn [a :- t/JSundefined] :- nil
             a))
  (is-tc-e (t/fn [a :- t/JSnull] :- nil
             a))
  (is-tc-err (t/fn [a :- t/JSnull] :-  t/JSundefined
               a))
  (is-tc-err (t/fn [a :- t/JSundefined] :-  t/JSnull
               a))
  (is-tc-e (when (undefined? nil)
             :kw)
           nil)
  (is-tc-err (when (undefined? (t/ann-form nil nil))
               :kw)
             nil)
  (is-tc-e (t/fn [a :- t/JSnull]
             (when (undefined? a)
               :kw))
           [t/JSnull :-> nil])
  (is-tc-e (t/fn [a :- t/JSundefined]
             (when (undefined? a)
               :kw))
           [t/JSundefined :-> ':kw])
  (is-tc-e (do
             (t/ann ^:no-check a t/JSundefined)
             (def a nil)
             a)
           nil)
  (is-tc-e (t/fn [a :- (t/U (cljs.core/IVector t/Any) t/JSundefined)]
             (if a
               (t/ann-form a (cljs.core/IVector t/Any))
               (t/ann-form a t/JSundefined)))))

(deftest ratio-test
  (is-tc-e 1/2 t/JSnumber))

#_
(deftest goog-imports
  (is-cljs (t/check-ns* 'cljs.core.typed.test.goog-import)))

(deftest jsobj-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.js-obj)))

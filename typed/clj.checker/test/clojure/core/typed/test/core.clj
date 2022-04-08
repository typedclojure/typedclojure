(ns clojure.core.typed.test.core
  (:refer-clojure :exclude [cast])
  (:require 
    ; this loads the type system, must go first
    [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.clj.checker.analyze-clj :as ana]
            ;[typed.clj.analyzer.passes.emit-form :as emit-form]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed.unsafe]
            [typed.cljc.checker.utils :as u :refer [expr-type]]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.check :as chk]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.update :as update :refer [env+ update-with-filter]]
            [typed.clj.checker.tc-equiv :refer [tc-equiv]]
            [typed.cljc.checker.collect-utils :as collect-u]
            [typed.cljc.checker.inst :as inst]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.type-ctors :refer :all]
            [typed.cljc.checker.type-rep :refer :all]
            [typed.cljc.checker.filter-rep :refer :all]
            [typed.cljc.checker.filter-ops :refer :all]
            [typed.cljc.checker.object-rep :refer :all]
            [typed.cljc.checker.path-rep :refer :all]
            [typed.clj.checker.parse-unparse :refer :all]
            [typed.clj.checker.constant-type :refer [constant-type]]
            [typed.cljc.checker.lex-env :refer :all]
            [typed.cljc.checker.promote-demote :refer :all]
            [typed.cljc.checker.frees :refer :all]
            [typed.cljc.checker.free-ops :refer :all]
            [typed.cljc.checker.dvar-env :refer :all]
            [typed.cljc.checker.cs-gen :as cgen :refer :all]
            [typed.cljc.checker.cs-rep :as crep :refer :all]
            [typed.cljc.checker.subst :refer [subst-all] :as subst]
            [clojure.core.typed.test.rbt]
            [clojure.core.typed.test.person]
            [clojure.core.typed.internal]
            [typed.clj.checker.path-type :refer :all]
            [clojure.core.typed.load :as load]
            [typed.cljc.checker.ns-deps-utils :as ndu]
            [clojure.core.typed.parse-ast :as prs-ast])
; we want clojure.lang.Seqable to be scoped here. 
; The :refer :all of clojure.core.typed adds another Seqable which
; is less useful here.
  (:use [clojure.core.typed :as t :exclude [Seqable loop fn defprotocol let dotimes
                                            for doseq def remove filter defn atom ref]])
  (:import (clojure.lang ISeq IPersistentVector Atom IPersistentMap
                         ExceptionInfo Var Seqable)))

(defmacro is-tc-e-with-aliases [& body]
  `(is-clj (do (check-ns '~'clojure.core.typed.test.util-aliases)
               (tc-e ~@body)
               true)))

(deftest add-scopes-test
  (is-clj (let [body (make-F 'a)]
            (= (add-scopes 0 body)
               body)))
  (is-clj (let [body (make-F 'a)]
            (= (add-scopes 1 body)
               (Scope-maker body))))
  (is-clj (let [body (make-F 'a)]
            (= (add-scopes 3 body)
               (-> body Scope-maker Scope-maker Scope-maker)))))

(deftest remove-scopes-test
  (is-clj (let [body (make-F 'a)]
            (= (remove-scopes 1 (Scope-maker body))
               body))))

(deftest parse-type-test
  (is-clj (= (Poly-body* '(x) (parse-type '(clojure.core.typed/All [x] x)))
             (make-F 'x)))
  (is-clj (= (Poly-body* '(x y) (parse-type '(clojure.core.typed/All [x y] x)))
             (make-F 'x)))
  (is-clj (= (Poly-body* '(x y) (parse-type '(clojure.core.typed/All [x y] y)))
             (make-F 'y)))
  (is-clj (= (Poly-body* '(a b c d e f g h i) (parse-type '(clojure.core.typed/All [a b c d e f g h i] e)))
             (make-F 'e))))

(deftest parse-type-fn-test
  (is-clj (= (parse-type '[nil * -> nil])
             (make-FnIntersection (make-Function () -nil :rest -nil))))
  (is-clj (= (parse-type '(clojure.core.typed/All [x ...] [nil ... x -> nil]))
             (PolyDots* '(x) [no-bounds]
                        (make-FnIntersection (make-Function () -nil :drest (DottedPretype1-maker -nil 'x)))))))

(deftest poly-constructor-test
  (is-clj (= (Poly-body*
               '(x)
               (Poly* '(x) [no-bounds]
                      (make-F 'x)))
             (make-F 'x)))
  (is-clj (= (Poly-body*
               '(x)
               (Poly* '(x)
                      [(Bounds-maker -nil -false nil)]
                      (make-F 'x)))
         (make-F 'x)))
  (is-clj (= (parse-type '(clojure.core.typed/All [x x1 [y :< x] z] [x -> y]))
             (let [no-bounds-scoped (Bounds-maker
                                      (add-scopes 4 -any)
                                      (add-scopes 4 (Un))
                                      nil)]
               (Poly-maker 4
                           [no-bounds-scoped
                            no-bounds-scoped
                            (Bounds-maker 
                              (add-scopes 4 (B-maker 3))
                              (add-scopes 4 (Un))
                              nil)
                            no-bounds-scoped]
                           (add-scopes 4
                                       (make-FnIntersection
                                         (make-Function [(B-maker 3)] (B-maker 1))))
                           {})))))

(deftest trans-dots-test
  (is-clj (= (inst/manual-inst (parse-type '(clojure.core.typed/All [x b ...]
                                                 [x ... b -> x]))
                               (mapv parse-type '(Integer Double Float))
                               {})
             (parse-type '[Integer Integer -> Integer])))
  (is-clj (= (inst/manual-inst (parse-type '(clojure.core.typed/All [x b ...]
                                                 [b ... b -> x]))
                               (mapv parse-type '(Integer Double Float))
                               {})
             (parse-type '[Double Float -> Integer])))
  ;map type
  (is-clj (= (inst/manual-inst (parse-type '(clojure.core.typed/All [c a b ...]
                                                 [[a b ... b -> c] (clojure.lang.Seqable a) (clojure.lang.Seqable b) ... b -> (clojure.lang.Seqable c)]))
                               (mapv parse-type '(Integer Double Float))
                               {})
             (parse-type '[[Double Float -> Integer] (clojure.lang.Seqable Double) (clojure.lang.Seqable Float) -> (clojure.lang.Seqable Integer)])))
  (is-clj (= (clj (inst/manual-inst (parse-type '(clojure.core.typed/All [x b ...]
                                                      ['[x b] ... b -> '['[x b] ... b]]))
                                    (mapv parse-type '(Integer Double Float))
                                    {}))
             (parse-type '['[Integer Double] '[Integer Float] 
                           -> '['[Integer Double] '[Integer Float]]])))
  ;TODO t/HSequential
  (is-clj (= (clj
               (inst/manual-inst (parse-type `(t/All [x# b# ...]
                                                   [x# ... b# :-> (t/HSequential [x# ... b#])]))
                                 (mapv parse-type `(Integer Double Float))
                                 {}))
             (parse-type `(t/IFn [Integer Integer :-> (t/HSequential [Integer Integer])]))))
  ; completeness check
  (is (check-ns 'clojure.core.typed.test.trans-dots))
  )


(deftest tc-invoke-fn-test
  (is-clj (subtype? (ety
                      ((fn [a :- Number, b :- Number] b)
                       1 2))
                    (parse-type `Number)))
  ; manual instantiation "seq"
  ;FIXME randomly fails. Try again when I/U are sorted sets.
  (is-clj (subtype? (ety
                      ((fn [a :- (clojure.lang.Seqable Number), b :- Number] 
                         ((clojure.core.typed/inst seq Number) a))
                       [1 2 1.2] 1))
                    (parse-type `(t/Option (t/I (clojure.lang.ISeq java.lang.Number) (t/CountRange 1))))))
  ; inferred "seq"
  (is-clj (subtype? (ety
                      (fn [a :- (clojure.lang.Seqable Number), b :- Number] 
                        1))
                    (make-FnIntersection
                      (make-Function
                        [(RClass-of Seqable [(RClass-of Number nil)]) (RClass-of Number nil)] 
                        (-val 1)
                        :filter (-FS -top -bot)
                        :object -empty))))
  ; poly inferred "seq"
  ; FIXME pfn> NYI
  #_(is-clj (both-subtype?
            (ety
              (fn :forall [c] 
                 [a :- (t/Seqable c)
                  b :- t/Num]
                 1))
            (clj 
              (let [x (make-F 'x)]
                (Poly* [(:name x)]
                       [no-bounds]
                       (make-FnIntersection
                         (make-Function
                           [(RClass-of Seqable [x]) (RClass-of Number)] 
                           (-val 1)
                           :filter (-FS -top -bot)
                           :object -empty)))))))
  ;test invoke fn
  (is-clj (subtype? (ety
                      ((fn [a :- (t/Seqable t/Num), b :- t/Num] 
                         (seq a))
                       [1 2 1.2] 1))
                    (parse-type `(t/U nil (t/I (t/CountRange 1) (clojure.lang.ISeq Number))))))
  (is-clj (subtype? (ety
                      ((fn [a :- (t/Map t/Any t/Num), b :- t/Num] 
                         ((inst get t/Num t/Nothing) a b))
                       (zipmap [1] [2]) 1))
                    (parse-type `(t/U nil Number)))))

;FIXME
;(deftest get-special-test
;  (is-clj (subtype? 
;            (ety 
;              (clojure.core.typed/fn [a :- (t/HMap :mandatory {:a Number})]
;                                     (get a :a)))
;            (parse-type
;              '(Fn ['{:a java.lang.Number} -> java.lang.Number 
;                    :filters {:then (& (is (t/HMap :mandatory {:a Number}) 0)
;                                       (is java.lang.Number 0 [(Key :a)]))
;                              :else (| (is (t/U nil false) 0 [(Key :a)])
;                                       (is (t/HMap :absent-keys #{:a}) 0) )} 
;                    :object {:path [(Key :a)], :id 0}])))))

(deftest truth-false-values-test
  (is-clj (= (tc-t (if nil 1 2))
         (ret (-val 2) (-FS -top -bot) (EmptyObject-maker))))
  (is-clj (= (tc-t (if false 1 2))
         (ret (-val 2) (-FS -top -bot) (EmptyObject-maker))))
  (is-clj (= (tc-t (if 1 1 2))
         (ret (-val 1) (-FS -top -bot) (EmptyObject-maker)))))

(deftest empty-fn-test
  (is-clj (= (tc-t (clojure.core/fn []))
         (ret (make-FnIntersection
                (Function-maker [] (make-Result -nil
                                            (-FS -bot -top)
                                            (EmptyObject-maker))
                            nil nil nil nil nil))
              (-FS -top -bot)
              (EmptyObject-maker))))
  (is-clj (= (tc-t (fn [] 1))
         (ret (make-FnIntersection
                (Function-maker [] (make-Result (-val 1)
                                              (-FS -top -bot)
                                              (EmptyObject-maker))
                              nil nil nil nil nil))
              (-FS -top -bot)
              (EmptyObject-maker))))
  (is-clj (= (tc-t (let []))
         (ret -nil (-FS -bot -top) (EmptyObject-maker)))))

(deftest path-test
  (is-clj (= (tc-t (fn [a] (let [a 1] a)))
             (ret (make-FnIntersection
                    (Function-maker [-any]
                                    (make-Result (-val 1)
                                                 (-true-filter)
                                                 -empty)
                                    nil nil nil nil nil))
                  (-FS -top -bot) -empty)))
  (is-clj (= (tc-t (let [a nil] a))
             (ret -nil (-FS -top -top) -empty))))

(deftest equiv-test
  ; 1 arity :else filter is always bot
  (is-clj (= (tc-t (= 1))
             (ret -true (-FS -top -bot) -empty)))
  (is-clj (= (tc-t (= 1 1))
             (tc-t (= 1 1 1 1 1 1 1 1 1 1))
             (ret (Un -true -false) (-FS -top -top) -empty)))
  (is-clj (= (tc-t (= 'a 'b))
             (tc-t (= 1 2))
             (tc-t (= :a :b))
             (tc-t (= :a 1 'a))
             (ret (Un -true -false) (-FS -top -top) -empty)))
  (is-clj (= (tc-t (= :Val (-> {:a :Val} :a)))
             (ret (Un -true -false) (-FS -top -top) -empty))))

(deftest name-to-param-index-test
  ;a => 0
  (is-clj 
    (= (tc-t 
         (fn [a :- (t/U '{:op (t/Value :if)}
                      '{:op (t/Value :var)})] 
           (:op a)))
       (clj
         (ret 
           (parse-type 
             `(t/IFn [(t/U '{:op (t/Value :var)} '{:op (t/Value :if)}) :-> (t/U ':var ':if) 
                    :filters {:then (& (~'! (t/U nil false) 0 [(~'Key :op)])
                                       (~'is (t/U ':if ':var) 0 [(~'Key :op)]))
                              :else (~'| (~'is (t/HMap :absent-keys #{:op}) 0) 
                                         (~'is (t/U nil false) 0 [(~'Key :op)]))} 
                    :object {:path [(~'Key :op)], :id 0}]))
           (-FS -top -bot)
           -empty)))))

(deftest refine-test
  (is-clj (= (tc-t 
               (fn [a :- (t/U (t/HMap :mandatory {:op (t/Value :if)})
                            (t/HMap :mandatory {:op (t/Value :var)}))]
                 (when (= (:op a) :if) 
                   a)))
         (ret (make-FnIntersection
                (make-Function
                    [(Un (make-HMap :mandatory {(-val :op) (-val :if)})
                         (make-HMap :mandatory {(-val :op) (-val :var)}))]
                    (Un -nil (make-HMap :mandatory {(-val :op) (-val :if)}))
                    :filter (-FS (-and (-filter (-val :if) 0 [(-kpe :op)])
                                       (-filter (make-HMap :mandatory {(-val :op) (-val :if)}) 0))
                                 (-not-filter (-val :if) 0 [(-kpe :op)]))))
              (-true-filter)))))


#_(deftest dotted-infer-test
  (is-cf (map number? [1])))

(deftest check-invoke
  ; wrap in thunk to prevent evaluation (analyzer currently evaluates forms)
  (is (err/top-level-error-thrown? (cf (fn [] (symbol "a" 'b)))))
  (is (both-subtype? (ety (symbol "a" "a"))
                     (clj (RClass-of clojure.lang.Symbol)))))

(deftest check-do-test
  (is-clj (= (ety (do 1 2))
         (-val 2))))

(deftest tc-var-test
  (is-clj (subtype? (ret-t (tc-t seq?))
                    (parse-type `(t/Pred (t/Seq t/Any))))))

(deftest heterogeneous-ds-test
  (is-clj 
    (not (subtype? (parse-type `(t/HMap :mandatory {:a (t/Value 1)}))
                   (RClass-of ISeq [-any]))))
  (is-clj 
    (not (subtype? (parse-type `(t/HVec [(t/Value 1) (t/Value 2)]))
                   (RClass-of ISeq [-any]))))
  (is-clj
    (subtype? (parse-type `(t/HSeq [(t/Value 1) (t/Value 2)]))
              (RClass-of ISeq [-any])))
  (is-clj 
    (subtype? (parse-type `(t/HList [(t/Value 1) (t/Value 2)]))
              (RClass-of ISeq [-any])))
  (is-clj (= (tc-t [1 2])
             (ret (-hvec [(-val 1) (-val 2)] :filters [(-true-filter) (-true-filter)]) (-true-filter) -empty)))
  (is-clj (= (tc-t '(1 2))
         (ret (HeterogeneousList-maker [(-val 1) (-val 2)]) (-true-filter) -empty)))
  (is-clj (= (tc-t {:a 1})
         (ret (-complete-hmap {(-val :a) (-val 1)}) (-true-filter) -empty)))
  (is-clj (= (tc-t {})
         (ret (-complete-hmap {}) (-true-filter) -empty)))
  (is-clj (= (tc-t [])
         (ret (-hvec []) (-true-filter) -empty)))
  (is-clj (= (tc-t '())
         (ret (HeterogeneousList-maker []) (-true-filter) -empty)))
  (is-cf '(a b) (clojure.core.typed/HList [clojure.lang.Symbol clojure.lang.Symbol])))

(deftest implied-atomic?-test
  (is-clj (implied-atomic? -top -bot))
  (is-clj (not (implied-atomic? -bot -top)))
  (is-clj (implied-atomic? -top
                           (-filter (RClass-of Long) 0)))
  (is-clj (not
            (implied-atomic? (-filter (RClass-of Long) 0)
                           -top)))
  (is-clj (implied-atomic? (-filter (RClass-of Long) 0)
                           (-filter (RClass-of Long) 0)))
  (is-clj (implied-atomic? (-filter (RClass-of Long) 'a__#0)
                           (-filter (RClass-of Long) 'a__#0)))
  (is-clj (implied-atomic? (-not-filter (Name-maker `Long) 'a__#0)
                           (-not-filter (RClass-of Long) 'a__#0)))
  (is-clj (implied-atomic? (-not-filter (RClass-of Long) 'a__#0)
                           (-not-filter (Name-maker `Long) 'a__#0)))
  (is-clj (implied-atomic? (-not-filter -false 'a)(-not-filter (Un -nil -false) 'a)))
  (is-clj (implied-atomic?
            (clj
              (parse-filter
                '(| (! ':Red   tmap [(Key :right) (Key :left) (Key :tree)]) 
                    (! ':Black tmap [(Key :tree)]) 
                    (! ':Red   tmap [(Key :right) (Key :tree)]) 
                    (! ':Red   tmap [(Key :left)  (Key :tree)]))))
            (clj 
              (parse-filter
                '(| (! ':Red   tmap [(Key :right) (Key :left) (Key :tree)]) 
                    (! ':Black tmap [(Key :tree)]) 
                    (! ':Red   tmap [(Key :right) (Key :tree)])) ))
            ))

  (is-clj 
    (let [f1 (clj 
               (parse-filter
                 '(| (! ':Red   tmap [(Key :right) (Key :left) (Key :tree)]) 
                     (! ':Black tmap [(Key :tree)]) 
                     (! ':Red   tmap [(Key :right) (Key :tree)]))))
          f2 (clj
               (parse-filter
                 '(| (! ':Red   tmap [(Key :right) (Key :left) (Key :tree)]) 
                     (! ':Black tmap [(Key :tree)]) 
                     (! ':Red   tmap [(Key :right) (Key :tree)]) 
                     (! ':Red   tmap [(Key :left)  (Key :tree)]))))]
      (= (-and f1 f2)
         (-and f2 f1)
         f1)))
  (is-clj 
    (not (implied-atomic?
           (parse-filter
             '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                 (! ':Black tmap [(Key :tree)]) 
                 (! ':Red   tmap [(Key :right) (Key :tree)])))
           (parse-filter
             '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                 (! ':Black tmap [(Key :tree)]) 
                 (! ':Red   tmap [(Key :right) (Key :tree)])
                 (! ':Red   tmap [(Key :left)  (Key :tree)]))))))
  (is-clj (implied-atomic?
                 (parse-filter
                   '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                       (! ':Black tmap [(Key :tree)]) 
                       (! ':Red   tmap [(Key :right) (Key :tree)]) 
                       (! ':Red   tmap [(Key :left)  (Key :tree)])))
                 (parse-filter
                   '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                       (! ':Black tmap [(Key :tree)]) 
                       (! ':Red   tmap [(Key :right) (Key :tree)])))))
  (is-clj (= (clj
               (-and
                 (clj
                   (parse-filter
                     '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                         (! ':Black tmap [(Key :tree)]) 
                         (! ':Red   tmap [(Key :right) (Key :tree)]))))
                 (clj
                   (parse-filter
                     '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                         (! ':Black tmap [(Key :tree)]) 
                         (! ':Red   tmap [(Key :right) (Key :tree)]) 
                         (! ':Red   tmap [(Key :left)  (Key :tree)]))))))
             (clj
               (-and
                 (clj
                   (parse-filter
                     '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                         (! ':Black tmap [(Key :tree)]) 
                         (! ':Red   tmap [(Key :right) (Key :tree)]) 
                         (! ':Red   tmap [(Key :left)  (Key :tree)]))))
                 (clj
                   (parse-filter
                     '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                         (! ':Black tmap [(Key :tree)]) 
                         (! ':Red   tmap [(Key :right) (Key :tree)]))))))
             (parse-filter
               '(| (! ':Red   tmap [(Key :right) (Key :right) (Key :tree)]) 
                   (! ':Black tmap [(Key :tree)]) 
                   (! ':Red   tmap [(Key :right) (Key :tree)])))))
  )


(deftest combine-props-test
  (is-clj (= (map set (update/combine-props [(ImpFilter-maker (-not-filter -false 'a)
                                                              (-filter -true 'b))]
                                            [(-not-filter (Un -nil -false) 'a)]
                                            (atom true)))
             [#{} #{(-not-filter (Un -nil -false) 'a)
                    (-filter -true 'b)}])))

(deftest env+-test
  ;test basic TypeFilter
  ;update a from clojure.core.typed/Any to (t/Value :a)
  (is-clj (let [props [(-filter (-val :a) 'a)]
            flag (atom true)]
        (and (= (let [env {'a -any}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (-val :a)} props))
             @flag)))
  ;test positive KeyPE
  ;update a from (t/U (t/HMap :mandatory {:op :if}) (t/HMap :mandatory {:op :var})) => (t/HMap :mandatory {:op :if})
  (is-clj (let [props [(-filter (-val :if) 'a [(-kpe :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (make-HMap :mandatory {(-val :op) (-val :if)})
                                  (make-HMap :mandatory {(-val :op) (-val :var)}))}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (make-HMap :mandatory {(-val :op) (-val :if)})} props))
             @flag)))
  ;test negative KeyPE
  (is-clj (let [props [(-not-filter (-val :if) 'a [(-kpe :op)])]
            flag (atom true)]
        (and (= (let [env {'a (Un (make-HMap :mandatory {(-val :op) (-val :if)})
                                  (make-HMap :mandatory {(-val :op) (-val :var)}))}
                      lenv (-PropEnv env props)]
                  (env+ lenv [] flag))
                (-PropEnv {'a (make-HMap :mandatory {(-val :op) (-val :var)})} props))
             @flag)))
  ;test impfilter
  (clj (let [{:keys [l props]}
             (env+ (-PropEnv {'a (Un -false -true) 'b (Un -nil -true)}
                             [(ImpFilter-maker (-not-filter -false 'a)
                                               (-filter -true 'b))])
                   [(-not-filter (Un -nil -false) 'a)]
                   (atom true))]
         (is (= l {'a -true, 'b -true}))
         (is (= (set props)
                #{(-not-filter (Un -nil -false) 'a)
                  (-filter -true 'b)}))))
  ; more complex impfilter
  (is-with-aliases
    (=
     (-PropEnv {'and1 -false
                'tmap (make-HMap :mandatory {(-val :type) (-val :MapStruct2)
                                             (-val :b) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)})}
               #{(-not-filter (-val :MapStruct1)
                              'tmap
                              [(-kpe :type)])
                 (-filter (Un -false -nil)
                          'and1)
                 (ImpFilter-maker (-not-filter (Un -nil -false) 'and1)
                                  (-filter (-val :MapStruct1)
                                           'tmap
                                           [(-kpe :type)]))})
     (env+ (-PropEnv {'and1 (Un -false -true)
                      'tmap (Name-maker 'clojure.core.typed.test.util-aliases/UnionName)}
                     [(ImpFilter-maker (-filter (Un -nil -false) 'and1)
                                       (-not-filter (-val :MapStruct1)
                                                    'tmap
                                                    [(-kpe :type)]))
                      (ImpFilter-maker (-not-filter (Un -nil -false) 'and1)
                                       (-filter (-val :MapStruct1)
                                                'tmap
                                                [(-kpe :type)]))])
           [(-filter (Un -nil -false) 'and1)]
           (atom true))))
  ; refine a subtype
  (is-clj (= (:l (env+ (-PropEnv {'and1 (RClass-of Seqable [-any])} [])
                       [(-filter (RClass-of IPersistentVector [-any]) 'and1)]
                       (atom true)))
             {'and1 (RClass-of IPersistentVector [-any])}))
  ; bottom preserved
  (is-clj (let [a (atom true)]
            (env+ (-PropEnv {'foo -any} []) [-bot] a)
            (false? @a))))

;FIXME all these tests relate to CTYP-24
(deftest destructuring-special-ops
  ;FIXME for destructuring rest args
;  (is-clj (= (tc-t (let [a '(a b)]
;                 (seq? a)))
;         (ret -true (-true-filter) -empty)))
  (is-clj (= (-> 
               (tc-t (let [a {:a 1}]
                       (if (seq? a)
                         (apply (clojure.core.typed/inst hash-map t/Keyword Number) a)
                         a)))
               ret-t)
             (-complete-hmap {(-val :a) (-val 1)})))
  (is-clj (= (tc-t (fn [{a :a} :- (t/HMap :mandatory {:a (t/Value 1)})]
                     a))
             (ret (make-FnIntersection 
                    (make-Function
                      [(make-HMap :mandatory {(-val :a) (-val 1)})]
                      (-val 1) 
                      :filter (-true-filter)
                      :object (-path [(-kpe :a)] 0)))
                  (-FS -top -bot)
                  -empty)))
  ;FIXME inferred filters are bit messy, but should be (-FS -bot (! t/Seq 0))
  #_(is-with-aliases (= (-> (tc-t (clojure.core.typed/fn [a :- clojure.core.typed.test.util-aliases/UnionName]
                                    (seq? a)))
           ret-t)
         (make-FnIntersection
           (Function-maker [(Name-maker 'clojure.core.typed.test.util-aliases/UnionName)]
                         (make-Result -false 
                                      ;FIXME why isn't this (-FS -bot (-not-filter (RClass-of ISeq [-any]) 0)) ?
                                      (-FS -bot -top)
                                      -empty)
                         nil nil nil nil nil))))
  (is-clj (= (tc-t (let [{a :a} {:a 1}]
                 a))
             (ret (-val 1) 
                  (-true-filter)
                  -empty)))
  ;FIXME should be (-FS -bot (! ISeq 0))
  #_(is-clj (= (tc-t (clojure.core.typed/fn [a :- (t/HMap :mandatory {:a (t/Value 1)})]
                               (seq? a)))
         (ret (make-FnIntersection
                (Function-maker [(make-HMap :mandatory {(-val :a) (-val 1)})]
                              (make-Result -false (-false-filter) -empty)
                              nil nil nil nil nil))
              (-FS -top -bot)
              -empty)))
  ;roughly the macroexpansion of map destructuring
  ;FIXME
  #_(is-clj (= (tc-t (clojure.core.typed/fn 
                 [map-param :- clojure.core.typed.test.rbt-types/badRight]
                 (when (and (= :Black (-> map-param :tree))
                            (= :Red (-> map-param :left :tree))
                            (= :Red (-> map-param :left :right :tree)))
                   (let [map1 map-param
                         map1
                         (if (clojure.core/seq? map1)
                           (clojure.core/apply clojure.core/hash-map map1)
                           map1)

                         mapr (clojure.core/get map1 :right)
                         mapr
                         (if (clojure.core/seq? mapr)
                           (clojure.core/apply clojure.core/hash-map mapr)
                           mapr)

                         maprl (clojure.core/get mapr :left)
                         ;_ (print-env "maprl")
                         maprl
                         (if (clojure.core/seq? maprl)
                           (clojure.core/apply clojure.core/hash-map maprl)
                           maprl)]
                     maprl))))))
  ;destructuring a variable of union type
  (is-clj (= (ety (fn [{a :a} :- (t/U (t/HMap :mandatory {:a (t/Value 1)})
                                    (t/HMap :mandatory {:b (t/Value 2)}))]
                    a))
             (make-FnIntersection 
               (make-Function [(Un (make-HMap :mandatory {(-val :a) (-val 1)})
                                   (make-HMap :mandatory {(-val :b) (-val 2)}))]
                              -any
                              :filter (-FS (-and (-not-filter (Un -nil -false) 0 [(-kpe :a)])
                                                  (-filter 
                                                    (parse-clj
                                                      `(t/U (t/HMap :mandatory {:a (t/Val 1)}) 
                                                          (t/HMap :mandatory {:a t/Any, :b (t/Val 2)})))
                                                    0))
                                            (-filter (Un -nil -false) 0 [(-kpe :a)]))
                              :object (-path [(-kpe :a)] 0))))))

(deftest Name-resolve-test
  (is-with-aliases (= (tc-t (fn [tmap :- clojure.core.typed.test.util-aliases/MyName]
                              ;call to (apply hash-map tmap) should be eliminated
                              (let [{e :a} tmap]
                                e)))
                      (ret (make-FnIntersection 
                             (make-Function
                               [(Name-maker 'clojure.core.typed.test.util-aliases/MyName)]
                               (-val 1) 
                               :filter (-true-filter)
                               :object (-path [(-kpe :a)] 0)))
                           (-true-filter))))
  (is-with-aliases (= (tc-t (fn [tmap :- clojure.core.typed.test.util-aliases/MapName]
                              (let [{e :a} tmap]
                                (assoc e :c :b))))
                      (ret (make-FnIntersection
                             (make-Function
                               [(Name-maker 'clojure.core.typed.test.util-aliases/MapName)]
                               (make-HMap :mandatory {(-val :a) (-val 1)
                                                      (-val :c) (-val :b)})
                               :filter (-true-filter)))
                           (-true-filter))))
  ; Name representing union of two maps, both with :type key
  (is-with-aliases (subtype? 
                     (-> (tc-t (fn [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                 (:type tmap)))
                         ret-t)
                     (parse-type 
                       `[clojure.core.typed.test.util-aliases/UnionName :-> (t/U (t/Value :MapStruct2)
                                                                               (t/Value :MapStruct1))])))
  ; using = to derive paths
  (is-with-aliases (subtype? 
                     (-> (tc-t (fn [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                 (= :MapStruct1 (:type tmap))))
                         ret-t)
                     (make-FnIntersection 
                       (make-Function 
                         [(Name-maker 'clojure.core.typed.test.util-aliases/UnionName)]
                         (Un -false -true)
                         :filter (let [t (-val :MapStruct1)
                                       path [(-kpe :type)]]
                                   (-FS (-and 
                                          (-filter (make-HMap :mandatory {(-val :type) (-val :MapStruct1)
                                                           (-val :a) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)})
                                                   0)
                                          (-filter (-val :MapStruct1) 0 path)
                                          (-filter t 0 path))
                                        (-not-filter t 0 path)))))))
  ; using filters derived by =
  (is-with-aliases (subtype? (-> (tc-t (fn [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                                         (if (= :MapStruct1 (:type tmap))
                                           (:a tmap)
                                           (:b tmap))))
                                 ret-t)
                             (parse-type 
                               `[clojure.core.typed.test.util-aliases/UnionName :-> clojure.core.typed.test.util-aliases/MyName])))
  ; following paths with test of conjuncts
  (is-clj (= (tc-t (fn [tmap :- clojure.core.typed.test.util-aliases/UnionName]
                     ; (and (= :MapStruct1 (-> tmap :type))
                     ;      (= 1 1))
                     (if (print-filterset 
                           "final filters"
                           (let [and1 (print-filterset
                                        "first and1"
                                        (= :MapStruct1 (-> tmap :type)))]
                             (print-env "first conjunct")
                             (print-filterset
                               "second and1"
                               (if (print-filterset
                                     "second test"
                                     and1)
                                 (do (print-env "second conjunct")
                                     (print-filterset
                                       "third and1"
                                       (= 1 1)))
                                 (do (print-env "fail conjunct")
                                     (print-filterset
                                       "fail and1"
                                       and1))))))
                       (do (print-env "follow then")
                           (assoc tmap :c :d))
                       1)))
             (ret (make-FnIntersection
                    (make-Function
                      [(Name-maker 'clojure.core.typed.test.util-aliases/UnionName)]
                      (Un (-val 1)
                          (make-HMap :mandatory {(-val :type) (-val :MapStruct1)
                                                 (-val :c) (-val :d)
                                                 (-val :a) (Name-maker 'clojure.core.typed.test.util-aliases/MyName)}))
                      :filter (-true-filter)))
                  (-true-filter) -empty))))


(deftest assoc-test
  (is-clj (= (tc-t (assoc {} :a :b))
             (ret (-complete-hmap {(-val :a) (-val :b)})
                  (-FS -top -bot)
                  -empty)))
  ;see `invoke-special` for assoc for TODO
  ;FIXME
  #_(is-clj (= (-> (tc-t (-> (fn [m]
                         (assoc m :c 1))
                     (clojure.core.typed/ann-form [clojure.core.typed.test.core/SomeMap -> (t/U '{:a ':b :c '1}
                                                                         '{:b ':c :c '1})])))
           ret-t :types first :rng)
         (make-Result (Un (make-HMap :mandatory {(-val :a) (-val :b)
                                  (-val :c) (-val 1)})
                          (make-HMap :mandatory {(-val :b) (-val :c)
                                  (-val :c) (-val 1)}))
                      (-FS -top -bot)
                      -empty))))
         


(deftest check-get-keyword-invoke-test
  ;truth valued key
  (is-clj (= (tc-t (let [a {:a 1}]
                     (:a a)))
             (ret (-val 1) (-FS -top -top) -empty)))
  ;false valued key, a bit conservative in filters for now
  (is-clj (= (tc-t (let [a {:a nil}]
                     (:a a)))
             (ret -nil (-FS -bot -top) -empty)))
  ;multiple levels
  (is-clj (= (tc-t (let [a {:c {:a :b}}]
                     (-> a :c :a)))
             (ret (-val :b) (-FS -top -top) -empty)))
  (is-clj (= (tc-t (clojure.core/get {:a 1} :a))
             (tc-t (clojure.lang.RT/get {:a 1} :a))
             ;FIXME
             #_(tc-t ({:a 1} :a))
             (tc-t (:a {:a 1}))
             (ret (-val 1)
                  (-FS -top -top)
                  -empty)))
  ;keyword-invoke with default
  (is-tc-e (:a (ann-form {} (t/HMap :optional {:a Number}))
               'a)
           (t/U Number t/Symbol))
  ; absent keys, optional keys and complete HMaps
  (is-clj (= (tc-t (:o (ann-form {} (t/HMap :optional {:o (t/Val "v")} :complete? true))))
             (ret (Un -nil (-val "v")))))
  (is-clj (= (tc-t (:a (ann-form {} (t/HMap :absent-keys #{:a} :complete? true))))
             (ret -nil)))
  (is-clj (= (tc-t (:a (ann-form {} (t/HMap :absent-keys #{:a} :complete? false))))
             (ret -nil)))
  (is-clj (= (tc-t (:n (ann-form {} (t/HMap :complete? true))))
             (ret -nil)))
  (is-clj (= (tc-t (:n (ann-form {} (t/HMap :complete? false))))
             (ret -any))))

(defn print-cset [cs]
  (into {} (doall
             (for [ms (:maps cs)
                   [k v] (:fixed ms)]
               [k
                [(str (unparse-type (:S v))
                      " << "
                      (:X v)
                      " << "
                      (unparse-type (:T v)))]]))))

(deftest promote-demote-test
  (is-clj (= (promote-var (make-F 'x) '#{x})
         -any))
  (is-clj (= (demote-var (make-F 'x) '#{x})
         (Bottom)))
  (is-clj (= (promote-var (RClass-of clojure.lang.ISeq [(make-F 'x)]) '#{x})
         (RClass-of clojure.lang.ISeq [-any])))
  (is-clj (= (demote-var (RClass-of clojure.lang.ISeq [(make-F 'x)]) '#{x})
             (RClass-of clojure.lang.ISeq [(Bottom)]))))

(deftest variances-test
  (is-clj (= (fv-variances (make-F 'x))
         '{x :covariant}))
  (is-clj (= (fv-variances -any)
         '{}))
  (is-clj (= (fv-variances 
           (make-Function [] (RClass-of Atom [(make-F 'a) (make-F 'a)])))
         '{a :invariant}))
  (is-clj (= (fv-variances 
           (make-Function [] (RClass-of Atom [-any (make-F 'a)])))
         '{a :covariant}))
  (is-clj (= (fv-variances 
           (make-Function [] (RClass-of Atom [(make-F 'a) -any])))
         '{a :contravariant})))


(deftest fv-test
  (is-clj (= (fv (make-F 'x))
         '#{x})))

(deftest fi-test
  (is-clj (empty? (fi (make-F 'x)))))

(deftest cs-gen-test
  (is-clj (= (cs-gen #{} ;V
                     (zipmap '[x y] (repeat no-bounds)) ;X
                     {} ;Y
                     (-val 1) ;S
                     (make-F 'x)) ;T
             (cset-maker [(make-cset-entry {'x (c-maker (-val 1) 'x -any no-bounds)
                                        'y (c-maker (Un) 'y -any no-bounds)})])))
  ;intersections correctly inferred
  (is-clj (= (cs-gen '#{} {'x no-bounds} '{} 
                     (-hvec [(RClass-of Number)])
                     (In (RClass-of Seqable [(make-F 'x)]) (make-CountRange 1)))
             (cset-maker [(make-cset-entry {'x (c-maker (RClass-of Number) 'x -any no-bounds)})])))
;correct RClass ancestor inference
  (is-clj (= (cs-gen #{} {'x no-bounds} {} 
                     (RClass-of IPersistentVector [(RClass-of Number)])
                     (RClass-of Seqable [(make-F 'x)]))
             (cset-maker [(make-cset-entry {'x (c-maker (RClass-of Number) 'x -any no-bounds)})]))))

(deftest subst-gen-test
  (let [cs (clj (cs-gen #{} ;V
                        (zipmap '[x y] (repeat no-bounds)) ;X
                        {} ;Y
                        (-val 1) ;S
                        (make-F 'x)))]
    (is-clj (= (subst-gen cs #{} (make-F 'x))
           {'x (t-subst-maker (-val 1) no-bounds)
            'y (t-subst-maker (Un) no-bounds)}))))

(deftest infer-test
  (is-clj (= (cgen/infer (zipmap '[x y] (repeat no-bounds)) ;tv env
                         {}
                         [(-val 1) (-val 2)] ;actual
                         [(make-F 'x) (make-F 'y)] ;expected
                         (make-F 'x)) ;result
             {'x (crep/t-subst-maker (-val 1)
                                     no-bounds)
              'y (crep/t-subst-maker (-val 2)
                                     no-bounds)}))
  (is-clj (= (cgen/infer {'x no-bounds} ;tv env
                         {}
                         [(RClass-of IPersistentVector [(Un (-val 1) (-val 2) (-val 3))])] ;actual
                         [(RClass-of Seqable [(make-F 'x)])] ;expected
                         (RClass-of clojure.lang.ASeq [(make-F 'x)])) ;result
             {'x (crep/t-subst-maker (Un (-val 1) (-val 2) (-val 3))
                                     no-bounds)})) 
  (is-clj (= (cgen/infer {'x no-bounds} ;tv env
                         {}
                         [(-hvec [(-val 1) (-val 2) (-val 3)])] ;actual
                         [(RClass-of Seqable [(make-F 'x)])] ;expected
                         (RClass-of clojure.lang.ASeq [(make-F 'x)])) ;result
             {'x (crep/t-subst-maker (Un (-val 1) (-val 2) (-val 3))
                                     no-bounds)})))

(deftest arith-test
  (is-clj (subtype? (:t (tc-t (+)))
                    (RClass-of Number)))
  (is-clj (subtype? (:t (tc-t (+ 1 2)))
                    (RClass-of Number)))
  ;wrap in thunks to prevent evaluation
  (is (err/top-level-error-thrown? (cf (fn [] (+ 1 2 "a")))))
  (is (err/top-level-error-thrown? (cf (fn [] (-)))))
  (is (err/top-level-error-thrown? (cf (fn [] (/))))))

(deftest tc-constructor-test
  (is-clj (= (tc-t (Exception. "a"))
         (ret (RClass-of Exception)
              (-FS -top -bot)
              (EmptyObject-maker)))))

(deftest tc-throw-test
  (is-clj (subtype? (:t (tc-t (fn [] (throw (Exception. "a")))))
                (make-FnIntersection
                  (make-Function [] (Un))))))

(deftest first-seq-test
  (is-clj (clj (subtype? (ret-t (tc-t (first [1 1 1])))
                     (Un -nil (RClass-of Number)))))
  (is-clj (subtype? (In (RClass-of clojure.lang.PersistentList [-any])
                    (make-CountRange 1))
                (In (RClass-of Seqable [-any])
                    (make-CountRange 1))))
  (is-clj (subtype? (ret-t (tc-t (let [l [1 2 3]]
                               (if (seq l)
                                 (first l)
                                 (throw (Exception. "Error"))))))
                (RClass-of Number)))
  (is-clj (= (tc-t (first [1]))
         (ret (-val 1))))
  (is-clj (= (tc-t (first []))
         (ret -nil)))
  (is-clj (subtype? (ret-t (tc-t (first [1 2 3])))
                (RClass-of Number))))

(deftest intersection-maker-test
  (is-clj (= (In -nil (-val 1))
         (Un)))
  (is-clj (clj 
        (= (In (RClass-of Seqable [-any])
               -nil)
           (Un)))))
;FIXME
;  (is-clj (= (In (RClass-of Number)
;             (RClass-of t/Symbol))
;         (Un)))
;  (is-clj (= (In (RClass-of clojure.lang.APersistentMap [-any -any])
;             (RClass-of clojure.lang.Sorted))
;         (make-Intersection 
;           #{(RClass-of clojure.lang.APersistentMap [-any -any])
;             (RClass-of clojure.lang.Sorted)}))))
;



(deftest names-expansion-test
  (is (do
        (tc-e (do
                (defalias MyAlias
                  (t/U nil (t/HMap :mandatory {:a Number})))
                (-> nil (ann-form MyAlias) (ann-form t/Any)))))))

(deftest ccfind-test
  (is-clj
    (subtype? (-> (tc-t (fn [a :- (t/Map Long String)]
                          (find a 1)))
                  :t :types first :rng :t)
              (Un (-hvec [(RClass-of Long) (RClass-of String)])
                  -nil))))

(deftest map-infer-test
  (is-clj (subtype? (ety (map + [1 2]))
                    (RClass-of Seqable [(RClass-of Number)])))
  (is-clj (subtype? (ety (map + [1 2] [1 2] [4 5] [6 7] [4 4] [3 4]))
                    (RClass-of Seqable [(RClass-of Number)])))
  (is-tc-err (map + [1 2] [1 2] [4 5] [6 7] [4 4] {3 4}))
  (is-tc-err (map + [1 2] [1 2] [4 5] [6 7] [4 4] #{'a 4})))

(deftest ann-form-test
  (is-clj (= (ety (ann-form (atom 1) (t/Atom1 t/Num)))
             (parse-type `(t/Atom1 t/Num)))))

(deftest atom-ops-test
  (is-clj (subtype? (ety
                      (reset!
                        (ann-form (atom 1) (t/Atom1 t/Num))
                        10.1))
                    (RClass-of Number))))

(deftest apply-test
  ;conservative while not tracking keys "not" in a hmap
  (is-clj (subtype? (ety (apply merge [{:a 1}]))
                    (Un -nil (RClass-of IPersistentMap [-any -any])))))

(deftest destructuring-test
  ;Vector destructuring with :as
  (is-clj (subtype? (ety (let [[a b :as c] :- (t/Vec t/Num), [1 2]]
                           [a b c]))
                    (-hvec [(Un -nil (RClass-of Number))
                            (Un -nil (RClass-of Number))
                            (RClass-of Seqable [(RClass-of Number)])])))
  (is-clj (= (ety (let [[a b :as c] [1 2]] 
                    [a b c]))
             (-hvec [(-val 1)
                     (-val 2)
                     (-hvec [(-val 1) (-val 2)]
                            :filters [(-true-filter) (-true-filter)])]
                    :filters [(-true-filter) (-true-filter) (-true-filter)])))
  ;Map destructuring of vector
  ;FIXME needs implementing, but gives a decent error msg
  #_(is-clj (= (ret-t (tc-t (let [{a 0 b 1 :as c} [1 2]] 
                        [a b c])))
         (-hvec [(-val 1)
                 (-val 2)
                 (-hvec [(-val 1) (-val 2)])]))))

(deftest vararg-subtyping-test
  (is-clj (subtype? (parse-type '[nil * -> nil])
                    (parse-type '[nil -> nil])))
  (is-cf (clojure.core.typed/ann-form (clojure.core.typed/inst merge clojure.core.typed/Any clojure.core.typed/Any) [nil -> nil])))

(deftest poly-filter-test
  (is-clj (both-subtype? 
            (ety (let [a :- (t/Coll t/Int), [1]]
                   (if (seq a)
                     (first a)
                     'a)))
            (parse-type `(t/U t/AnyInteger (t/Value ~'a))))))

(deftest type-fn-test 
  (is-clj (clj
            (= (with-bounded-frees {(make-F 'm) (-bounds (parse-type `(t/TFn [[~'x :variance :covariant]] t/Any))
                                                         (parse-type `(t/TFn [[~'x :variance :covariant]] t/Nothing)) )}
                 (funapp/check-funapp
                   (ana/ast-for-form ''a) ;dummy
                   [(ana/ast-for-form 1)];dummy
                   (ret (parse-type 
                          `(t/All [~'x]
                                  [~'x :-> (~'m ~'x)])))
                   [(ret -nil)]
                   nil))
               (ret (TApp-maker (make-F 'm) [-nil]))))))

;TODO how to handle casts. CTYP-12
;Also need tc-t to bind *delayed-errors*
#_(deftest prims-test
  (is-clj (= (ret-t (tc-t (Math/sqrt 1)))
         (parse-type 'double))))

(deftest hmap-subtype
  (is-tc-e {} (clojure.lang.APersistentMap t/Any t/Any)))

;; `do` is special at the top level, tc-ignore should expand out to `do`
;; THIS IS A TOP-LEVEL TEST, DON'T REMOVE
(tc-ignore
 (defprotocol some-proto (some-proto-method [_]))
 ;; if tc-ignore did not expand to a `do`, this would fail with some-proto-method
 ;; being undefined
 some-proto-method)
;; END TOP-LEVEL TEST

(deftest class-pathelem-test
  (is-clj (= (-> (ety #(class %))
                 :types first :rng Result-object*)
             (-path [(ClassPE-maker)] 0)))
  (is-clj (subtype? 
            (ety 
              #(= Number (class %)))
            (FnIntersection-maker
              [(make-Function
                 [-any]
                 (RClass-of 'boolean)
                 :filter (-FS (-and (-filter (-val Number) 0 [(ClassPE-maker)])
                                    ;the important filter, updates first argument to be Number if predicate is true
                                    (-filter (RClass-of Number) 0))
                              (-not-filter (-val Number) 0 [(ClassPE-maker)])))]))))

;TODO ^--
;(-> (tc-t #(let [a (class %)]
;             (if a
;               true
;               false)))
;  ret-t unparse-type)

(deftest map-literal-test
  (is-tc-e {:bar :b}
           '{:bar ':b})
  ;correctly generalise
  (is-tc-e {(ann-form :bar t/Kw) :b}
           (t/Map t/Kw ':b)))

(deftest isa-test
  (is-tc-e (isa? 1 1))
  (is-tc-e (let [a :- t/Any 1
                 b :- t/Any 2]
             (assert (isa? [(class a) (class b)] [Number Number]))
             (+ a b)))
  (is-tc-err #(let [a :- t/Any 1
                    b :- t/Any 2]
                (assert (isa? [(class a) (class b) 1] [Number Number]))
                (+ a b)))
  (is-tc-e (isa? {:parents {} :ancestors {} :descendants {}} 1 1))
  (is-tc-e #(isa? (class %) Number))
  (is (= (ret (parse-clj `(t/U (t/Value 1) (t/Value 2)))
              (-true-filter))
         (tc-e (let [m {:a :b}
                     a :- t/Any 1
                     b :- t/Any 2]
                 (if (isa? (:a m) (ann-form 1 Long))
                   1
                   2)))))
  (is (= (ret (parse-clj `(t/U (t/Value 1) (t/Value 2)))
              (-true-filter))
         (tc-e
           (let [c clojure.lang.Keyword]
             (if (isa? c Object)
               (do (print-env "then")
                   1)
               (do (print-env "else")
                   2))))))
  (is-tc-err (do (ann nil?? (t/Pred nil))
                 (cc/defn nil?? [x]
                   (not (isa? (-> x class class class class) Object)))))
  (is-tc-e (do (ann rnil [t/Any -> nil])
               (cc/defn rnil [x]
                 (when (isa? (-> x class class class class class class class class class) nil)
                   x))))
  (is-tc-e (do (ann robject [t/Any -> Object])
               (cc/defn robject [x]
                 (if (isa? (-> x class class class class class class class) Object)
                   x
                   (Object.)))))
  (is-tc-e (do (ann rnilobject [t/Any -> nil])
               (cc/defn rnilobject [x]
                 (if (not (isa? (-> x class class) Object))
                   x
                   nil))))
  (is-tc-err (do (ann robject [t/Any -> nil])
                 (cc/defn robject [x]
                   (if (isa? (-> x class class) Object)
                     x
                     nil))))
  (is-tc-e (do (defn minc [x :- (t/U nil t/Num)]
                 (if (isa? (-> x class class) Class)
                   (inc x)
                   0))))
  ;(is-tc-e (do (ann nil?? (t/Pred nil))
  ;             (cc/defn nil?? [x]
  ;               (not (isa? (-> x class class) Object)))))
  )

(deftest equiv-filters-test
  (is-tc-e (do (ann a? (t/Pred ':a))
               (cc/defn a? [x]
                 (= :a x))))
  (is-tc-err (do (ann a? (t/Pred ':b))
                 (cc/defn a? [x]
                   (= :a x)))))

(deftest flow-assert-test
  (is-clj (subtype?
            (ety (fn [a]
                   {:pre [(integer? a)]}
                   a))
            (parse-type `[t/Any :-> t/AnyInteger])))
  (is-clj (subtype? 
            (ety (let [a (read-string "1")
                       _ (assert (integer? a))]
                   (+ 10 a)))
            (parse-type `t/AnyInteger)))
  ;postconditions
  (is-clj (subtype?
            (ety (fn [a]
                   {:post [(vector? %)]}
                   a))
            (parse-type `[t/Any :-> (t/Vec t/Any)]))))

(deftest complete-hmap-test
  (is-clj (subtype? (-complete-hmap {})
                    (parse-type `(clojure.lang.APersistentMap t/Nothing t/Nothing))))
  (is-clj (not
            (subtype? (make-HMap :mandatory {})
                      (parse-type `(clojure.lang.APersistentMap t/Nothing t/Nothing)))))
  (is-clj (subtype? (-> (tc-t {}) ret-t)
                    (parse-type `(clojure.lang.APersistentMap t/Nothing t/Nothing)))))

(deftest dotted-on-left-test
  (is-tc-e (memoize (fn []))))

(deftest string-as-seqable-test
  (is-clj (subtype? 
            (RClass-of String)
            (RClass-of Seqable [-any])))
  (is-clj (subtype? 
            (-val "a")
            (RClass-of Seqable [-any])))
  (is-tc-e (seq "a"))
  (is-tc-e (first "a") Character)
  (is-tc-e (first (ann-form "a" String)) (t/Option Character)))

(deftest string-as-indexed-test
  (is (sub? String (clojure.lang.Indexed clojure.core.typed/Any))))

(deftest recursive-cf-test
  (is (thrown? Exception
               (tc-e (cf 1 Number)))))

(deftest intersection-simplify-test
  (is-tc-e (let [a (ann-form [] (t/U (Extends [Number] :without [(clojure.lang.IPersistentVector Number)])
                                   (clojure.lang.IPersistentVector Number)))]
             (when (vector? a)
               a))
           (t/U nil (clojure.lang.IPersistentVector Number))))

(deftest kw-args-test
  (is (check-ns 'clojure.core.typed.test.kw-args)))

(deftest get-APersistentMap-test
  (is-tc-e (get (ann-form {} (clojure.lang.APersistentMap t/Num t/Num)) :a)
           (t/U nil t/Num)))

(deftest enum-field-non-nilable-test
  (is-tc-e (java.util.concurrent.TimeUnit/NANOSECONDS)
           java.util.concurrent.TimeUnit))

;;;;

;FIXME CTYP-71
;(deftest let-filter-unscoping-test
;  (is-cf (fn [a]
;            (and (< 1 2) a))
;         [(t/U nil Number) -> clojure.core.typed/Any :filters {:then (is Number 0)}]))

(deftest map-literal-containing-funapp-test
  (is-tc-e {:bar (identity 1)}))

(deftest doseq-test
  (is-tc-e (doseq [a :- (t/U t/AnyInteger nil), [1 nil 2 3]
                   :when a]
             (inc a)))
  ;wrap in thunk to prevent evaluation
  (is-tc-err
    (fn []
      (doseq [a :- (t/U t/AnyInteger nil), [1 nil 2 3]]
        (inc a)))))

(deftest dotimes-test
  (is-tc-e (dotimes [i 100] (inc i)) nil)
  (is-tc-e (dotimes [i :- t/Num 100] (inc i)) nil)
  (is (let [a (atom 0)]
        (clojure.core.typed/dotimes
          [i 10]
          (swap! a inc))
        (= @a 10)))
  (is (let [a (atom 0)]
        (clojure.core.typed/dotimes
          [i :- t/Num, 10]
          (swap! a inc))
        (= @a 10))))

(deftest records-test
  (is (check-ns 'clojure.core.typed.test.records))
  (is (check-ns 'clojure.core.typed.test.records2)))

(deftest string-methods-test
  (is-tc-e (.toUpperCase "a") 
           String)
  (is-tc-e (.intern "a") 
           String))

(deftest loop-errors-test
  (is (caught-top-level-errors #{1}
        (cf (loop [a 1] a))))
  (is (caught-top-level-errors #{1}
        (cf (loop [a :- String, 1] a)))))

(deftest map-indexed-test
  (is-tc-e (map-indexed (inst vector t/Any t/AnyInteger Long) 
                        [1 2])
           (t/Seqable '[t/AnyInteger Long])))

(deftest letfn>-test
  (is-tc-e (letfn> [a :- [Number -> Number]
                    (a [b] (inc b))]
             (a 1))
           Number)
  ; erase objects
  (is-clj (= (tc-t (letfn> [a :- [Number -> Number]
                            (a [b] (inc b))]
                     a))
             (ret (parse-clj '[Number -> Number]) (-FS -top -bot) -empty)))
  ; annotation needed
  (is-tc-err (letfn> [(a [b] (inc b))]
               (a 1)))
  ;interdependent functions
  (is-tc-e (letfn> [a :- [Number -> Number]
                    (a [c] (b c))

                    b :- [Number -> Number]
                    (b [d] (do a d))]
             (a 1))
           Number))

;FIXME convert datatypes+records to RClasses
(deftest protocol-untyped-ancestor-test
  (is (check-ns 'clojure.core.typed.test.protocol-untyped-extend)))

(deftest kw-args-fail-test
  (is (caught-top-level-errors #{1}
        (check-ns 'clojure.core.typed.test.kw-args-undeclared-fail))))

(deftest filter-combine-test
  (is (check-ns 'clojure.core.typed.test.filter-combine)))

(deftest or-filter-simplify-test
  ;(| (is T  a)
  ;   (is T' a))
  ; simplifies to
  ;(is (t/U T T') a)
  (is-clj 
    (= (-or (-filter (RClass-of clojure.lang.Symbol) 'id)
            (-filter (RClass-of String) 'id))
       (-filter (Un (RClass-of clojure.lang.Symbol)
                    (RClass-of String))
                'id)))

  ;(| (is T  a pth)
  ;   (is T' a pth))
  ; simplifies to
  ;(is (t/U T T') a pth)
  (is-clj 
    (= (-or (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
            (-filter (RClass-of String) 'id [(-kpe :a)]))
       (-filter (Un (RClass-of clojure.lang.Symbol)
                    (RClass-of String))
                'id [(-kpe :a)])))
  
  ;(& (is T a pth)
  ;   (when (is T a pth)
  ;     (is T' 'b)))
  ;  simplifies to 
  ;(& (is T a pth)
  ;   (is T' 'b))
  ;  FIXME
;  (is-clj 
;    (= (-and (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
;             (-imp (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
;               (-filter (RClass-of String) 'id2 [(-kpe :a) (-kpe :b)])))
;       (-and (-filter (RClass-of clojure.lang.Symbol) 'id [(-kpe :a)])
;             (-filter (RClass-of String) 'id2 [(-kpe :a) (-kpe :b)]))))
  )

(deftest instance-field-test
  (is-tc-e (.ns ^clojure.lang.Var #'clojure.core/map))
  (is-tc-err (fn [] (.ns ^clojure.lang.Var 'a))))

(deftest instance-method-test
  (is-tc-e (fn [^Class a :- Class]
             (.getName a)))
  (is-tc-e (fn [a :- Class]
             (let [a a]
               (.getName a))))
  (is-tc-e (defn foo [a :- Class]
             (let [a a]
               (.getName a))))
  (is-tc-e (do
             (typed.clojure/ann Class->symbol [Class -> typed.clojure/Sym])
             (cc/defn Class->symbol [^Class cls]
               {:pre [(class? cls)]
                :post [(symbol? %)]}
               (symbol (.getName cls)))))
  (is-tc-e (fn [a :- Class]
             (cc/let [a a]
               (.getName a)))))

(deftest HMap-syntax-test
  (is-clj (= (parse-type `(t/HMap :absent-keys #{:op}))
             (make-HMap :absent-keys #{(-val :op)} :complete? false))))

(deftest map-filter-test
  (is-tc-e 
    (ann-form (fn [a] (:op a))
              [(t/U '{:op ':if} '{:op ':case})
               -> (t/U ':if ':case)
               :filters {:then (is (t/U ':case ':if) 0 [(Key :op)])
                         :else (| (is (t/HMap :absent-keys #{:op}) 0)
                                  (is (t/U false nil) 0 [(Key :op)]))}
               :object {:id 0
                        :path [(Key :op)]}]))
  ; {:then (is :if 0 [:op])
  ;  :else (| (! :if 0 [:op])
  ;           (is (t/HMap :absent-keys #{:op}) 0))}
  (is-tc-e #(= :if (:op %))
           [(t/U '{:op ':if} '{:op ':case})
            -> Boolean
            :filters {:then (& (is '{:op (t/Value :if)} 0)
                               (is ':if 0 [(Key :op)]))
                      :else (! ':if 0 [(Key :op)])}])
  (is-tc-e (fn [a :- (t/U '{:op ':if} '{:op ':case})
                b :- (t/U '{:op ':if} '{:op ':case})]
             (if (= :if (:op a))
               (= :case (:op b))
               false)))
  (is-tc-e (fn [a b] 
             (let [and__3941__auto__ (clojure.core/symbol? a)] 
               (if (print-filterset "test" and__3941__auto__)
                 (clojure.core/number? b) 
                 and__3941__auto__)))))

(deftest warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.warn-on-unannotated-var)))

(deftest number-ops-test
  (is-tc-e (min (Integer. 3) 10) Number))

(deftest ctor-infer-test
  (is-tc-e (java.io.File. "a"))
  (is-tc-e (let [a (or "a" "b")]
             (java.io.File. a)))
  (is-tc-e
    (fn [& {:keys [path] :or {path "foo"}}]
      (java.io.File. path))
    [& :optional {:path String} -> java.io.File])
  (is-tc-err
    (fn [& {:keys [path] :or {path "foo"}}]
      (java.io.File. path))
    [& :optional {:path t/Int} -> java.io.File]))

(deftest extends-test
  ; without extends: never returns a (IPV Number) because we can have
  ; a type (t/I (IPM clojure.core.typed/Any clojure.core.typed/Any) (IPV clojure.core.typed/Any))
  (is-tc-err
    (fn [a]
      (if (vector? a)
        a
        nil))
    [(t/U (t/Vec t/Num) (t/Map t/Any t/Any)) -> (t/U nil (t/Vec t/Num))])
  ; can use assertions to prove non-overlapping interfaces
  (is-tc-e
    (fn [a]
      {:pre [(or (and (vector? a)
                      (not (map? a)))
                 (and (map? a)
                      (not (vector? a))))]}
      (if (vector? a)
        a
        nil))
    [(t/U (t/Vec t/Num) (t/Map t/Any t/Any)) -> (t/U nil (t/Vec t/Num))])
  ; or use static types
  (is-tc-e 
    (fn [a]
      (if (vector? a)
        a
        nil))
    [(t/U (Extends [(clojure.lang.IPersistentVector Number)]
                 :without [(clojure.lang.IPersistentMap t/Any t/Any)])
        (Extends [(clojure.lang.IPersistentMap t/Any t/Any)]
                 :without [(clojure.lang.IPersistentVector t/Any)]))
     -> (t/U nil (clojure.lang.IPersistentVector Number))])
  ; technically it's ok to implement Number and IPM
  (is-tc-e 
    (fn [a]
      {:pre [(number? a)]}
      (print-env "a")
      (+ 1 a))
    [(clojure.lang.IPersistentMap t/Any t/Any) -> Number]))


(deftest set!-test
  (is (check-ns 'clojure.core.typed.test.set-bang)))

(deftest flow-unreachable-test
  ; this will always throw a runtime exception, so it's unreachable
  (is-tc-e 
    (fn [a] 
      {:pre [(symbol? a)]}
      (print-env "a") 
      (ann-form a t/Sym))
    [Long -> t/Sym]))

(deftest every?-update-test
  (is-tc-e (let [a (ann-form [] (t/U nil (t/Coll t/Any)))]
             (assert (every? number? a))
             a)
           (t/U nil (t/Coll Number)))
  #_
  (is-tc-e (fn [as :- (t/Coll (t/U nil (t/Coll Number)))] :- (t/Coll (t/Coll Number))
             (assert (every? (inst seq Number) as))
             as))
  )

; a sanity test for intersection cache collisions
(deftest intersect-cache-test
  (is-clj (=
       (Poly-body*
         ['foo1 'foo2]
         (Poly* '[x y] 
                [no-bounds no-bounds]
                (In (make-F 'x) (make-F 'y))))
       (In (make-F 'foo1) (make-F 'foo2)))))

(deftest CTYP-27-nth-inline-test
  (is-tc-e (fn [s] (clojure.lang.RT/nth s 0 nil))
           [nil -> nil])
  (is-tc-e (fn [s] (nth s 0 nil))
           [nil -> nil])
  (is-tc-e #(inc (first [1 2 3])))
  (is-tc-e #(let [[x & xs] [1 2 3]] (inc x))))

(deftest invoke-tfn-test
  (is-clj (inst/manual-inst (parse-type `(t/All [[~'x :< (t/TFn [[~'x :variance :covariant]] t/Any)]]
                                                (~'x t/Any)))
                            [(parse-type `(t/TFn [[~'x :variance :covariant]] Number))]
                            {}))
  (is-clj (inst/manual-inst (parse-type `(t/All [[~'x :< (t/TFn [[~'x :variance :covariant]] Number)]]
                                              (~'x t/Any)))
                            [(parse-type `(t/TFn [[~'x :variance :covariant]] Number))]
                            {}))
  (is-clj (inst/manual-inst (parse-type `(t/All [x#
                                                 [y# :< x#]] 
                                                t/Any))
                            [(parse-type `t/Any)
                             (parse-type `t/Any)]
                            {})))

#_(fully-resolve-type (parse-type '((clojure.core.typed/All [a] (t/TFn [[x :variance :covariant :< a]] a)) Number)))


#_(deftest filter-seq-test
  ;  TODO possible extension for filter
;  (is (cf (filter :a (clojure.core.typed/ann-form [] (clojure.lang.Seqable '{:b Number})))
;          (clojure.lang.Seqable '{:b Number :a clojure.core.typed/Any})))
  (is-tc-err (core/filter (inst identity (t/U nil Number)) [1 nil])
             (t/Seqable Number))
  (is-tc-e ((inst core/filter (t/U nil Number) nil) (inst identity (t/U nil Number)) [1 nil])
           (t/Seqable Number))
  (is-tc-e ((inst core/filter (t/U nil Number) nil) identity [1 nil])
           (t/Seqable Number))

  (is-tc-e (filter-identity :in (t/U nil Number) [1 nil])
           (t/Seqable Number))
  (is (= (filter-identity :in (t/U nil Number) [1 nil])
         [1]))

  (is-tc-e (filter-identity :in (t/U false nil Number) [1 nil])
           (t/Seqable Number))
  (is (= (filter-identity :in (t/U false nil Number) [1 nil])
         [1]))

  (is-tc-err (filter-identity :in Number [1 nil])
             (t/Seqable Number))
  (is (= (filter-identity :in Number [1 nil])
         [1]))

  (is-tc-e (let [filter (ann-form core/filter
                                  (clojure.core.typed/All [x y]
                                       [[x -> clojure.core.typed/Any :filters {:then (is y 0)}] 
                                        (t/U nil (t/Seqable x)) -> (t/Seq y)]))]
             (filter number? [1 nil]))
           (t/Seqable Number)))

#_(deftest remove-nil-test
  (is-tc-e (remove-nil :in Number [1 2 3])
           (t/Seqable Number))
  (is (= (remove-nil :in Number [1 2 3])
         [1 2 3]))

  (is-tc-e (remove-nil :in (t/U nil Number) [1 2 3])
           (t/Seqable Number))
  (is-tc-e (remove-nil :in (t/U false nil Number) [1 2 3])
           (t/Seqable (t/U false Number))))

#_(deftest remove-false-test
  (is-tc-e (remove-false :in Number [1 2 3])
           (t/Seqable Number))
  (is (= (remove-false :in Number [1 2 3])
         [1 2 3]))

  (is-tc-e (remove-false :in (t/U false Number) [1 2 3])
           (t/Seqable Number))
  (is-tc-e (remove-false :in (t/U false nil Number) [1 2 3])
           (t/Seqable (t/U nil Number)))
  (is-tc-err ((inst core/remove (t/U nil Number) nil) nil? [1 2 3 nil]) (t/Seq Number)))

; keeping if we decide to use more expressive type for conj
;(deftest extensible-conj-test
;  (is (cf ((clojure.core.typed/inst conj Number (t/TFn [[x :variance :covariant]] x)
;                 (t/TFn [[x :variance :covariant]] (clojure.lang.PersistentHashSet x)))
;            (clojure.core.typed/ann-form #{} (clojure.lang.PersistentHashSet Number)) 1)
;          (clojure.lang.PersistentHashSet Number)))
;  ;FIXME
;#_(is (cf (conj
;            (clojure.core.typed/ann-form #{} (clojure.lang.PersistentHashSet Number)) 1)
;          (clojure.lang.PersistentHashSet Number))))
;(deftest subtype-ipcoll-test
;  (is-clj (sub? (clojure.lang.IPersistentCollection 
;              (clojure.lang.IMapEntry clojure.core.typed/Any clojure.core.typed/Any)
;              (t/TFn [[x :variance :covariant
;                     :< (t/U nil (clojure.lang.IMapEntry clojure.core.typed/Any clojure.core.typed/Any))]]
;                   x)
;              (clojure.core.typed/All [a1 b1]
;                   (t/TFn [[x :variance :covariant
;                          :< (t/U nil (clojure.lang.IMapEntry a1 b1))]]
;                        (clojure.lang.APersistentMap a1 b1))))
;            (clojure.core.typed/Coll clojure.core.typed/Any)))
;  (is-clj (sub? (t/TFn [[x :variance :covariant]]
;                   x)
;            (t/TFn [[x :variance :covariant]]
;                 clojure.core.typed/Any)))
;  (is-clj (sub? (t/TFn [[x :variance :covariant
;                     :< (t/U nil (clojure.lang.IMapEntry clojure.core.typed/Any clojure.core.typed/Any))]]
;                   x)
;            (t/TFn [[x :variance :covariant]]
;                 clojure.core.typed/Any)))
;  (is-clj (sub? (clojure.core.typed/All [a1 b1]
;                 (t/TFn [[x :variance :covariant
;                        :< (t/U nil (clojure.lang.IMapEntry a1 b1))]]
;                      (clojure.lang.APersistentMap a1 b1)))
;            (t/Rec [c]
;                 (t/TFn [[x :variance :covariant]] 
;                      (clojure.lang.IPersistentCollection 
;                        clojure.core.typed/Any 
;                        (t/TFn [[x :variance :covariant]] clojure.core.typed/Any) 
;                        c))))))


(deftest intersection-csgen-test
  (is-clj (clj (cs-gen #{} {'a no-bounds} {}
                   (In (RClass-of Seqable [(RClass-of Number)])
                       (make-CountRange 1))
                   (In (RClass-of Seqable [(make-F 'a)])
                       (make-CountRange 1))))))

(deftest iterable-as-seqable-test
  (is-cf (first (clojure.core.typed/ann-form [] (Iterable clojure.core.typed/Any)))))

; See CTYP-29 for discussion. f in (map f coll) needs to be only a single arity
; to help inference.
(deftest map-over-multiarity-fn-test
  (is-cf (map (clojure.core.typed/ann-form + [Number -> Number]) 
              (clojure.core.typed/ann-form [] (clojure.lang.Seqable Number))))
  (is-cf (map inc [(or (first (range)) 0) 1])
         (clojure.lang.Seqable clojure.core.typed/AnyInteger))
  (is-cf (fn [x] (map (clojure.core.typed/ann-form inc [Number -> Number]) [x 1]))
         [Number -> (clojure.lang.Seqable Number)])
  (is-clj (subtype? (ret-t (tc-t [(or (first (range)) 2) 1]))
                    (RClass-of Seqable [(RClass-of Number)])))
  (is-cf (fn [x] 
           (map (clojure.core.typed/ann-form inc [Number -> Number]) 
                [x 2 3])) 
         [Number -> (clojure.core.typed/Seq Number)]))

(deftest unannotated-datatype-test
  (is (check-ns 'clojure.core.typed.test.unannotated-datatype)))

;(deftest intersect-RClass-ancestors-test
;  (is-clj (= (In (RClass-of IPersistentSet [-any])
;             (RClass-of Seqable [(RClass-of Number)]))
;         (RClass-of IPersistentSet [-any]))))

(deftest munged-datatype-fields-test
  (is (check-ns 'clojure.core.typed.test.munge-record-field)))

(deftest mm-warn-on-unannotated-vars-test
  (is (check-ns 'clojure.core.typed.test.mm-warn-on-unannotated)))

(deftest HMap-parse-fail-test
  (is (err/tc-error-thrown? (clj (parse-type `(t/HMap :mandatory {:a t/Any} :absent-keys #{:a}))))))

(deftest HMap-absent-complete-test
  (is-clj (not (sub? (HMap :mandatory {:a Any}) (HMap :absent-keys #{:a}))))
  (is-clj (not (sub? (HMap :complete? false) (HMap :complete? true))))
  (is-clj (sub? (HMap :complete? false) (HMap :complete? false)))
  (is-clj (sub? (HMap :complete? true) (HMap :complete? false)))
  (is-clj (sub? (HMap :complete? true) (HMap :complete? true)))
  (is-clj (sub? (HMap :absent-keys #{:a :b}) (HMap :absent-keys #{:a})))
  (is-clj (not (sub? (HMap :absent-keys #{}) (HMap :absent-keys #{:a}))))
  (is-clj (not (sub? (HMap :absent-keys #{:a}) (HMap :absent-keys #{:a :b})))))

(deftest extend-record-to-protocol-test
  (is (check-ns 'clojure.core.typed.test.extend-record)))

(deftest hmap-smart-infer-test
  (is-tc-e {:a #(+ % 1)} (t/HMap :optional {:a [Number -> Number]})))

(deftest fnil-test
  (is-tc-e ((fnil + (ann-form 0 Number)) 2))
  ;;FIXME probably related to how we handle :invariant variables in subst-gen
  #_(is-cf ((fnil + 0) 2))
  (is-cf ((fnil + 0) nil))
  ;;FIXME probably related to how we handle :invariant variables in subst-gen
  #_
  (is-cf ((fnil (clojure.core.typed/ann-form + [Number * -> Number])
                0)
          2.2))
  ; can Typed Racket do better here?
  (is-tc-e ((fnil (clojure.core.typed/ann-form + [Number * -> Number])
                  (ann-form 0 Number))
            2.2))
)

;(cf (every? (fn [a] a) [1]))

;
(deftest csgen-combine-test
  (is-cf (map inc [0 1.1])
         (clojure.lang.Seqable Number))
  (is-cf (map (clojure.core.typed/inst vector clojure.core.typed/Any Number Number) [1] [2])
         (clojure.lang.Seqable '[Number Number])))

;FIXME uncomment after core.typed internals are being checked
;(deftest subtype-explosion-test
;  (is (sub? nil typed.cljc.checker.type-rep/TCType)))

(deftest var-as-function-test
  (is-cf #'+ [Number * -> Number])
  (is-cf (#'+ 1 2))
  (is (sub? (clojure.core.typed/Var1 [-> nil]) [-> nil])))

(deftest future-test
  (is-cf @(future 'a) clojure.lang.Symbol)
  (is-cf (future 'a) java.util.concurrent.Future))

(deftest ignore-macro-def-test
  (is-cf (defmacro foobar [])))

(deftest typed-deps-fail-gracefully-test
  (is-tc-err (clojure.core.typed/typed-deps no.exist.fail)))

(deftest def-expected-test
  (is-cf (do
           (clojure.core.typed/ann foo1 clojure.core.typed/Any)
           (clojure.core.typed/ann-form (def foo1 1) clojure.core.typed/Any)))
  (is-cf (do
           (clojure.core.typed/ann foo2 clojure.core.typed/Any)
           (clojure.core.typed/ann-form (def foo2) clojure.core.typed/Any))))

(deftest CTYP-42
  (is (check-ns 'clojure.core.typed.test.succeed.CTYP-42-record-extend-protocol)))

(deftest CTYP-48
  (is-tc-e (fn [a] (:a a))
           [t/Nothing -> t/Any]))

(deftest CTYP-49 
  (is (check-ns 'clojure.core.typed.test.succeed.CTYP49-unreachable)))

#_(deftest CTYP-47-Fn-as-IFn
  (is-cf (fn [] #())
          [-> clojure.lang.IFn]))

(deftest plain-defprotocol-test
  (is (check-ns 'clojure.core.typed.test.fail.CTYP-45)))

;; FIXME this doesn't seem to test what it claims
(deftest HMap-absent-key-update-test
  ;ensure absent keys are preserved when passed through occurrence typing's `update-with-filter`
  (is-tc-e
    (let [a :- (t/HMap :mandatory {:a t/Num}
                     :optional {:b t/Num,
                                :c t/Num})
          {:a 1}]
      (when (:b a)
        (ann-form a (t/HMap :mandatory {:a t/Num}
                          :optional {:b t/Num,
                                     :c t/Num}))))))

(deftest non-empty-map-test
  (is-tc-e (map inc [1 2 3])
           (t/NonEmptySeq t/Num)))

;CTYP-53
(deftest hmap-cast-test
  (is (both-subtype?
        (ety
          (fn
            [m :- (t/HMap)]
            (assert (:foo m))
            m))
        (parse-clj `['{} :-> '{:foo t/Any}
                     :filters {:then ~'tt
                               :else ~'ff}
                     :object {:id 0}])))
  (is (both-subtype? 
        (ety
          (fn
            [m :- (t/HMap)]
            :- (t/HMap :mandatory {:foo (clojure.core.typed/Vec clojure.core.typed/Any)})
            (assert (vector? (:foo m)))
            m))
        (parse-clj `[(t/HMap) :-> 
                     (t/HMap :mandatory {:foo (t/Vec t/Any)})
                     :filters {:then ~'tt
                               :else ~'ff}
                     :object {:id 0}])))
  (is (both-subtype? 
        (ety 
          (fn
            [m :- (t/HMap :mandatory {:bar t/Any})]
            (assert (nil? (:foo m)))
            m))
        (parse-clj `[(t/HMap :mandatory {:bar t/Any}) :-> 
                     (t/HMap :mandatory {:bar t/Any}
                           :optional {:foo nil})
                     :filters {:then ~'tt
                               :else ~'ff}
                     :object {:id 0}])))
  (is
    (both-subtype?
      (ety
        (fn
          [m :- '{}]
          (assert (not (vector? (:foo m))))
          m))
      (parse-clj `[(t/HMap) :-> 
                   ; not sure if this should simplify to (t/HMap)
                   (t/HMap :optional {:foo t/Any})
                   :filters {:then ~'tt
                             :else ~'ff}
                   :object {:id 0}])))
  (is 
    (clj
      (let [t1 (clj (update-with-filter
                      (parse-type `(t/HMap))
                      (parse-filter `(~'is (t/Vec t/Any) ~'m [(~'Key :foo)]))))
            t2 (clj (parse-type `(t/HMap :mandatory {:foo (t/Vec t/Any)})))]
        (both-subtype? t1 t2)))))

;CTYP-60
(deftest absent-keys-test
  (is (not (sub?-q `(t/HMap :mandatory {:a String}
                          :complete? true)
                   `(t/HMap :absent-keys #{:a}))))
  (is-tc-err {:a "a"} (t/HMap :absent-keys #{:a})))

;CTYP-61
(deftest hmap-assoc-test
  (is-tc-e (assoc (ann-form {} (t/HMap :optional {:a t/Any})) 
                  :b "v")
           (t/HMap :mandatory {:b (t/Value "v")} 
                 :optional {:a t/Any}))
  (is-tc-e (assoc (ann-form {} (t/HMap :optional {:a t/Any})) 
                  :a "v")
           (t/HMap :mandatory {:a (t/Value "v")})))

(deftest CTYP-37-defprotocol-better-error
  (is (check-ns 'clojure.core.typed.test.CTYP-37)))

(deftest invoke-dissoc-test
  ; complete dissocs
  (equal-types (dissoc {} :a)
               (HMap :mandatory {} :complete? true))
  
  (equal-types (dissoc {:a 6} :a)
               (HMap :mandatory {} :complete? true))
  
  (equal-types (dissoc {:a 6 :b 7} :a)
               (HMap :mandatory {:b '7} :complete? true))
  
  (equal-types (dissoc {:a 6 :b 7} :a :b)
               (HMap :mandatory {} :complete? true))
  
  (equal-types (dissoc {:a 6 :b 7 :c 8} :a :b)
               (HMap :mandatory {:c (Value 8)} :complete? true))
  
  ; incomplete dissocs
  (equal-types (dissoc (clojure.core.typed/ann-form {} (t/HMap)) :a)
               (HMap :absent-keys #{:a}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {} (t/HMap :optional {:a String})) :a)
               (HMap :absent-keys #{:a}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {} (t/HMap :optional {:a String})) :b)
               (HMap :optional {:a String} :absent-keys #{:b}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {:b 7} (t/HMap :mandatory {:b '7} :optional {:a String})) :a)
               (HMap :mandatory {:b (Value 7)} :absent-keys #{:a}))
  
  (equal-types (dissoc (clojure.core.typed/ann-form {:b 7} (t/HMap :optional {:a String :c String})) :a :b)
               (HMap :optional {:c String} :absent-keys #{:a :b})))

(deftest invoke-assoc-test
  
  ; HMaps
  (equal-types (assoc {} :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc nil :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil (t/U nil (HMap))) :a 5)
               (HMap :mandatory {:a '5}))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil (t/U nil (t/HMap :complete? true))) :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (t/HMap)) :a 5)
               (HMap :mandatory {:a '5}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (t/HMap :optional {:a clojure.core.typed/Any})) :b "v")
               (HMap :mandatory {:b (Value "v")} :optional {:a Any}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (t/HMap :optional {:a clojure.core.typed/Any})) :a "v")
               (HMap :mandatory {:a (Value "v")}))

  ;CTYP-79 resolve types properly in assoc
  (is (check-ns 'clojure.core.typed.test.hmap-resolve-assoc))
  
  ; HVecs
  (equal-types-noparse (assoc [] 0 1)
                       (-hvec [(-val 1)]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (assoc [3] 1 2)
                       (-hvec [(-val 3) (-val 2)]
                              :filters [(-true-filter)
                                        (-true-filter)]
                              :objects [-empty -empty]))
  
  (equal-types-noparse (assoc [0] 0 1)
                       (-hvec [(-val 1)]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (assoc [0] 0 (if (clojure.core.typed/ann-form 1 clojure.core.typed/Any) 1 2))
                       (-hvec [(Un (-val 1) (-val 2))]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  ; Basic types
  (equal-types (assoc {} 'a 5)
               (clojure.lang.IPersistentMap 'a '5))
  
  (equal-types (assoc {:b 6} 'a 5)
               (clojure.lang.IPersistentMap (U 'a ':b) (U '5 '6)))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil
                                                   (t/U nil (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)))
                 :a 5)
               (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)) :a 5)
               (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))
  
  (equal-types (assoc (clojure.core.typed/ann-form [] (clojure.lang.IPersistentVector clojure.core.typed/Any)) 0 2)
               (clojure.lang.IPersistentVector clojure.core.typed/Any))
  
  ;; TODO: assocs on records
  
  )

(deftest CTYP-62-equiv-test
  (is-clj (tc-equiv := 
                    [(ret (-val "a"))
                     (ret -any)]
                    nil)
          (ret (Un -false -true)))
  (is (= (eret (= "a" (clojure.core.typed/ann-form 1 clojure.core.typed/Any)))
         (ret (Un -false -true))))
  (is (= (eret (= "a" (clojure.core.typed/ann-form 1 clojure.core.typed/Any)))
         (ret (Un -false -true))))
  ; needs value objects
  ;(is-tc-e (clojure.lang.Util/equiv :a :a)
  ;         (t/Val true))
  (is-tc-err (do (import 'clojure.lang.Util)
                 (let [Util 1] (Util/equiv "a" "a")))
             (t/Val true))
  (is-tc-err (do (import 'clojure.lang.Util)
                 (let [Util 1] (. Util equiv "a" "a")))
             (t/Val true))
  )

(deftest invoke-merge-test
  
  ; basic literal case
  (equal-types (merge {:a 5 :b 6} {:c 7 :d 8} {:e 9})
               (HMap :mandatory {:a '5 :b '6 :c '7 :d '8 :e '9} :complete? true))
  
  ; right hand optionals
  (equal-types (merge {:a 5 :b 6}
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:c String} :complete? true)))
               (HMap :mandatory {:a '5 :b '6}
                     :optional {:c String}
                     :complete? true))
  
  ; left hand optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number :b String} :complete? true))
  
  ; nil first argument
  (equal-types (merge nil
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number :b String} :complete? true))
  
  ; nils in other arguments
  (equal-types (merge nil
                      nil
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? true))
                      nil
                      nil
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:b String} :complete? true))
                      nil)
               (HMap :optional {:a Number :b String} :complete? true))
  
  ; all nils
  (equal-types (merge nil)
               nil)
  (equal-types (merge nil nil nil)
               nil)
  
  ; (t/Option t/HMap) first argument
  (equal-types (merge (clojure.core.typed/ann-form {:c 5} (t/U nil (t/HMap :mandatory {:c '5} :complete? true)))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number :b String :c '5} :complete? true))
  
  ; (t/Option t/HMap) arguments
  (equal-types (merge (clojure.core.typed/ann-form {} (t/U nil (t/HMap :complete? true)))
                      (clojure.core.typed/ann-form {} (t/U nil (t/HMap :optional {:a Number} :complete? true)))
                      (clojure.core.typed/ann-form {} (t/U nil (t/HMap :optional {:b String} :complete? true))))
               (U nil (HMap :optional {:a Number :b String} :complete? true)))
  
  ; this merge doesn't actually give us any information about :b because
  ; the second map might not have a :b key, and the first map is partial.
  (equal-types (merge (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? false))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:b String} :complete? true)))
               (HMap :optional {:a Number}))
  
  ; but this does
  (equal-types (merge (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? false))
                      (clojure.core.typed/ann-form {:b "s"} (t/HMap :mandatory {:b String} :complete? true)))
               (HMap :mandatory {:b String} :optional {:a Number}))
  
  ; multiple optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? true))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:b String} :complete? true))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:c Long} :complete? true)))
               (HMap :optional {:a Number :b String :c Long} :complete? true))
  
  ;; incomplete right handsides
  
  ; non-covering right hand side
  (equal-types (merge {:a 5 :b 6}
                      (clojure.core.typed/ann-form {} '{}))
               '{:a clojure.core.typed/Any :b clojure.core.typed/Any})
  
  (equal-types (merge (clojure.core.typed/ann-form {:a 6} '{:a Number})
                      (clojure.core.typed/ann-form {:b "s"} '{:b String}))
               '{:a clojure.core.typed/Any :b String})
  
  ; incomplete covering mandatory
  (equal-types (merge {:a 5}
                      (clojure.core.typed/ann-form {:a 10} '{:a '10}))
               '{:a '10})
  
  ; incomplete covering optional
  (equal-types (merge {:a 5}
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:a (t/Value 10)})))
               '{:a (U (Value 5) (Value 10))})
  
  
  ; both incomplete optionals
  (equal-types (merge (clojure.core.typed/ann-form {} (t/HMap :optional {:a '5}))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:a '10})))
               (HMap :optional {:a (U '5 '10)}))
  
  ; (t/Option t/HMap) first argument incomplete
  (equal-types (merge (clojure.core.typed/ann-form {:a 5} (t/U nil '{:a '5}))
                      (clojure.core.typed/ann-form {:b 8} (t/HMap :mandatory {:b Number} :complete? true)))
               (U (HMap :mandatory {:b Number} :complete? true)
                  '{:a '5 :b Number}))
  
  ;; nil (t/HMap :absent-keys #{:a}) -> (t/HMap :absent-keys #{:a})
  ;; '{:a 5} '{} -> '{:a 5}
  ;; nil '{:a Number} -> '{:a Number}
  ;; '{:a 5} '{:a Number} -> '{:a Number}
  ; clojure.core.typed/All together: (t/U '{:a Number} (t/HMap :absent-keys #{:a})) or (t/HMap :optional {:a Number})
  (equal-types (merge (clojure.core.typed/ann-form {:a 5} (t/U nil '{:a '5}))
                      (clojure.core.typed/ann-form {} (t/HMap :optional {:a Number} :complete? false)))
               (HMap :optional {:a Number}))
  
  ; Basic maps
  (equal-types (merge (clojure.core.typed/ann-form {} (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)) {:a 5})
               (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any))
  
  (equal-types (merge {} {'a 5})
               (clojure.lang.IPersistentMap 'a '5))
  
  (equal-types (merge {:b 6} {'a 5})
               (clojure.lang.IPersistentMap (U 'a ':b) (U '5 '6)))

  (is-tc-e (fn [m1 :- '{:a t/Num}
                m2 :- '{:b t/Num}]
             :- '{:a t/Any :b t/Num}
             (merge m1 m2)))

  (is-tc-err (fn [m1 :- '{:a t/Num}
                  m2 :- '{:b t/Num}]
               :- '{:a t/Num :b t/Num}
               (merge m1 m2)))

;;  TODO not handling presence of non keyword keys yet
;;   (equal-types (merge {'a 5} {:b 6})
;;                (clojure.lang.IPersistentMap (t/U 'a ':b) (t/U '5 '6)))
  
  )

(deftest invoke-conj-test
  
  ; need to manually build hvec to match filters/objects
  (equal-types-noparse (conj nil nil)
                       (-hvec [-nil]
                              :filters [(-false-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (conj [1] 2 3)
                       (-hvec [(-val 1) (-val 2) (-val 3)]
                              :filters [(-true-filter)
                                        (-true-filter)
                                        (-true-filter)]
                              :objects [-empty -empty -empty]))
  (equal-types-noparse (conj [1]
                             (ann-form nil (t/U nil '2))
                             3)
                       (-hvec [(-val 1) (Un -nil (-val 2)) (-val 3)]
                              :filters [(-true-filter)
                                        (-FS -top -top)
                                        (-true-filter)]
                              :objects [-empty -empty -empty]))
  
  (equal-types (conj (ann-form nil (t/U nil '['1]))
                     (ann-form nil (t/U nil '2)))
               (U '[(U nil '2)]
                  '['1 (U nil '2)]))
  
  (equal-types (conj {:a 1} [:b 2])
               (HMap :mandatory {:a '1 :b '2} :complete? true))
  
  (equal-types (conj {:a 1}
                     (clojure.core.typed/ann-form nil (t/U nil '[':b '2])))
               (U (HMap :mandatory {:a '1} :complete? true)
                  (HMap :mandatory {:a '1 :b '2} :complete? true)))
  
  (equal-types (conj (clojure.core.typed/ann-form nil (t/U nil (HMap :mandatory {:a '1} :complete? true)))
                     (clojure.core.typed/ann-form nil (t/U nil '[':b '2])))
               (U '[(U nil '[':b '2])]
                  (HMap :mandatory {:a '1} :complete? true)
                  (HMap :mandatory {:a '1 :b '2} :complete? true)))
  
  (equal-types (conj #{5} 6 7)
               (clojure.lang.IPersistentSet (U '5 '6 '7))))

(deftest unannotated-record-test
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.unannotated-record))))

(deftest datatype-method-recur-test
  (is (check-ns 'clojure.core.typed.test.datatype-recur)))

(deftest record-annotated-as-datatype-test
  ; record annotated as datatype
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.record-as-datatype)))
  ; datatype annotated as record
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.datatype-as-record))))

(deftest recursive-ann-test
  (is (check-ns 'clojure.core.typed.test.recursive)))

(deftest comparable-inline-test
  (is-tc-e (fn [v x] (compare v x)) (t/IFn [Comparable t/Any -> t/Num])))

(deftest CTYP-71-simplify-test
                                              ; must be resolvable to trigger the bug
  (is-tc-e (clojure.core.typed/fn [a :- (t/U nil (clojure.core.typed/Nilable java.util.Date))] 
                                 (when a (clojure.core.typed/ann-form a java.util.Date)))))

;(clj (compact [(-filter (parse-type 'Number) 0)
;               (-not-filter (Un -false -nil) 0)]
;              false))

;TODO uncomment
; See CTYP-150
#_(deftest CTYP-84-hlist-ancestor-test
  (is-tc-e (seq '(1)) 
           (t/NonEmptySeq t/Num)))

(deftest CTYP-78-finally-expected-test
  (is (check-ns 'clojure.core.typed.test.finally)))

(deftest CTYP-77-invoke-nonliteral-kw-test
  (is (check-ns 'clojure.core.typed.test.non-literal-val-fn)))

(deftest CTYP-74-malformed-TApp-test
  (is-clj (err/tc-error-thrown? (parse-type `([t/Any ~'-> t/Any])))))

(deftest CTYP-73-reduced-test
  (is-tc-e (reduced 1) (clojure.lang.Reduced t/Num))
  (is-tc-e @(reduced 1) t/Num)
  (is-tc-e (reduce (ann-form
                     (fn [a b] (if (= a b) 1 (reduced 1)))
                     [Number Number -> (t/U (clojure.lang.Reduced Number) Number)])
                 1 [1 2 3])
           t/Num))

(deftest Assoc-test
  (is-tc-e {:a 1} (t/Assoc '{} ':a Number))
  (is-tc-e {:a 1} (t/Assoc (t/U '{:a Number} '{:a Double}) ':a Long))
  (is-tc-e (fn [a] (assoc a 1 2))
           (t/All [[x :> (t/Map t/Nothing t/Nothing) 
                  :< (t/Map t/Num t/Num)]]
              [x -> (t/Map t/Num t/Num)]))
  (is-tc-e (fn [a] (assoc a :a 1)) 
           (t/All [[x :> (t/Map t/Nothing t/Nothing) :< (t/Map t/Any t/Any)]] 
              [x -> (t/Assoc x ':a t/Num)]))
  (is-tc-e (let [f :- (t/All [[x :< (t/Map t/Any t/Any)]] 
                           [x -> (t/Assoc x ':a Number)])
                     (fn [a] (assoc a :a 1))]
             (f {:b 1}))
           '{:b t/Num :a t/Num})
  (is-tc-e (fn [a] (assoc a :a 1)) 
           (t/All [[x :< (t/Map t/Any t/Any)]] 
             [x -> (t/Assoc x ':a Number)]))
  (is-tc-e (let [add-a :- (t/All [[x :< (t/Map t/Any t/Any)]]
                               [x -> (t/Assoc x ':a Number)])
                 #(assoc % :a 1)
                 _ :- '{:a t/Num}, (add-a {})
                 _ :- t/Num, (-> (add-a {}) :a)]))
  (is-tc-e {:a 1 :b 2}
           (t/U (t/Assoc '{} ':a t/Num)
              (t/Assoc '{} ':b t/Num)
              (t/Assoc '{} ':a t/Num ':b t/Num))))

;(clj
;  (parse-filter 
;    '(&
;      (when
;        (! (t/U nil false) b69880)
;        (& (is clojure.core.typed/NonEmptyCount b) (! nil b)))
;      (! (t/U nil false) b69880)
;      (is clojure.core.typed/NonEmptyCount a)
;      (when
;        (is (t/U nil false) a69879)
;        (is (t/U clojure.core.typed/EmptyCount nil) a))
;      (! nil a)
;      (! (t/U nil false) a69879)
;      (when
;        (is (t/U nil false) b69880)
;        (is (t/U clojure.core.typed/EmptyCount nil) b)))))
;
;(clj
;  (parse-filter 
;    '(&
;      (when (! (t/U nil false) b69880)
;        (& (is clojure.core.typed/NonEmptyCount b) (! nil b)))
;      (! (t/U nil false) b69880))))
;
;(clj
;  (compact
;    [(parse-filter '(when (! (t/U nil false) b69880)
;                      (& (is clojure.core.typed/NonEmptyCount b) (! nil b))))
;     (parse-filter '(! (t/U nil false) b69880))]
;    false))

(deftest hvec-ops
  (is-tc-e (first [1 'a]) 
           Number)
  (is-tc-e {:a 1} 
           (t/Map t/Keyword Number))
  (is-tc-e {:a 1} 
           (t/I (t/Map t/Keyword Number)
                        (t/ExactCount 1)))
  (is-tc-e {:a 1} 
           (t/NonEmptySeqable '[t/Keyword Number]))
  (is-tc-e ((inst first '[t/Keyword Number]) {:a 1})
           '[t/Keyword Number])
  (is-tc-e (let [first (ann-form first
                                (clojure.core.typed/All [x]
                                  [(t/NonEmptySeqable x) -> x]))]
             (first {:a 1}))
           '[t/Keyword Number])
  (is-tc-e (first (ann-form {:a 1} (t/NonEmptySeqable '[t/Keyword Number])))
           '[t/Keyword Number])
  (is-tc-e (first (ann-form {:a 1} (t/I (t/ExactCount 1) (t/Map t/Keyword Number))))
           '[t/Keyword Number])
  (is-tc-e (first (ann-form {:a 1} (t/I (t/ExactCount 1) (clojure.lang.APersistentMap t/Keyword Number))))
           '[t/Keyword Number])
  (is-tc-e (first {:a 1})
           '[t/Keyword Number])
  (is-cf (-> {:a 1} first second) Number)
  )

(deftest variable-hvec-test
  (is (sub?-q `'[t/Num t/Num t/Num] `'[t/Num t/Num ~'*]))
  (is (sub?-q `'[t/Num t/Num t/Num] `'[t/Num t/Num ~'*]))
  (is (sub? '[Number Number Integer *] '[Number Number *]))
  (is (not (sub? '[Number Number clojure.lang.Symbol *] '[Number Number *])))
  (is-tc-e [1 2 3] '[t/Num t/Num *])
  (is-tc-e [1 2 3] '[t/Num t/Num t/Num t/Num *])
  (is-tc-e (first [1 'a 2]) t/Num)
  (is-tc-e (second [1 2 3]) t/Num))

(deftest CTYP-85-abo-test
  (is-tc-e (fn [] (fn [b] b))
           [-> [t/Any -> t/Any]]))

(deftest expected-IPersistentMap-test
  (is-tc-e {:a #(+ %1 %2)}
           (t/Map t/Any [t/Num t/Num -> t/Num])))

;(reset-caches)

;(chk/abstract-result
;  (ret (-hvec [-any] :filters [(-FS (-filter (parse-clj 'Number) 'a) -top)] :objects [(-path nil 'a)])
;       (-FS (-filter (parse-clj 'Number) 'a) -top))
;  ['a])


;TODO support (some #{...} coll)
;TODO (apply == (non-empty-seq))
;TODO tests for inferring upper/lower bounds

(deftest def-test
  (is (check-ns 'clojure.core.typed.test.def-arrow))
  (is-tc-e (clojure.core.typed/def a :- t/Num 1)
           (t/Var1 t/Num)))

(deftest poly-inst-scoping-test
  (is-tc-e (fn [a] (inst identity foo))
           (t/All [foo] [t/Any -> t/Any]))
  (is-tc-e
    (fn [f coll]
      (fn
        [x :- a
         y :- (t/Seqable b)]))
    (t/All [a b] [t/Any t/Any -> t/Any]))
  (is-tc-err
    (fn :forall [x y]
      [f coll]
      (fn
        [x :- a
         y :- (t/Seqable b)]))
    (t/All [a b] [t/Any t/Any -> t/Any]))
  (is-tc-e
    (fn :forall [x y]
      [f coll]
      (fn
        [x :- x
         y :- (t/Seqable y)]))
    (t/All [a b] [t/Any t/Any -> t/Any]))
  ;zero args is fine for dotted vars
  (is-tc-e (fn [a] (inst a))
           [(t/All [b ...] [b ... b -> t/Any]) -> t/Any]))

(deftest infer-bounds-test
  (is (= (infer-bounds -any nil)
         (infer-bounds -any -nothing)
         (infer-bounds nil nil)
         (-bounds -any -nothing)))
  (is-clj (let [t (parse-type `(t/Seq t/Num))]
            (= (infer-bounds t nil)
               (-bounds t -nothing)))))

(deftest consistent-variance-test
  (is-clj (let [t (parse-type `(t/TFn [[x# :variance :covariant]] x#))]
            (TypeFn-body* (TypeFn-fresh-symbols* t) t)
            true))
  (is-clj (let [t (parse-type `(t/TFn [[x# :variance :contravariant]] t/Any))]
            (TypeFn-body* (TypeFn-fresh-symbols* t) t)
            true)))

(deftest hvec-abstract-test
  (is-tc-e (fn [a b] [(class a) (class b)])
           [t/Any t/Any
            -> (t/HVec [(t/U nil Class) (t/U nil Class)]
                     :objects [{:path [Class], :id 0} {:path [Class], :id 1}])]))

(deftest interop-test
  (is (check-ns 'clojure.core.typed.test.interop)))

;(sub? (clojure.core.typed/All [x] (t/TFn [[a :variance :covariant]] clojure.core.typed/Any))
;      (t/Rec [m] (t/TFn [[a :variance :covariant]] m)))

(deftest nested-tfn-test
  (is (check-ns 'clojure.core.typed.test.nested-tfn-operator)))

(deftest parse-forbidden-rec-test
  (is-clj (throws-tc-error?
            (parse-type `(t/Rec [x#] x#))))
  (is-clj (throws-tc-error?
            (parse-type `(t/Rec [x#] (t/I x# Number)))))
  (is-clj (throws-tc-error?
            (parse-type `(t/Rec [x#] (t/U x# Number)))))
  (is-clj (throws-tc-error?
            (parse-type `(t/Rec [x#] (t/U (t/I x# Number) Double))))))

(deftest parse-value-test
  (is-clj (throws-tc-error?
            (parse-type `(t/Value))))
  (is-clj (throws-tc-error?
            (parse-type `(t/Value 1 2 3))))
  (is-clj (throws-tc-error?
            (parse-type `a)))
  (is-clj (throws-tc-error?
            (parse-type ':a)))
  (is-clj (throws-tc-error?
            (parse-type '1))))

(deftest parse-TFn-bad-args-test
  (is-clj (throws-tc-error?
            (parse-type `(t/TFn [[x# :variance :covariant :argh]] t/Any)))))

(deftest parse-HMap-bad-args-test
  (is-clj (throws-tc-error?
            (parse-type `(t/HMap :foo))))
  (is-clj (throws-tc-error?
            (parse-type `(t/HMap :foo :foo))))
  (is-clj (throws-tc-error?
            (parse-type `(t/HMap :mandatory {} :mandatory {}))))
  (is-clj (throws-tc-error?
            (parse-type `(t/HMap ~'mandatory {})))))

(deftest hmap-intersection-test
  (is-tc-e {:a 1} 
           (t/I '{} '{:a t/Num}))
  (is-tc-e {:a 1 :b 2} 
           (t/I '{:b t/Num} '{:a t/Num}))
  (is-tc-e {:foo 3 :bar "hi"} 
           (t/I '{:foo t/Int} 
              '{:bar t/Str}))
  (is-tc-e {:a 1 :b 2} 
           (t/I '{:b t/Num} 
              '{:a t/Num}))
  (is-tc-e-with-aliases 
    {:a 1 :b 2} (t/I clojure.core.typed.test.util-aliases/HMapAlias1 
                   clojure.core.typed.test.util-aliases/HMapAlias2))
  (is-tc-e-with-aliases
    {:foo 3 :bar "hi"}
    (t/I clojure.core.typed.test.util-aliases/HMapAliasInt1 
       clojure.core.typed.test.util-aliases/HMapAliasStr2)))

(deftest rclass-invariant-test
  (is-clj
    (subtype? 
      (RClass-of 'clojure.lang.ChunkBuffer
                 [(RClass-of 'java.lang.Number)])
      (RClass-of 'clojure.lang.ChunkBuffer
                 [(Name-maker 'java.lang.Number)]))))

(deftest protocol-method-ann-test
  (is-clj (let [x1 (gensym 'x1)
                x2 (gensym 'x2)
                names [x1 x2]
                bnds [no-bounds no-bounds]
                mt (with-bounded-frees (zipmap (map make-F names)
                                               bnds)
                     (parse-type `(t/All [m1#]
                                    [t/Any ~x1 m1# ~'-> ~x2])))]
            (both-subtype? (collect-u/protocol-method-var-ann
                             mt names bnds)
                           (parse-type 
                             `(t/All [x1# x2# m1#]
                                [t/Any x1# m1# ~'-> x2#]))))))

(deftest map-predicate-test
  (is-tc-e (fn [a] (number? (:k a)))
           (t/Pred (t/HMap :mandatory {:k Number})))
  ; integer check is not sufficient
  (is-tc-err (fn [a] (integer? (:k a)))
             (t/Pred (t/HMap :mandatory {:k Number})))
  ; wrong key
  (is-tc-err (fn [a] (number? (:wrong-key a)))
             (t/Pred (t/HMap :mandatory {:k Number})))
  (is 
    (sub?-q
      `(t/IFn [t/Any :-> Boolean 
             :filters {:then (~'is Number 0 [(~'Key :k)]), 
                       :else (~'! Number 0 [(~'Key :k)])}])
      `(t/Pred (t/HMap :mandatory {:k Number}))))

  (is
    (not
      (sub?-q
        `(t/IFn [t/Any :-> Boolean 
               :filters {:then (~'is Long 0 [(~'Key :k)]), 
                         :else (~'! Long 0 [(~'Key :k)])}])
        `(t/Pred (t/HMap :mandatory {:k Number})))))

  (is-clj 
    (sub/subtype-type-filter?
      (parse-filter `(~'is Number 0 [(~'Key :k)]))
      (parse-filter `(~'is (t/HMap :mandatory {:k Number}) 0))))

  (is-clj 
    (sub/subtype-not-type-filter?
      (parse-filter `(~'! Number 0 [(~'Key :k)]))
      (parse-filter `(~'! (t/HMap :mandatory {:k Number}) 0)))))

(deftest function-as-ifn-test
  (is (sub? [-> nil] clojure.lang.IFn))
  (is (sub? [-> nil] Callable))
  (is (sub? [-> nil] Runnable)))

#_
(deftest swap!-special-test
  (is (check-ns 'clojure.core.typed.test.swap-bang)))

(deftest seqable-map-test
  (is-tc-e (map (fn [[a b] :- '[Number Number]]
                  (+ a b))
                {1 2 3 4 5 6})))

(deftest mapentry-first-test
  (is-tc-e (first {1 2})
           '[t/Num t/Num])
  (is-tc-e (first (first {1 2}))
           t/Num))

(deftest CTYP-101-mapentry-test
  (is (check-ns 'clojure.core.typed.test.CTYP-101-mapentry)))

(deftest demunged-protocol-method-test
  (is (check-ns 'clojure.core.typed.test.protocol-munge)))

(deftest csgen-hmap-test
  ; (t/HMap :mandatory {:a Number :b Number} :complete? true) :!< (t/HMap :mandatory {:a x} :complete? true)
  (is-tc-err
    (letfn>
      [take-map :- (t/All [x] [(t/HMap :mandatory {:a x} :complete? true) -> x])
       (take-map [a] (:a a))]
      (take-map {:a 1 :b 2})))
  ; (t/HMap :mandatory {:a Number}) :!< (t/HMap :mandatory {:a x} :complete? true)
  (is-tc-err
    (letfn>
      [take-map :- (t/All [x] [(t/HMap :mandatory {:a x} :complete? true) -> x])
       (take-map [a] (:a a))]
      (take-map (ann-form 
                  {:a 1}
                  '{:a Number})))))

(deftest subtype-hmap-optional-test
  (is (sub?-q
        `(t/HMap :mandatory {:a Number})
        `(t/U (t/HMap :mandatory {:a Number})
            (t/HMap :absent-keys [:a]))))
  (is (sub?-q
        `(t/HMap :mandatory {:a Number})
        `(t/HMap :optional {:a Number})))
  (is (not
        (sub?-q
          `(t/HMap :complete? true :mandatory {:a Number :b t/Any})
          `(t/HMap :complete? true :mandatory {:a Number}))))
  (is (sub?-q
        `(t/HMap :complete? true :optional {:a Number :b t/Any})
        `(t/HMap :complete? true :optional {:a Number})))
  (is (not
        (sub?-q
          `(t/HMap :optional {:a Number})
          `(t/HMap :mandatory {:a Number}))))
  (is (not
        (sub?-q
          `(t/HMap :optional {:b Number})
          `(t/HMap :optional {:a Number}))))
  (is (not
        (sub?-q
          `(t/HMap :optional {:a t/Any})
          `(t/HMap :optional {:a Number}))))
  (is (not
        (sub?-q
          `(t/HMap :mandatory {:a Number})
          `(t/ExactCount 1))))
  (is (sub?-q
        `(t/HMap :complete? true :mandatory {:a Number})
        `(t/ExactCount 1)))
  (is (not
        (sub?-q
          `(t/HMap :complete? true 
                 :mandatory {:foo t/Any}
                 :optional {:a Number})
          `(t/ExactCount 1))))
  (is (sub?-q
        `(t/HMap :complete? true
               :mandatory {:foo Number})
        `(t/Map t/Any t/Num)))
  (is (not
        (sub?-q
          `(t/HMap :complete? true
                 :mandatory {:foo Number}
                 :optional {:bar t/Any})
          `(t/Map t/Any t/Num))))

  (is (sub?-q 
        `(t/U (t/HMap :mandatory {:foo Number}
                 :complete? true)
           (t/HMap :complete? true))
        `(t/HMap :optional {:foo Number})))
  (is (sub?-q
        `(t/U (t/HMap :mandatory {:c Number}
                  :optional {:b Number :a Number}
                  :complete? true)
            (t/HMap :optional {:b Number :c Number}
                  :complete? true))
        `(t/HMap :optional {:a Number :b Number :c Number})))
  (is (sub?-q
        `(t/U (t/HMap :mandatory {:c (t/Value 5)} 
                  :complete? true) 
            (t/HMap :complete? true))
        `(t/HMap :optional {:c (t/Value 5)} :complete? true)))
  (is (sub?-q
        `(t/U (t/HMap :mandatory {:c (t/Value 5)} 
                 :optional {:b t/Str :a t/Num} 
                 :complete? true) 
           (t/HMap :mandatory {} 
                 :optional {:b t/Str :a t/Num} 
                 :complete? true))
        `(t/HMap :optional {:a t/Num :b t/Str :c (t/Value 5)} 
               :complete? true)))
  (is (sub?-q
        `(t/HMap :optional {:b t/Str :a t/Num})
        `(t/HMap :optional {:a t/Num})))
  (is (not
        (sub?-q
          `(t/HMap :optional {:a Number})
          `(t/HMap :optional {:b String :a Number}))))
  )

(deftest hmap-expecteds-infer-test
  (is-tc-e {:a (fn [a] (+ a 1))}
           (t/HMap :mandatory {:a [Number -> Number]}))
  (is-tc-e {:a (fn [a] (+ a 1))}
           (t/HMap :optional {:a [Number -> Number]}))
  (is-tc-e {:a (fn [a] (+ a 1))}
           (t/Map t/Any [Number -> Number]))
  )

(deftest hmap-optional-get-test
  (is-tc-err (let [m :- (t/HMap :optional {:a Number})
                   {:a 1}]
               (get m :a))
             t/Num
             ;:ret (ret (Un (-name `t/Num) -nil))
             )
  (is-tc-e (get (ann-form
                  {:a 1}
                  (t/HMap :optional {:a Number}))
                :a)
           (t/U nil Number)))

(deftest datatype-variance-test
  (is (check-ns 'clojure.core.typed.test.variance-test)))

(deftest rec-type-test
  (is-clj (sub?-q
            `(t/HMap :mandatory {:a [t/Any :-> t/Any]} :complete? true)
            `(t/Rec [x#] (t/Map t/Any (t/U [t/Any :-> t/Any] x#)))))
  (is-clj (sub?-q
            `(t/HVec [[t/Any :-> t/Any]])
            `(t/Rec [x#] (t/Vec (t/U [t/Any :-> t/Any] x#)))))
  (is-clj (sub?-q
            `(t/Vec [t/Any :-> t/Any])
            `(t/Rec [x#] (t/Vec (t/U [t/Any :-> t/Any] x#)))))
  (is-clj (sub?-q
            `(t/Vec ':a)
            `(t/Rec [x#] (t/Vec (t/U ':a x#)))))
  (is-clj (not
            (sub?-q
              nil
              `(t/Rec [x#] (t/Vec (t/U ':a x#))))))
  (is
    (clj (not
           (subtype?
             (parse-type `(t/Rec [x#] (t/U nil '[t/Any x#])))
             (parse-type `(t/Rec [x#] '[Number x#]))))))
  ;; infinite loop
  (is
    (clj (overlap
           (parse-type `(t/Rec [x#] (t/U nil '[t/Any x#])))
           (parse-type `(t/Rec [x#] '[Number x#])))))
  (is (check-ns 'clojure.core.typed.test.rec-type)))

(deftest poly-rec-type-test
  ; without Rec type
  (is-tc-e
    (letfn> 
      [pfoo :- (t/All [x] [(t/Map t/Any x) -> (t/Seq x)])
       (pfoo [a] (vals a))]
      (pfoo
        (ann-form
          {}
          (t/Map t/Any Number)))))
  ; with Rec type, but non-polymorphic function
  (is-tc-e
    (letfn> 
      [pfoo :- [(t/Map t/Any t/Any) -> (t/Seq t/Any)]
       (pfoo [a] (vals a))]
      (pfoo
        (ann-form
          {}
          (t/Rec [x] (t/Map t/Any (t/U Number x)))))))
  ; with Rec type, polymorphic function
  (is-tc-e
    (letfn> 
      [pfoo :- (t/All [x] [(t/Map t/Any x) -> (t/Seq x)])
       (pfoo [a] (vals a))]
      (pfoo
        (ann-form
          {}
          (t/Rec [x] (t/Map t/Any (t/U Number x)))))))
  )

(deftest profile-fail-test
  ; ensure check-ns still runs even if timbre isn't loaded
  (is (check-ns-info 'clojure.core.typed.test.trampoline
                     :profile true)))

; CTYP-105
(deftest hmap-absent-and-optional-subtype-test
  (is (sub?-q `(t/HMap :absent-keys #{:a})
              `(t/HMap :optional {:a t/Any})))
  (is (check-ns 'clojure.core.typed.test.ctyp105)))

(deftest trampoline-test
  (is (check-ns 'clojure.core.typed.test.trampoline)))

(deftest polydots-unparse-test
  (is-clj (= '[a b ...]
             (second
               (unparse-type
                 (parse-type
                   '(clojure.core.typed/All [a b ...])))))))

(deftest Get-test
  ;resolve
  (is-clj (= (fully-resolve-type (parse-clj `(t/Get '{:a Number} ':a)))
             (fully-resolve-type (parse-clj `Number))))
  (is-clj (both-subtype? (parse-clj `Number)
                         (parse-clj `(t/Get '{:a Number} ':a))))
  (is-tc-e 1 (t/Get '{:a Number} ':a))
;  (is-tc-e (fn [a] (inc a)) 
;           [(t/Get '{:a [Number -> Number]} ':a) -> Number])
  (is-tc-e (fn [a] (deref a))
           [(t/Get '{:a (t/Atom1 Number)} ':a)
            -> Number])
  )

(deftest apply-hmap-test
  ;; special case for direct apply
  (is-tc-e (apply hash-map [:a 1 :b 2])
           (t/HMap :mandatory {:a Number
                             :b Number}
                 :complete? true))
  (is-tc-err #(apply hash-map [:a 1 :b]))
  (is-tc-err #(apply hash-map [:a 1 :b 'a])
             [-> (t/HMap :mandatory {:a Number
                                   :b Number}
                       :complete? true)])
  ;; indirect higher-order usages are less precise
  (is-tc-e (apply (inst hash-map t/Keyword Number) [:a 1 :b 2])
           :expected (t/Map t/Keyword Number)))

(deftest heterogeneous-parse-ast-test
  (is (prs-ast/parse-clj `(t/HList [Number])))
  (is (prs-ast/parse-clj `(t/HSeq [Number])))
  (is (prs-ast/parse-clj `(t/HVec [Number]))))

;(deftest collect-on-eval-test
;  (is (do (ann foo-bar Number)
;          (cf (def foo-bar 1))
;          (cf foo-bar)
;          true)))

;(deftest parse-with-inferred-variance
;  (is-clj (= (clj (parse-type '(t/TFn [[x :variance :inferred]] x)))
;             (parse-type '(t/TFn [[x :variance :covariant]] x)))))

;(sub? (t/TFn (t/Rec [m]
;                     (t/TFn [[x :variance :covariant]]
;                       (t/Rec [c]
;                         (IColl max-arg m c))))))

; test cast CTYP-118
(deftest cast-test
  (is (check-ns 'clojure.core.typed.test.CTYP-118-cast))
  (is-tc-err (fn [] (core/cast "a" "a")))
  (is-tc-err (fn [] (core/cast String "a" 1)))
  (is-tc-err (fn [] (core/cast #('ok) 2))))

(deftest optional-record-keys-test
  (is (check-ns 'clojure.core.typed.test.record-optional-key))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.record-no-nil)))
  (is (check-ns 'clojure.core.typed.test.record-poly-map-ctor)))

(deftest recur-rest-args-test
  (is (check-ns 'clojure.core.typed.test.recur-rest-arg))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.recur-non-seq-rest)))
  (is (err/top-level-error-thrown?
        (check-ns 'clojure.core.typed.test.fail.recur-empty-seq))))

(deftest poly-record-test
  (is (check-ns 'clojure.core.typed.test.poly-record)))

(deftest polymorphic-hmap-test
  (is-tc-e (t/letfn>
             [f :- (t/All [m]
                          [(t/HMap 
                             :mandatory {:m m}
                             :optional {:o Number}) 
                           -> t/Any])
              (f [a])]
             (f {:m 1 :o 2}))))

(deftest ctyp97-tvar-scoping-test
  (is (check-ns 'clojure.core.typed.test.ctyp97-tvar-scoping)))

;TODO
;(deftest ctyp124)

(deftest get-bounded-tvar-test
  (is (check-ns 'clojure.core.typed.test.get-bounded-tvar)))

(deftest promise-test
  (is (check-ns 'clojure.core.typed.test.promise)))

(deftest pred-scoping-test
  (is (check-ns 'clojure.core.typed.test.pred-scoping)))

(deftest hvec-count-test
  (is (not
        (sub?-q `(t/I (t/CountRange 1) (t/HVec [clojure.core.typed/Any ~'*]))
                `(t/CountRange 0 0)))))

(deftest annotate-user-defined-polydot
  (is-tc-e (fn [x & y] x) 
           (t/All [x y ...] [x y ... y -> x]))
  (is-tc-e (fn [f a] (f a))
           [(t/All [x] [(t/HSequential [x *]) -> x])
            (t/HSequential [t/Any *]) -> t/Any])
  (is-tc-e (fn [a] (first a)) 
           [(t/I (t/CountRange 1) (t/HVec [t/Any *])) -> t/Any])
  (is-tc-e (fn [a] (first a)) 
           [(t/I (t/CountRange 1) (t/HSequential [t/Any *])) -> t/Any])
  (is-tc-e (fn [& y] (when-not (empty? y) (first y))) 
           (t/All [x y] [y * -> (t/U nil y)]))
  (is-tc-e (fn [x & y] x) 
           (t/All [a b ...] [a b ... b -> a]))

  (is (check-ns 'clojure.core.typed.test.hsequential))

  (is-tc-err (fn [x c & y] x) 
             (t/All [x y ...] [x y ... y -> x])))

(deftest kw-args-seq-complete-test
  (is-tc-err
    (apply concat {:a 1 :b 2})
    (t/Seq t/Keyword))
  (is-tc-e (apply concat {:a 1 :b 2})
           (t/Seq (t/U t/Keyword Number)))
  (is (subtype? (-kw-args-seq :mandatory {(-val :a) (-val 1)}
                              :complete? true)
                (parse-clj `(t/Seq (t/U t/Keyword Number))))))

(deftest add-HSequential
  (is-tc-e [1 2 3] (t/HSequential [Number *]))
  (is-tc-e '(1 2 3) (t/HSequential [Number *]))
  (is-tc-e '(1 2 3) (t/HSequential [(t/Value 1) (t/Value 2) (t/Value 3)])))

(deftest nilable-non-empty-rest-args-test
  (is-tc-e (fn [& args]
             (ann-form args (t/U nil (t/NonEmptySeq t/Any))))))

(deftest fail-on-reflection-test
  (is (caught-top-level-errors (constantly true)
        (check-ns 'clojure.core.typed.test.fail.reflection))))

(deftest tc-ignore-test
  (is-tc-e (fn [] (tc-ignore (+ 'a 1))))
  ;; evaluates body
  (is (thrown? Exception
               (tc-e (tc-ignore (+ 'a 1)))))
  (is (= 1
         (:result
           (check-form-info
             '(do (do 1))))))
  ;; FIXME
  #_
  (is (= 1
         (:result
           (check-form-info
             '(do (do (tc-ignore 1))))))))

(deftest loop-macro-test
  (is-tc-e (fn [] (loop [a 1] (recur a))))
  (is-tc-e (fn [] (loop [a :- Number 1] (recur a))))
  (is-tc-err
    (fn []
      (loop [a :- t/Symbol 1] (recur a)))))

(deftest nth-test
  (is-tc-err #(nth #{1 2} 0))
  (is-tc-err #(nth (ann-form #{1 2} (t/Seqable t/Any)) 0))
  (is-tc-err #(nth (ann-form [1 2] (t/Seqable t/Any)) 0))
  (is-tc-err #(nth {1 0} 0))
  (is-tc-e (nth [1] 0) t/Int)
  (is-tc-e (nth '(1) 0) t/Int)
  (is-tc-e (nth (ann-form "a" CharSequence) 0) Character)
  (is-tc-e (nth "a" 0) Character)
  (is-tc-e (let [nth (ann-form nth 
                               (t/All [x y] 
                                 [(t/U (clojure.lang.Indexed x) 
                                     (t/I clojure.lang.Sequential (t/Seqable x))) 
                                  t/Int -> t/Any]))]
             (nth "a" 0)))
  (is-clj (do
            (dotimes [_ 100]
              (cs-gen #{'x} {'x no-bounds} {}
                      (-val "a")
                      (Un (RClass-of clojure.lang.Indexed [(make-F 'x)])
                          (In (RClass-of clojure.lang.Sequential) 
                              (RClass-of clojure.lang.Seqable [(make-F 'x)])))))
            true))
  (is-clj (some
            #{(RClass-of clojure.lang.Indexed [-any])}
             (mapv fully-resolve-type (RClass-supers* (RClass-of 'java.util.ArrayList)))))
  (is (sub?-q `java.util.ArrayList 
              `(clojure.lang.Indexed t/Any)))
  (is-tc-e (nth (ann-form (java.util.ArrayList. [1 2 3]) (java.util.RandomAccess t/Any)) 0))
  (is-tc-e (nth (java.util.ArrayList. [1 2 3]) 0))
  ; this used to fail randomly
  (dotimes [_ 20]
    (is-tc-e (let [nth (ann-form nth (t/All [x y] 
                                            [(t/U (clojure.lang.Indexed x) 
                                                  (t/I clojure.lang.Sequential (t/Seqable x))) 
                                             t/Int -> t/Any]))]
               (nth "a" 0))))
  (is (apply = (for [n (range 20)]
                 (clj
                   (cs-gen #{}
                           {'x no-bounds
                            'y no-bounds}
                           {}
                           (parse-clj `(t/Value "a") )
                           (with-bounded-frees {(make-F 'x) no-bounds}
                             (parse-clj `(t/U (t/I clojure.lang.Sequential (clojure.lang.Seqable ~'x)) (clojure.lang.Indexed ~'x))))))))))

(deftest nested-poly-test
  (is (Poly* ['a] [no-bounds]
             (Poly* ['x] [no-bounds] -any)))
  (is (Poly* ['a] [no-bounds]
             (PolyDots* ['x] [no-bounds] -any)))
  (is (PolyDots* ['a] [no-bounds]
             (Poly* ['x] [no-bounds] -any)))
  (is (parse-clj '(clojure.core.typed/All [b] [clojure.core.typed/Any -> (clojure.core.typed/All [b ...] [clojure.core.typed/Any -> clojure.core.typed/Any])])))
  (is (parse-clj '(clojure.core.typed/All [b] [b -> (clojure.core.typed/All [b ...] [b ... b -> clojure.core.typed/Any])])))
  (is (parse-clj '(clojure.core.typed/All [b ...] [b ... b -> (clojure.core.typed/All [b ...] [b ... b -> clojure.core.typed/Any])])))
  (is (parse-clj '(clojure.core.typed/All [b ...] [b ... b -> (clojure.core.typed/All [b ...] '[])]))))

(deftest instantiate-polydots-test
  (is (let [sym (gensym)]
        (= sym
           (-> (PolyDots-body* [sym] (parse-clj '(clojure.core.typed/All [b ...] ['[b ... b] -> clojure.core.typed/Any])))
               :types
               first
               :dom
               first
               :drest
               :name))))
  (is (= (-> (parse-clj '(clojure.core.typed/All [b ...] ['[b ... b] -> clojure.core.typed/Any]))
             :scope
             :body
             :types
             first
             :dom
             first
             :drest
             :name)
         0)))

(deftest dotted-fn-test
  (is-tc-e (fn [f :- (t/All [b ...] [-> [b ... b -> t/Any]])] 
             (f)))
  (is-tc-e (fn [f :- (t/All [b ...]
                          ['[b ... b] ... b -> [b ... b -> t/Any]])] 
             (f [1 2] [1 2])))
  (is-tc-e (fn [f :- (t/All [b ...]
                          [-> (t/HVec [b ... b])])] 
             (f)))
  (is-tc-e (fn [f :- (t/All [b ...]
                          [-> (t/HSequential [b ... b])])]
             (f))))

;FIXME
#_(deftest first-class-poly-test
  (is-tc-err
    (fn [f] (second (f [1 2])))
    [(clojure.core.typed/All [b]
          [b -> b])
     -> '1])
  ; ensure b ... b does not leak into the return type
  (is (cf (fn [f] (f))
          [(clojure.core.typed/All [b ...]
                [-> [b ... b -> clojure.core.typed/Any]])
           -> clojure.core.typed/Any]))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [a b ...] [-> '[a *]]))
          (def foo)
          (fn []
            (foo)))))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b] [-> '[b *]]))
          (def foo)
          (fn []
            (foo)))))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b] [-> [b * -> clojure.core.typed/Any]]))
          (def foo)
          (fn []
            (foo)))
      [-> [clojure.core.typed/Any * -> clojure.core.typed/Any]]))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> [b ... b -> clojure.core.typed/Any]]))
          (def foo)
          (fn []
            (foo)))
      [-> [clojure.core.typed/Any * -> clojure.core.typed/Any]]))
  (is
    (= 
      (tc-e 
        (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> '[b ... b]]))
            (def foo)
            (fn []
              (foo))))
      (ret (parse-type '[-> '[clojure.core.typed/Any *]])
           (-true-filter)
           -empty)))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> (t/HSequential [b ... b])]))
          (def foo)
          (fn []
            (foo)))))
  (is
    (tc-e 
      (do (ann ^:no-check foo (clojure.core.typed/All [b ...] [-> '[b ... b]]))
          (def foo)
          (fn []
            (foo)))))
  (is (cf (fn [f] (f))
          [(clojure.core.typed/All [b ...]
                [-> '[b ... b]])
           -> clojure.core.typed/Any]))
  (is (cf (fn [f] (f [1 2]))
          [(clojure.core.typed/All [b ...]
                ['[b ... b] -> '[b ... b]])
           -> '['1 '2]]))
  (is (cf (fn [f] (f (fn [a] a)))
          [(clojure.core.typed/All [b ...]
                [[b ... b -> clojure.core.typed/Any] -> [b ... b -> clojure.core.typed/Any]])
           -> [clojure.core.typed/Any clojure.core.typed/Any -> clojure.core.typed/Any]]))
  (is (cf (fn [f] (f))
          [(clojure.core.typed/All [b ...]
                [-> [b ... b -> clojure.core.typed/Any]])
           -> clojure.core.typed/Any #_['1 '2 -> clojure.core.typed/Any]]))
  (is-tc-err
    (fn [f] (second (f [1 2])))
    [(clojure.core.typed/All [b ...]
          ['[b ... b] -> '[b ... b]])
     -> '1]))

(deftest deref-var-test
  (is-tc-e @#'+ 
           [Number -> Number]))

(deftest static-pred-test
  (is (check-ns 'clojure.core.typed.test.pred-hmap)))

(deftest infer-def-test
  (is-tc-err (do (def a 1)
                 a))
  (is-tc-err (do (t/def a :- t/Num, 1)
                 (defn b [_] 'a)
                 (b a))
             t/Symbol))

(deftest typed-for-test
  (is (check-ns 'clojure.core.typed.test.for))
  (is-tc-e
    (for
      [a :- (t/U nil Number), [1 nil 2 3]
       b :- Number, [1 2 3]
       :when a]
      :- Number
      (+ a b))
    (t/Seq Number))
  (is-tc-err
    (for
      [a :- (t/U t/Symbol nil Number), [1 nil 2 3]
       b :- Number, [1 2 3]]
      :- Number
      (+ a b)))
  (is-tc-e (for [a :- t/Num [1 2 3]]
             (inc a))
           (t/Seq t/Any))
  (is-tc-err (lazy-seq 1))
  (is-tc-e (lazy-seq nil))
  (is-tc-e (fn a [] :- (t/Seq t/Any) (lazy-seq (a))))
  (is-tc-e (fn a [] :- (t/Seq t/Any) (lazy-seq (a))))
  (is-tc-e (t/let [] 1))
  (is-tc-e
    (inc ((fn []
            (if 1 2 3)))))
  (is-tc-e
    (inc ((core/fn []
            (if 1 2 3)))))
  (is-tc-e
    (inc ((fn [] :- t/Num
            (if 1 2 3)))))
  (is-tc-e (for [a :- t/Num [1 2 3]] :- t/Num
             (inc a))
           (t/Seq t/Num))
  (is-tc-err (for [a [1 2 3]]
               (inc a))))


(deftest file-not-found-error-test
  (is (err/top-level-error-thrown?
        (check-ns 'this.doesnt-exist))))

(deftest CTYP146-test
  (is (check-ns 'clojure.core.typed.test.CTYP146)))

(deftest defn-test
  (is-tc-e (defn add-two [a :- t/Int] :- t/Int
             (+ a 2))
           (t/Var1 [t/Int -> t/Int]))
  (is-tc-e (defn add-three 
             ([a :- t/Int] :- t/Int 
              (+ a 3)))
           #_(t/Var1 [t/Int -> t/Int]))
  ;; arg is annotated
  (is-tc-e (do
             (defn foo [a :- t/Num]
               (inc a))
             (foo 1)))
  (is-tc-err (do
               (defn foo [a :- t/Num]
                 (inc a))
               (foo nil)))
  ;; return type is not inferred
  (is-tc-err (do
               (defn foo [a :- t/Num]
                 (inc a))
               (inc (foo 1)))
             t/Num)
  ; old :forall syntax
  (is (thrown? AssertionError 
               (tc-e (defn [x]
                       foo 
                       ([a :- x] :- x
                        a)))))
  ; mixing old+new :forall syntax
  (is (thrown? AssertionError 
               (tc-e (defn [x]
                       :forall [x]
                       foo 
                       ([a :- x] :- x
                        a)))))
  (is-tc-e (fn :forall [x]
             ([a :- x] :- x
              a)))
  (is-tc-e (fn :forall [x]
             ([a :- x] :- x
              a))
           (t/All [y] [y :-> y]))
  (is-tc-err (fn :forall [x]
               ([a :- x] :- x
                a))
             (t/All [x y] [x :-> y]))
  (is-tc-e (do
             (t/ann foo (t/All [y] [y :-> y]))
             (def foo (fn :forall [x]
                        ([a :- x] :- x
                         a)))))
  (is-tc-e (t/def foo :- (t/All [y] [y :-> y])
             (fn :forall [x]
               ([a :- x] :- x
                a))))
  (is-tc-e (defn 
             :forall [x]
             foo 
             ([a :- x] :- x
              a)))
  (is-tc-e (do (defn :forall [x]
                 foo 
                 ([a :- x] :- x
                  a))
               (foo 1))
           t/Num)
  (is-tc-err (do (defn :forall [x]
                   foo 
                   ([a :- x] :- x
                    a))
                 (foo nil))
             t/Num)
  )

(deftest atom-test
  (is-tc-e (atom :- (t/Vec t/Any) []) (t/Atom1 (t/Vec t/Any)))
  (is-tc-e @(atom 1) t/Any)
  (is-tc-e @(atom :- Number 1) Number)
  (is-tc-e (atom :- Number 1) (t/Atom1 Number))
  (is-tc-e (atom :- Number 1))
  (is-tc-e (atom (ann-form 1 Number)) (t/Atom1 Number))
  (is-tc-err (atom :- Number 1) (t/Atom1 Boolean))
)

(deftest ref-test
  (is-tc-e @(ref 1) t/Any)
  (is-tc-e @(ref :- Number 1) Number))

(deftest performance-CTYP83
  (is (check-ns 'clojure.core.typed.test.CTYP-83-performance)))

(deftest count-set-test
  (is-tc-e (let [v :- (t/Vec t/Int) [1 2 3]
                 _ (assert (#{1 2 3} (count v)))]
             (first v))
           t/Int)
  (is-tc-err (let [v :- (t/Vec t/Int) [1 2 3]
                   _ (assert (#{0 1 2 3} (count v)))]
               (first v))
             t/Int)
  (is-tc-e (let [v :- (t/Vec t/Int) [1 2 3]
                 _ (assert (#{1 2 3} (count v)))]
             (nth v 0 nil))
           t/Int)
  (is-tc-err (let [v :- (t/Vec t/Int) [1 2 3]
                   _ (assert (#{0 1 2 3} (count v)))]
               (nth v 0 nil))
             t/Int)
  (is-tc-e (let [v :- (t/Vec t/Int) [1 2 3]
                 _ (assert (#{1 2 3} (count v)))]
             (nth v 0))
           t/Int)
  ; we let nth fail at runtime here
  (is-tc-e (let [v :- (t/Vec t/Int) [1 2 3]
                 _ (assert (#{0 1 2 3} (count v)))]
             (nth v 0))
           t/Int))

(deftest keyword-default-arg-test
  (is-tc-e (:a {} 0) t/Int)
  (is-tc-e (:a {} 0) t/Int)
  (is-tc-err (:a {}) t/Int)
  (is-tc-e
    (fn [m :- (t/Map t/Keyword Long)] :- Long
      (:a m 0)))
  (is-tc-e
    (fn [m :- (t/Map t/Keyword Long)] :- Long
      (get m :a 0))))

(defn top-tfn1 []
  (parse-type `(t/TFn [[~'x :variance :covariant]] t/Any)))

(defn bot-tfn1 []
  (parse-type `(t/TFn [[~'x :variance :covariant]] t/Nothing)))

(defmacro with-hk-m [& body]
  `(with-bounded-frees {(make-F '~'m) (-bounds (top-tfn1) (bot-tfn1))}
     ~@body))

(deftest subtype-hk-app
  (is-clj (with-hk-m
            (sub?-q `(~'m t/Int) `(~'m t/Int))))
  (is-clj (with-hk-m
            (sub?-q `(~'m t/Int) `(~'m t/Num))))
  (is-clj (with-hk-m
            (sub?-q `(~'m t/Int) `(t/U (~'m t/Int) (clojure.lang.Reduced t/Int)))))
  (is-clj (with-hk-m
            (sub?-q `(~'m t/Int) `(t/U (~'m t/Int) (clojure.lang.Reduced t/Int)))))
  (is-clj (with-hk-m
            (sub?-q `(~'m t/Int) `(t/U (~'m t/Num) (clojure.lang.Reduced t/Int)))))
  )

(deftest anon-fn
  (is-tc-e (inc ((fn [a :- t/Num] a) 1)))
  (is-tc-e (fn foo 
             ([a :- t/Num] :- t/Num (foo 1 a))
             ([a :- t/Num b :- t/Num] :- t/Num b)))
  (is-tc-e (inc ((fn foo 
                   ([a :- t/Num] :- t/Num (foo 1 a))
                   ([a :- t/Num b :- t/Num] :- t/Num b))
                 1)))
  (is-tc-e (inc ((fn foo 
                   ([a :- t/Num] (foo 1 a))
                   ([a :- t/Num b :- t/Num] :- t/Num b))
                 1)))
  (is-tc-err (inc ((fn foo 
                     ([a :- t/Num] (foo 1 a))
                     ([a :- t/Num b :- t/Num] b))
                   1)))
  (is-tc-e (clojure.core/fn foo [a] (foo a)))
  (is-tc-e (fn [b] (inc b))
           (t/Rec [b]
             (t/U t/Num [t/Num -> t/Num])))
  (is-tc-e (core/fn 
             ([b] b)
             ([b c] [b c]))
           [t/Any -> t/Any])
  (is-tc-e (core/fn [b] (inc b))
           (t/U t/Num [t/Num -> t/Num]))
  (is-tc-e (core/fn [b] (inc b))
           (t/Rec [b]
             (t/U t/Num [t/Num -> t/Num])))
  (is-tc-e (fn [b] (inc b))
           [t/Num -> t/Num])
  (is-tc-err (fn 
               ([b] (inc b)))
             (t/IFn [t/Num -> t/Num]
                  [t/Num t/Num -> t/Num]))
  (is-tc-err (fn 
               ([b] (inc b)))
             t/Num)
  (is (both-subtype?
        (ret-t
          (tc-t (fn
                  [x :- t/Any 
                   y :- t/Any])))
        (parse-clj `[t/Any t/Any :-> nil :filters {:then ~'ff :else ~'tt}])))
  ; interesting case, perfectly valid to remember t/Any is falsy here
  (is (both-subtype?
        (ret-t
          (tc-t (fn
                  [x :- t/Any 
                   y :- t/Any]
                  :- t/Any nil)))
        (parse-clj `[t/Any t/Any :-> t/Any :filters {:then ~'ff :else ~'tt}])))
  (is (both-subtype?
        (ret-t
          (tc-t (fn
                  [x :- t/Any 
                   y :- t/Any]
                  :- t/Any
                  (throw (Exception. "a")))))
        (parse-clj `[t/Any t/Any :-> t/Nothing :filters {:then ~'ff :else ~'ff}]))))

(deftest pfn-test
  (is-tc-e (fn :forall [x]
             [a :- x] :- x
             a))
  (is-tc-e (fn :forall [x]
             [a :- x] :- x
             a))
  (is-tc-err (ann-form
               (fn :forall [x]
                 [a :- x] :- x
                 a)
               (t/All [x y z] [t/Any t/Any -> t/Any])))
  (is-tc-err (fn :forall [x]
               [a :- x] :- x
               a)
             :expected
             (t/All [x y z] [t/Any t/Any -> t/Any])))

(deftest unsafe-body-test
  (is
    (FnIntersection? (Poly-body-unsafe* (parse-clj `(t/All [a# b# c# d# x#] [x# :-> nil])))))
  (is
    (FnIntersection? (PolyDots-body-unsafe* (parse-clj `(t/All [x# ...] [nil ... x# :-> nil])))))
  (is
    (FnIntersection? (PolyDots-body-unsafe* (parse-clj `(t/All [a# b# c# d# x# ...] [nil ... x# :-> nil]))))))

#_(deftest reduce-test
  (is-tc-err (reduce (fn ([] :- nil) ([x :- t/Num y :- t/Num] :- nil)) [1]))
  (is-tc-e (reduce (fn ([] :- t/Num 1) ([x :- t/Num y :- t/Num] :- t/Num 1)) [1]))
  (is-tc-err (reduce (fn ([x :- t/Num y :- t/Num] :- t/Num 1)) [1]))
  (is-tc-err (reduce (fn ([x :- t/Num y :- t/Num] :- t/Num 1)) [1]))
  (is-tc-e (reduce (fn ([x :- t/Num, y :- t/Num] 1) ([] 1)) [1])))


(deftest fn-type-parse-test
  (is (not (= (-FS -bot -bot)
              (-FS -top -bot))))
  (is (not (= (-FS -bot -bot)
              (-FS -bot -top))))
  (is (= (:then (-FS -bot -bot))
         -bot))
  (is (= (:else (-FS -bot -bot))
         -bot))
  (is (= (parse-clj `[t/Any :-> t/Any])
         (parse-clj `[t/Any :-> t/Any
                      :filters {:then ~'tt :else ~'tt}])))
  (is (-> (parse-clj `[t/Any :-> t/Any
                       :filters {:then ~'ff :else ~'ff}])
          :types first :rng :fl #{(-FS -bot -bot)}))
  (is (-> (parse-clj `[t/Any :-> t/Any
                       :filters {:then ~'ff :else ~'ff}])
          :types first :rng :fl :then BotFilter?))
  (is (-> (parse-clj `[t/Any :-> t/Any
                       :filters {:then ~'ff :else ~'ff}])
          :types first :rng :fl :else BotFilter?)))

(deftest subtype-filter-test
  (testing "top and bot"
    (is (sub/subtype-filter? -top -top))
    (is (sub/subtype-filter? -bot -top))
    (is (sub/subtype-filter? -bot -bot))
    (is (not (sub/subtype-filter? -top -bot))))
  (testing "this simplifies to top"
    (is (= (-filter -any 'a) -top)))
  (testing "this doesn't simplify to top"
    (is (not= (-filter -true 'a) -top)))
  (testing "combine type-filter and top/bot"
    (is (sub/subtype-filter? (-filter -true 'a) -top))
    (is (sub/subtype-filter? -bot (-filter -true 'a)))
    (is (not (sub/subtype-filter? -top (-filter -true 'a))))
    (is (not (sub/subtype-filter? (-filter -true 'a) -bot))))
  (testing "simple type-filters"
    (is-clj (sub/subtype-filter? (-filter -true 'a) (-filter -true 'a)))
    (testing "different types, that are subtypes"
      (is-clj (sub/subtype-filter? (-filter -false 'a) (-filter (parse-clj `Boolean) 'a)))
      (is-clj (sub/subtype-filter? (-filter -false 'a [(-kpe :a)]) (-filter (parse-clj `Boolean) 'a [(-kpe :a)]))))
    (testing "different types, not subtypes"
      (is-clj (not (sub/subtype-filter? (-filter -false 'a) (-filter -true 'a))))
      (is-clj (not (sub/subtype-filter? (-filter -false 'a [(-kpe :a)]) 
                                        (-filter -true 'a [(-kpe :a)])))))
    (testing "different paths, but types happen to be subtypes (still should fail)"
      (is-clj (not (sub/subtype-filter? (-filter -true 'a) (-filter -true 'b))))
      (is-clj (not (sub/subtype-filter? (-filter -false 'a) (-filter (parse-clj `Boolean) 'b))))
      (is-clj (not (sub/subtype-filter? (-filter -false 'a) (-filter (parse-clj `Boolean) 'a [(-kpe :a)])))))
    )
  (testing "or filter"
    (is-clj (sub/subtype-filter? (-filter -true 'a) (-or (-filter -true 'a) (-filter -false 'b))))
    (is-clj (not (sub/subtype-filter? (-filter -false 'a) (-or (-filter -true 'a) (-filter -false 'b)))))
    (is-clj (not (sub/subtype-filter? (-or (-filter -true 'a) (-filter -false 'b)) (-filter -true 'a))))
    (is-clj (sub/subtype-filter? (-or (-filter -true 'a) (-filter -false 'b)) (-or (-filter -true 'a) (-filter -false 'b))))
    (is-clj (sub/subtype-filter? (-or (-filter -true 'a) (-filter -false 'b)) 
                                 (-or (-filter (parse-clj `Boolean) 'a) (-filter (parse-clj `Boolean) 'b))))
    (is-clj (not
              (sub/subtype-filter? (-or (-filter (parse-clj `Boolean) 'a) (-filter (parse-clj `Boolean) 'b))
                                   (-or (-filter -true 'a) (-filter -false 'b))))))
  (testing "and filter"
    (is-clj (not
              (sub/subtype-filter? (-filter -true 'a) 
                                   (-and (-filter -true 'a) (-filter -false 'b)))))
    (is-clj (sub/subtype-filter? (-and (-filter -true 'a) (-filter -false 'b))
                                 (-filter -true 'a)))
    (is-clj (not
              (sub/subtype-filter? (-and (-filter (parse-clj `Boolean) 'a) (-filter (parse-clj `Boolean) 'b))
                                   (-and (-filter -true 'a) (-filter -false 'b)))))
    (is-clj (sub/subtype-filter? (-and (-filter -true 'a) (-filter -false 'b))
                                 (-and (-filter (parse-clj `Boolean) 'a) (-filter (parse-clj `Boolean) 'b))))
    )
  )

(deftest reduced?-test
  (testing "a plain old object"
    (is-tc-e (reduced? :a)))
  (testing "a nil"
    (is-tc-e (reduced? nil)))
  (testing "control flow + inlining"
    (is-tc-e (let [r :- (t/U nil (clojure.lang.Reduced t/Any)) (reduced 1)]
               (when (reduced? r)
                 @r))))
  (testing "an t/Any"
    (is-tc-e (fn [x :- t/Any] (reduced? x)))))


;(deftest dotted-apply-test
;  (is-tc-e
;    (do (ann foo (t/All [x y ...] [[y ... y -> x] -> [y ... y -> x]]))
;        (defn foo
;          [f]
;          (let [mem (memoize (fn [& args] #(apply f args)))]
;            (fn [& args]
;              ((apply mem args))))))))

(ann-form vector [Number * -> '[Number]])
#_(cf (inst vector Number Number))
#_(is (cf (juxt (inst vector clojure.core.typed/Any))))
#_(is (cf (juxt (ann-form vector [Number * -> '[Number]]))))
#_(cf (t/juxt first :- [(t/Seq Number) -> (t/U nil Number)]
              rest :- [(t/Seq Number) -> (t/Seq Number)]))

;(clojure.core.typed/All [b ...] [b ... b -> (t/HVec [b ... b])]) <: [java.lang.Number * -> (t/HVec [java.lang.Number])]

(deftest locking-test
  (testing "return value is the final expr"
    (is-tc-e (locking :a (+ 1 2)) Number))

  (testing "can't lock nil"
    (is-tc-err #(locking nil (+ 1 2) Number)))

  (testing "incorrect return value"
    (is-tc-err (locking :a :b) Number)))

(deftest CTYP-169-count-pe-test
  (is-tc-e (defn f [c :- clojure.lang.Counted] :- t/Int (count c))))

(deftest path-type-test
  (is-clj (= (path-type -any nil)
             -any))
  (is-clj (= (path-type -nil nil)
             -nil))
  (is-clj (= (path-type -nil [(-kpe :a)])
             -nil))
  (is-clj (= (path-type (-complete-hmap {(-val :a) (-val :b)}) [(-kpe :a)])
             (-val :b)))
  (is-clj (= (path-type (-partial-hmap {(-val :a) (-val :b)}) [(-kpe :a)])
             (-val :b)))
  (is-clj (= (path-type (make-HMap :optional {(-val :a) (-val :b)}) [(-kpe :a)])
             (Un -nil (-val :b))))
  (is-clj (= (path-type (make-HMap :optional {(-val :a) (-val :b)}
                                   :complete? true) 
                        [(-kpe :a)])
             (Un -nil (-val :b))))
  (is-clj (= (path-type (make-HMap :optional {(-val :a) (-val :b)}
                                   :complete? true) 
                        [(-kpe :a)])
             (Un -nil (-val :b))))
  (is-clj (= (path-type (make-HMap :complete? true)
                        [(-kpe :a)])
             -nil))
  (is-clj (= (path-type (make-HMap :complete? false)
                        [(-kpe :a)])
             -any))
  (is-clj (= (path-type (make-HMap :absent-keys #{(-val :a)})
                        [(-kpe :a)])
             -nil))
  (is-clj (= (path-type -nil
                        [(-kpe :a)])
             -nil))
  (is-clj (= (path-type -any
                        [(-kpe :a)])
             -any))
  (is-clj (= (path-type (-val :b)
                        [(-kpe :a)])
             -any))
  ; um CountPE just returning t/Int will probably do
  (is-clj (both-subtype? 
            (path-type (-val :b)
                       [(CountPE-maker)])
            (Name-maker `t/Int)))
  (is-clj (both-subtype? (clj (path-type (parse-clj `(t/Vec t/Int))
                                         [(CountPE-maker)]))
                         (Name-maker `t/Int)))
  (is-clj (= (path-type (-hvec [(-val :a) (-val :b) (-val :c)])
                        [(NthPE-maker 0)])
             (-val :a)))
  (is-clj (= (path-type (-hvec [(-val :a) (-val :b) (-val :c)])
                        [(NthPE-maker 1)])
             (-val :b)))
  (is-clj (= (path-type (-hvec [(-val :a) (-val :b) (-val :c)])
                        [(NthPE-maker 2)])
             (-val :c)))
  (is-clj (= (path-type (-hvec [(-val :a) (-val :b) (-val :c)])
                        [(NthPE-maker 3)])
             -any))
  (is-clj (= (path-type (Un (-hvec [(-val :b)])
                            (-hvec [(-val :a) (-val :b) (-val :c)]))
                        [(NthPE-maker 0)])
             (Un (-val :a) (-val :b))))
  )

(deftest aliasing-test
  (is-tc-e
    (let* [map__64934 {} 
           map__64934 (if (clojure.core/seq? map__64934) 
                        (clojure.lang.PersistentHashMap/create (clojure.core/seq map__64934)) 
                        map__64934)
           b (clojure.core/get map__64934 :b)] 
      (ann-form b (t/U nil Number))))
  (is-tc-err
    (let [{:keys [b]} {}] (ann-form b Number)))
  (is-tc-e
    (let [m {}
          b (or (:b m) 3)]
      (ann-form b Number)))
  (is-tc-e
    (let [m {}
          b (get m :b 3)]
      (ann-form b Number)))
  (is-tc-e
    (let [m {}
          b (if (print-filterset "a" (:b m)) (:b m) 3)]
      (ann-form b Number)))
  (is-tc-e
    (do
      (ann-record FooRec [a :- Number])
      (defrecord FooRec [a])
      (let [{:keys [a]} (->FooRec 1)]
        (ann-form a Number))))
  (is-tc-e
    (do
      (ann-record FooRec [a :- Number])
      (defrecord FooRec [a])
      (:a (->FooRec 1))))
  ;FIXME weird error?
  #_(is-tc-e
    (do
      (ann-record FooRec [a :- Number])
      (defrecord FooRec [a])
      (:a (:a (->FooRec 1)))))
  ; used to take infinite time
  (is-tc-e
    (let [{:keys [a b c d e f g h] :as props}
          (ann-form {} '{})]
      (when-not (and a b c d e f g h)
        #_(print-env "")
        props)))
  (testing "simultaneous local updates"
    (is-tc-e
      (let [old :- (t/U nil t/Num) 1
            new old]
        (when (number? new)
          (+ old new))))
    (is-tc-e
      (let [old :- (t/U nil t/Num) 1
            new old]
        (when (number? old)
          (+ old new))))
    (is-tc-e
      (let [{lkup :a} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? lkup)
          (inc alias))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? (:a old))
          (inc lkup))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? (:a old))
          (inc (:a old)))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? (:a old))
          (inc alias))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? alias)
          (inc (:a old)))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? alias)
          (inc lkup))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? lkup)
          (inc alias))))
    (is-tc-e
      (let [{lkup :a :as old} :- '{:a (t/U nil t/Num)} {:a 1}
            alias lkup]
        (when (number? lkup)
          (inc (:a old)))))
    (is-tc-e
      (do
        (ann parent ['{:file (t/U nil java.io.File)} -> (t/U nil t/Str)])
        (cc/defn parent [m]
          (let [^java.io.File file (:file m)]
            (when (instance? java.io.File (:file m))
              (.getParent file))))))
    (is-tc-e
      (defn parent [m :- '{:str (t/U nil t/Str)}]
        (when (:str m)
          (ann-form (:str m) t/Str))))
    (is-tc-e
      (do
        (ann parent ['{:file (t/U nil java.io.File)} -> (t/U nil t/Str)])
        (cc/defn parent [m]
          (let [^java.io.File file (:file m)]
            (when (:file m)
              (.getParent file))))))
    (is-tc-e
      (fn [{:keys [a]} :- (t/HMap :mandatory {:a t/Num}
                                :absent-keys #{:b})]
        (when a
          (inc a))))
    (is-tc-e
      (fn [{:keys [a] :as m} :- (t/HMap :optional {:a t/Num})]
        (when (print-filterset "a" a)
          (inc a))))
    (is-tc-e
      (fn [{:keys [a] :as m} :- (t/U (t/HMap :mandatory {:a t/Num})
                                   (t/HMap :absent-keys #{:a}))]
        (when (print-filterset "a" a)
          (inc a))))
    (is-tc-e
      (fn [{:keys [a b]} :- (t/U (t/HMap :mandatory {:a t/Num}
                                     :complete? true)
                               (t/HMap :mandatory {:b t/Num}
                                     :complete? true))]
        (when (and a b)
          (+ a b))))
    (is-tc-e
      (fn [{:keys [a b]} :- (t/U (t/HMap :mandatory {:a t/Num}
                                     :absent-keys #{:b})
                               (t/HMap :mandatory {:b t/Num}
                                     :absent-keys #{:a}))]
        (when (and a b)
          (+ a b))))))

(deftest flatten-test
  (is-tc-e (flatten nil))
  (is-tc-e (flatten [1 2]))
  (is-tc-e (vec (flatten [1 2])))
  (is-tc-err (flatten :a)))

;(try (tc-e (for [x :- t/Int, [[1 2] [3 4]]] :- (t/Seq t/Int) x))
;     (catch Throwable e
;       (clojure.repl/pst e)))
;

(deftest CTYP-196-frees
  (is-tc-e (let [a first]
             a)))

(deftest recursive-defalias-test
  ;; List already refers to c.c.t/List
  (is (thrown?
        Throwable
        (tc-e (do (defalias List
                    (t/U '{:op ':cons
                         :car t/Any
                         :cdr List}
                       '{:op ':nil}))
                  (let [a :- List1, {:op :nil}
                        b :- List1, {:op :cons 
                                     :car 1
                                     :cdr a}])))))
  (is-tc-e
    (do (defalias List1
          (t/U '{:op ':cons
               :car t/Any
               :cdr List1}
             '{:op ':nil}))
        (let [a :- List1, {:op :nil}
              b :- List1, {:op :cons 
                           :car 1
                           :cdr a}]))))

(deftest hmap-optional-update-test
  (is-tc-e
    (fn [m :- (t/HMap :optional {:a (t/U nil Number)})]
      (if (:a m) (inc (:a m)) 0))))

(deftest group-by-test
  (is-tc-err (group-by (inst identity t/Num) [1 2 3])
             (t/Map t/Num t/Num))
  (is-tc-e (group-by (inst identity t/Num) [1 2 3])
           (t/Map t/Num (t/Vec t/Num))))

(deftest CTYP-189-test
  (is-tc-e (for [x :- t/Int []] :- t/Int x)))

(comment
  (tc-e (let [x 1] (loop [x x] x)))
  (tc-e (loop [x 1] x))
(-> (ana/ast-for-form '(clojure.core.typed/for [x :- t/Int []] :- t/Int x))
    emit-form/emit-form
    pprint)
(-> (ana/ast-for-form '(let [x 1] (loop [x x] x)))
    :body
    :ret
    :body
    :ret

    #_emit-form/emit-form
    #_pprint)
)


(deftest CTYP-215-zero?-test
  ; inlinings
  (is-tc-e (zero? 1) Boolean)
  (is-tc-err #(zero? 'a) [-> Boolean])
  (is-tc-e zero? [Number -> Boolean]))

(deftest CTYP-181-prim-cast-test
  (is-tc-e float [Number -> Float])
  ;; inlinings
  (is-tc-e (float 1) Float)
  (is-tc-err #(float 'a) [-> Float])
  (is-tc-err (let [^Character c \c]
               (float c))
             Float)

  (is-tc-e double [Number -> Double])
  ;; inlinings
  (is-tc-e (double 1) Double)
  (is-tc-err #(double 'a) [-> Double])
  (is-tc-err (let [^Character c \c]
               (double c))
             Double)

  (is-tc-e int [(t/U Character Number) -> Integer])
  ;; inlinings
  (is-tc-e (int 1) Integer)
  (is-tc-e (int \c) Integer)
  (is-tc-err #(int 'a) [-> Integer])

  (is-tc-e long [(t/U Character Number) -> Long])
  ;; inlinings
  (is-tc-e (long 1) Long)
  (is-tc-e (let [^Character c \c]
             (long c)) 
           Long)
  (is-tc-err #(long 'a) [-> Long])

  (is-tc-e num [Number -> Number])
  ;; inlinings
  (is-tc-e (num 1) Number)
  (is-tc-err (let [^Character c \c]
               (num c))
             Number)
  (is-tc-err #(num 'a) [-> Number])

  (is-tc-e short [(t/U Character Number) -> Short])
  ;; inlinings
  (is-tc-e (short 1) Short)
  (is-tc-e (let [^Character c \c]
             (short c))
           Short)
  (is-tc-err #(short 'a) [-> Short])

  (is-tc-e byte [(t/U Character Number) -> Byte])
  (is-tc-e (byte 1) Byte)
  (is-tc-e (let [^Character c \c]
             (byte c))
           Byte)
  (is-tc-err #(byte 'a) [-> Byte])

  (is-tc-e char [(t/U Character Number) -> Character])
  (is-tc-e (char 1) Character)
  (is-tc-e (char \c) Character)
  (is-tc-err #(char 'a) [-> Character])
  )

(deftest CTYP-170-test
  (is-tc-e (apply concat [[]])))

(deftest CTYP-200-test
  (is-tc-e (min 1 2) t/Int)
  (is-tc-e (max 1 2) t/Int)
  (is-tc-e (#'min 1 2) t/Int)
  (is-tc-e (#'max 1 2) t/Int))

(deftest do-exp-repl-test
  (is-tc-e (do (require '[clojure.core :as c])
               (c/map inc []))))

; promote-demote bug with HSet's
(deftest CTYP-214-test
  (is-tc-e (atom #{})))

(deftest seq-branch-test
  (is-tc-e (if (seq [1 2 3]) 1 nil)
           t/Num)
  (is-tc-e (if (seq []) 1 nil)
           nil)
  (is-tc-err (if (seq (ann-form [] (t/Seqable t/Num))) 1 nil)
             nil)
  (is-tc-err (if (seq (ann-form [1] (t/Seqable t/Num))) 1 nil)
             t/Num))

(deftest quote-string-test
  (is-tc-e "a" '"a")
  (is-tc-err "a" '"b")
  (is-tc-e "a" (t/Val "a")))

(deftest keyword-pe-test
  ;; with keywords
  (is-tc-e (do 
             (defalias M (t/U '{:op ':plus
                              :plus t/Int}
                            '{:op ':minus
                              :minus t/Int}))
             (let [m :- M, {:op :plus
                            :plus 1}]
               (if (-> m :op #{:plus})
                 (inc (:plus m))
                 (dec (:minus m))))))
  ;; with strings, via keyword
  (is-tc-e (do 
             (defalias M (t/U '{:op '"plus"
                              :plus t/Int}
                            '{:op '"minus"
                              :minus t/Int}))
             (let [m :- M, {:op "plus"
                            :plus 1}]
               (if (-> m :op keyword #{:plus})
                 (inc (:plus m))
                 (dec (:minus m))))))
  ;; defmulti dispatch
  (is-tc-e (do 
             (defalias M (t/U '{:op '"plus"
                              :plus t/Int}
                            '{:op '"minus"
                              :minus t/Int}))
             (ann f [M -> t/Int])
             (defmulti f (fn [m :- M] (-> m :op keyword)))
             (defmethod f :plus [m] (inc (:plus m)))
             (defmethod f :minus [m] (inc (:minus m)))))
  ;; polymorphic setting
  (is-tc-e (map keyword '[a b c]))
  ;; with symbols, via keyword
  (is-tc-e (do 
             (defalias M (t/U '{:op 'plus
                              :plus t/Int}
                            '{:op 'minus
                              :minus t/Int}))
             (let [m :- M, {:op 'plus
                            :plus 1}]
               (if (-> m :op keyword #{:plus})
                 (inc (:plus m))
                 (dec (:minus m))))))
  ;; with keywords and nil, via keyword
  (is-tc-e (do 
             (defalias M (t/U '{:op ':plus
                              :plus t/Int}
                            '{:op nil
                              :minus t/Int}))
             (let [m :- M, {:op :plus
                            :plus 1}]
               (if (-> m :op keyword #{:plus})
                 (inc (:plus m))
                 (dec (:minus m))))))
  (is-tc-e (do 
             (defalias M (t/U '{:op t/Num
                              :plus t/Int}
                            '{:op t/Str
                              :minus t/Int}))
             (let [m :- M, {:op 1
                            :plus 1}]
               (if (-> m :op keyword not)
                 (inc (:plus m))
                 (dec (:minus m))))))
  )

(deftest keyword-path-type-test
  (is-tc-e (keyword :a))
  (is-tc-e (keyword (ann-form :a t/Kw)))
  (is-tc-e (let [k (keyword (ann-form :a t/Kw))]
             (name k)))
  (is (=
       (-val :a)
       (path-type (-val 'a)
                  [(KeywordPE-maker)])))
  (is-tc-e (fn [k :- 'a] :- t/Kw
             (let [i (keyword k)]
               i)))
  ; need symbol literals as objects
  ;(is-tc-e (keyword 'a) ':a)
  (is-tc-e (fn [k]
             (let [i (keyword k)]
               i))
           (t/IFn ['a -> ':a]
                ['a/b -> ':a/b]
                ['"a" -> ':a]
                ['"a/b" -> ':a/b]
                [':a -> ':a]
                [':a/b -> ':a/b]
                [t/Sym -> t/Kw]
                [t/Kw -> t/Kw]
                [t/Str -> t/Kw]
                [(t/U t/Sym t/Kw t/Str) -> t/Kw]
                [nil -> nil]
                [t/Any -> (t/U nil t/Kw)]))
  (is-tc-e (fn [k :- t/Any] :- t/Kw
             (let [i (keyword k)]
               (assert (keyword? k))
               i)))
  (is-tc-e (fn [k :- t/Any] :- (t/U t/Kw t/Str t/Sym)
             (let [i (keyword k)]
               (assert (keyword? i))
               k)))
  (is-tc-err (fn [k :- t/Any] :- (t/U t/Kw t/Str)
               (let [i (keyword k)]
                 (assert (keyword? i))
                 k)))
  )

(deftest rewrite-reflecting-method-test
  (is-tc-err (fn [a] (.getParent a)))
  (is
    (should-not-reflect
      (tc-e 
        (fn [^java.io.File a] (.getParent a))
        [java.io.File -> t/Any])))
  (is
    (should-not-reflect
      (tc-e 
        (let [^java.io.File a (java.io.File. "a")]
          (.getParent a)))))
  (is
    (should-not-reflect
      (tc-e 
        (let [a (java.io.File. "a")]
          (.getParent a)))))
  (is
    (should-not-reflect
      (tc-e 
        (fn [a] (.getParent a))
        [java.io.File -> t/Any])))
  (is
    (should-not-reflect
      (tc-e 
        (fn [a] (.getParent a))
        [java.io.File -> (t/U nil t/Str)])))
  (testing "the type hint can be inferred, but we only add type
           hints to :local nodes"
    (is (tc-err
          (.getParent (do (if (zero? 0) (java.io.File. "a") "a"))))))
  (testing "special form fn with inline annotations should rewrite body"
    (is
      (should-not-reflect
        (tc-e 
          (fn [a :- java.io.File]
            (.getParent a))))))
  (is
    (should-not-reflect
      (tc-e 
        (fn [a] 
          {:pre [(instance? java.io.File a)]}
          (.getParent a)))))
  (is 
    (should-not-reflect
      (tc-e 
        (fn [a] (.getParent a))
        [java.io.File -> t/Any]))))

(deftest rewrite-reflecting-ctor-test
  (is-tc-err #(java.io.File. 1))
  (is-tc-err (fn [a]
               (java.io.File. a)))
  (is
    (should-not-reflect
      (tc-e (fn [a]
              (java.io.File. a))
            [t/Str -> t/Any])))
  (testing "special fn with inline"
    (is
      (should-not-reflect
        (tc-e (fn [a :- t/Str]
                (java.io.File. a))))))
  (is
    (should-not-reflect
      (tc-e (let [[a] [(str "a")]]
              (java.io.File. a)))))
  (testing "type hint is only added via a :local node"
    (is-tc-err (java.io.File. (first [(str "a")]))))
  (is-tc-err #(let [[a] [(long 1)]]
                (java.io.File. a)))
  (is-tc-e (fn [a]
             (java.io.File. a))
           [t/Str -> java.io.File]))

;; unclear what this is testing, but it's been fixed sometime after 0.2.11
(deftest CTYP-80-test
  (is-tc-e (do (defalias SLiteral (t/U t/Sym (t/Option (t/Seqable SLiteral))))

               (ann s-check (t/Pred t/Symbol))
               (cc/defn s-check
                 [x]
                 (symbol? x))

               (ann s-test [SLiteral -> SLiteral])
               (cc/defn s-test
                 [sliteral]
                 (if (s-check sliteral)
                   'win
                   (second sliteral))))))

(defmacro typed-defn-check-meta [meta-fn & args]
  (let [g (gensym "v")]
    `(do (t/defn ~g ~@args)
         (~meta-fn (meta (var ~g))))))

(defmacro named-typed-defn-check-meta [meta-fn nme & args]
  (let [g (with-meta (gensym "v")
                     (meta nme))]
    `(do (t/defn ~g ~@args)
         (~meta-fn (meta (var ~g))))))

(deftest CTYP-168-test
  (testing ":arglists metadata is added with typed defn"
    (is (typed-defn-check-meta
          #(not= '([]) (:arglists %))
          [a] a))
    (is (typed-defn-check-meta
          #(= '([a]) (:arglists %))
          [a] a))
    (is (typed-defn-check-meta
          #(= '([a]) (:arglists %))
          ([a] a)))
    (is (typed-defn-check-meta
          #(= '([a] [b c]) (:arglists %))
          ([a] a) ([b c] c))))
  (testing ":doc metadata works"
    (is (typed-defn-check-meta
          (comp #{"blah"} :doc)
          "blah" [a] a)))
  (testing "typed defn supports metadata map"
    (is (typed-defn-check-meta
          #(not (contains? % :foo))
          [a] a))
    (is (typed-defn-check-meta
          #(and (= '([a]) (:arglists %))
                (= 1 (:foo %)))
          {:foo 1} [a] a))
    (testing "metadata map is always merged last"
      (is (typed-defn-check-meta
            #(= '([]) (:arglists %))
            {:arglists '([])} [a] a))
      (is (typed-defn-check-meta
            (comp #{"b"} :doc)
            "a"
            {:doc "b"} [a] a))
      (is (named-typed-defn-check-meta
            #(= 1 (:baz %))
            ^{:baz 1} nme
            [a] a))
      (is (named-typed-defn-check-meta
            #(= 2 (:baz %))
            ^{:baz 1} nme
            {:baz 2} [a] a)))))

(deftest CTYP-212-test
  (is-tc-e (promise)
           (t/Promise t/Int))
  (is-tc-e (ann-form (promise) (t/Promise t/Int)))
  (is-tc-e (clojure.core.typed/ann-form (promise) (t/Promise t/Int)))
  (is-tc-e
    (do
      (ann-record MyRecord [p :- (t/Promise t/Int)])

      (defrecord MyRecord [p])

      (defn foo []
        (let [x :- (t/Promise t/Int) (promise)])))))

(deftest should-check-ns-form-test
  (testing "does not depend on core.typed, don't check"
    (is-clj
      (= false
         (ndu/should-check-ns-form?
           '(ns foo))
         (ndu/should-check-ns-form?
           '(ns foo
              {:core.typed {:collect-only true}}))
         (ndu/should-check-ns? 'clojure.spec.alpha))))
  (testing ":collect-only should not check"
    (is-clj
      (= false
         (ndu/should-check-ns-form?
           '(ns foo
              {:core.typed {:collect-only true}}
              (:require [clojure.core.typed])))
         (ndu/should-check-ns?
           'clojure.core.typed.test.CTYP-234.dep))))
  (testing "plain :require of clojure.core.typed should trigger check"
    (is-clj
      (= true
         (ndu/should-check-ns-form?
           '(ns foo
              (:require [clojure.core.typed])))
         (ndu/should-check-ns?
           'clojure.core.typed.test.CTYP-234.core)))))

(deftest CTYP-234-test
  (is (check-ns 'clojure.core.typed.test.CTYP-234.core)))

(deftest CTYP-203-test
  (is-tc-e
    (do
      (defalias BaseValidationSchema
        '[Boolean (t/HMap :complete? false)])

      (ann-form [true {}] BaseValidationSchema))))

(deftest CTYP-172-test
  (is-tc-e (fn [[a b] :- (t/I (t/Vec t/Num) (t/ExactCount 2))]
             (+ a b))))

(deftest CTYP-210-test
  (is-tc-e #(long (+ 2 (int 123)))))

(deftest CTYP-256-test
  (is (= (impl/with-clojure-impl
           (impl/impl-case
             :clojure 1
             :cljs 2))
         1))
  (is (= (impl/with-cljs-impl
           (impl/impl-case
             :clojure 1
             :cljs 2))
         2))
  (is (= (impl/impl-case
           :clojure 1
           :cljs 2
           :unknown 3)
         3))
  (is (thrown? AssertionError
               (impl/impl-case
                 :clojure 1
                 :cljs 2))))

(deftest gradual-untyped-import-test
  (is (try
        (do (load/load-typed-file "clojure/core/typed/test/gradual/import_untyped")
            ((requiring-resolve 'clojure.core.typed.test.gradual.import-untyped/bad)))
        false
        (catch ExceptionInfo e
          ;(prn (ex-data e))
          (and (-> (ex-data e) :blame)
               (= (select-keys (-> (ex-data e) :blame)
                               [:file :line :column :positive :negative])
                  {:line 16
                   :column 3
                   :file "clojure/core/typed/test/gradual/import_untyped.clj"
                   :negative "clojure.core.typed.test.gradual.import-untyped"
                   :positive "Annotation for clojure.core.typed.test.gradual.untyped/b"}))))))


(deftest CTYP-313-substitution-in-optional-hmap
  (is-tc-e (do
             (t/defalias OptMap
               (t/TFn [[z :variance :covariant :< Number]] (t/HMap :optional {:z z})))

             (t/defn z-getter
               [m :- (OptMap Integer)] :- (t/Option Integer)
               (:z m)))))

(deftest ctyp-294-infer-unannotated-vars
  (is (do (check-ns 'clojure.core.typed.test.ctyp-294.typed)
          (= 2
             (count
               (t/infer-unannotated-vars 'clojure.core.typed.test.ctyp-294.typed))))))

(deftest override-ctor-test
  ;; dumb override
  (is-tc-e 
    (do (override-constructor java.util.concurrent.LinkedBlockingQueue
                              (t/IFn
                                [String ->
                                 java.util.concurrent.LinkedBlockingQueue]))
        #(let [^int i "fo"]
           (java.util.concurrent.LinkedBlockingQueue. i))))
  (is-tc-e
    (do (override-constructor java.util.concurrent.LinkedBlockingQueue
                              (t/IFn
                                [String ->
                                 String]))
        #(let [^int i "fo"]
           (ann-form (java.util.concurrent.LinkedBlockingQueue. i)
                     String))))
  (is-tc-err
    (do (override-constructor java.util.concurrent.LinkedBlockingQueue
                              (t/IFn
                                [String ->
                                 String]))
        #(let [^int i "fo"]
           (ann-form (java.util.concurrent.LinkedBlockingQueue. i)
                     java.util.concurrent.LinkedBlockingQueue))))
  )


(deftest conflicting-anns-test
  (is-tc-e (do (ann foo t/Num)
               (def foo 1)
               (ann foo t/Bool)
               (def foo false)))
  ; FIXME this used to work with the removed def>
  #_
  (is-tc-e (do (clojure.core.typed/def foo :- t/Num 1)
               (clojure.core.typed/def foo :- t/Bool false)))
  )

(deftest subtype-heterogeneous*-with-repeat
  (let [t (impl/with-clojure-impl
            (parse-type `(t/HSequential [Number String] :repeat true)))]
    ; t/HVec, t/HSeq are all rely on t/HSequential to implement subtype
    (is-clj (subtype? (parse-type `(t/HSequential [Number String])) t))

    (is-clj (subtype? (parse-type `(t/HSequential [Number String Number String])) t))

    ; if both s and t have :repeat, then (count (:types t)) should <= (count (:types s))
    (is-clj (subtype? (parse-type `(t/HSequential [Number String Number String] :repeat true)) t))
    (is-clj (not (subtype? t (parse-type `(t/HSequential [Number String Number String] :repeat true)))))

    (is-clj (not (subtype? (parse-type `(t/HSequential [Number])) t)))

    (is-clj (not (subtype? (parse-type `(t/HSequential [Number String Number])) t)))

    ; they are same
    (is-clj (subtype? (parse-type `(t/HSequential [Number] :repeat true))
                      (parse-type `(t/HSequential [Number Number ~'*]))))
    (is-clj (subtype? (parse-type `(t/HSequential [Number Number ~'*]))
                      (parse-type `(t/HSequential [Number] :repeat true))))

    (is-clj (subtype? (parse-type `(t/HSequential [Number] :repeat true))
                      (parse-type `(t/HSequential [Number Number Number ~'*]))))

    (is-clj (not (subtype? (parse-type `(t/HSequential [Number] :repeat true))
                           (parse-type `(t/HSequential [Number Number String ~'*])))))

    (is-clj (not (subtype? (parse-type `(t/HSequential [Number] :repeat true))
                           (parse-type `(t/HSequential [Number String Number ~'*])))))

    (is-clj (subtype? (parse-type `(t/HVec [Number String Number String])) t))

    (is-clj (not (subtype? (parse-type `(t/HVec [Number String] :repeat true))
                           (parse-type `(t/HSequential [Number String Number String])))))

    (is-clj (subtype? (parse-type `(t/HSeq [Number String Number String])) t))))

(deftest function-prest
  (is-tc-e (fn [a & rst] 1) [Number (t/HSeq [Number String] :repeat true) <* -> Number])
  (is-tc-e (fn [a & rst]
             (when-not (empty? rst) (first rst)))
           [Number (t/HSeq [Number String] :repeat true) <* -> (t/U nil Number)])
  (is-tc-e (hash-map 1 "a" 2 "c" 3 "d") :expected (t/Map Number String))
  (is-clj (not (subtype? (parse-type `[(t/HSeq [String Number] :repeat true) ~'<* ~'-> String])
                         (parse-type `[(t/HSeq [String Number String] :repeat true) ~'<* ~'-> String]))))
  (is (check-ns 'clojure.core.typed.test.prest-cs-gen))
  (is-tc-e (map (inst hash-map Number String) [1 2 3] ["a b c"])
           :expected (t/NonEmptySeq (t/Map Number String)))
  (is-tc-err (hash-map 1 "a" 2 \c) :expected (t/Map Number String))
  )

(deftest normal-invoke-apply
  (is-tc-e (apply (inst hash-map Number String) 1 ["a"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" [2 "b"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" [2 "b" 3 "c"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" [2 "b" 3 "c" 4 "c"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" []) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" nil) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 "a" [2 \c]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 "a" [2 "b" 3 \c]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 \a [2 "c"]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 "a" [2 \c]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 \a [2 \c]) :expected (t/Map Number String))
  )

(deftest nil-empty-with-repeat
  (let [t (impl/with-clojure-impl
            (parse-type `(t/HSequential [Number String] :repeat true)))
        tt (impl/with-clojure-impl
             (parse-type `(t/All [k# v#] (t/HSequential [k# v#] :repeat true))))
        cg #(cs-gen #{} ;V
                    (zipmap '[k v] (repeat no-bounds)) ;X
                    {} ;Y
                    % ;S
                    tt)]
    (is-clj (subtype? (parse-type `(t/HSequential [])) t))
    (is-clj (subtype? (parse-type `(t/HVec [])) t))
    (is-clj (subtype? (parse-type `(t/HSeq [])) t))
    (is-clj (subtype? -nil t))
    (is-clj (subtype? -nil (parse-type `(t/HVec [Number String] :repeat true))))
    (is-clj (subtype? -nil (parse-type `(t/HSeq [Number String] :repeat true))))
    (is-clj (do (cg (parse-type `(t/HSequential []))) true))
    (is-clj (do (cg (parse-type `(t/HVec []))) true))
    (is-clj (do (cg (parse-type `(t/HSeq []))) true))
    (is-clj (do (cg -nil) true))
  ))

(deftest some?-test
  (is-tc-e (let [x (ann-form 1 (t/U nil t/Int))]
             (when (some? x)
               (inc x)))))

(deftest invoke-vector-test
  (is-tc-e (fn [a :- (t/Vec t/Int)] :- t/Int
             (a 0)))
  (is-tc-e (fn [a :- '[t/Int t/Int]] :- t/Int
             (a 0)))
  (is-tc-e (fn [a :- '[t/Int t/Bool]] :- t/Bool
             (a 1)))
  (is-tc-err (fn [a :- '[t/Int t/Bool]] :- t/Int
               (a 1)))
  (is-tc-e (fn [a :- (t/Vec t/Bool)] :- (t/Seqable t/Bool)
             (map a [1])))
  ;; upcast to t/IFn
  (is-tc-e (fn [a :- (t/Vec t/Bool)] :- [t/Int -> t/Bool]
             a))
  (is-tc-err (fn [a :- (t/Vec t/Bool)] :- [t/Int -> t/Int]
               a))
)

(deftest invoke-set-test
  ;; currently sets always return t/Any, but this checks
  ;; the expected type checking.
  (is-tc-err (fn [a :- (t/Set t/Int)] :- t/Int
               (a 1)))
  (is-tc-err (fn [a :- (t/HSet #{t/Int})] :- t/Int
               (a 1)))
  ;; test expected type checking
  (is-tc-err (fn [a :- (t/HSet #{:a})] :- t/Int
               (a 1)))
)

(deftest invoke-intersection-test
  (is-tc-e (fn [a :- (t/AVec t/Int)] :- t/Int
             (a 1))))

(deftest apply-concat-test
  (is-tc-e (apply concat []))
  (is-tc-e (apply concat [[1]]))
  (is-tc-e (apply distinct? [[1]])))

(deftest vals-path-test
  (is-tc-e (fn [blah :- (t/Map t/Int t/Int)]
             (let [foo (vals blah)]
               foo)))
  (is-tc-e (fn [blah :- (t/Map t/Int t/Int)]
             (let [foo (vals blah)]
               (apply + foo)))))

(deftest sorted-map-test
  (is-tc-e (hash-map))
  (is-tc-e (sorted-map))
  (is-tc-err (sorted-map 1))
  (is-tc-e (sorted-map 1 2)))

(deftest number-intersection-test
  (is-tc-e 
    (do
      (ann ^:no-check takes-int-coll [(t/Coll t/Int) -> t/Any])
      (def takes-int-coll identity)
      (ann ^:no-check num-coll-pred [(t/Coll t/Any) :-> Boolean :filters {:then (is (t/Coll t/Num) 0)}])
      (def num-coll-pred identity)
      (fn [e :- (t/Coll (t/U Character t/Int))]
        (when (num-coll-pred e)
          (when (seq e)
            (inc (first e))))))))

(deftest vector-as-first-class-function-test
  (is-tc-e (fn [a :- (t/Vec String)] :- (t/Seqable String)
             (map a (range 10)))))

(deftest group-by-annotation-test
  (is-tc-e #(group-by (inst identity t/Any) (range 10))))

(deftest metadata-ann-test
  (is-tc-e (for ^{:clojure.core.typed/ann t/Num} [^{:clojure.core.typed/ann t/Num} a [1 2 3]] a))
  (is-tc-err (for ^{:clojure.core.typed/ann t/Bool} [^{:clojure.core.typed/ann t/Num} a [1 2 3]] a))
  (is-tc-e #(clojure.core/loop [^{:clojure.core.typed/ann t/Num} a 1] (inc a)))
  (is-tc-err #(clojure.core/loop [^{:clojure.core.typed/ann t/Bool} a 1] (inc a)))
  ;;TODO
  ;(is-tc-e (doseq [^{:clojure.core.typed/ann t/Num} a [1 2 3]] (inc a)))
  ;(is-tc-err (doseq [^{:clojure.core.typed/ann t/Bool} a [1 2 3]] a))
  ;(is-tc-e (fn [^{:clojure.core.typed/ann t/Num} a] (inc a)))
  ;(is-tc-err (fn [^{:clojure.core.typed/ann t/Bool} a] (inc a)))
)

(deftest ann-namespace-alias-test
  (is (check-ns 'clojure.core.typed.test.ann-qualify.child)))

(deftest multimethod-no-expected-test
  (is-tc-e (do (ann f [t/Any :-> t/Any])
               (defmulti f identity)
               (defmethod f :foo [a]
                 1)))
  (is-tc-e (defmulti f identity))
  ;; expected type required
  (is-tc-err (do (defmulti f identity)
                 ;(ann-form f t/Nothing)
                 (defmethod f :foo [a]
                   1))))

(deftest typed-fn-return-empty-body
  (is (= ((clojure.core.typed/fn [] :-)) :-))
  (is (= ((clojure.core.typed/fn [] :- nil)) nil))
  (is (= ((clojure.core.typed/fn [] :- clojure.core.typed/Any)) nil))
  (is (= ((clojure.core.typed/fn [] :- clojure.core.typed/Any, nil)) nil))
  (is (= ((clojure.core.typed/fn [] :- nil, nil)) nil)))

#_
;TODO
(deftest nil-branch-test
  (is-tc-e (fn [a :- false]
             (when (false? a)
               :kw))
           [false :-> ':kw])
  (is-tc-e (fn [a :- false]
             (when (= false a)
               :kw))
           [false :-> ':kw])
  (is-tc-e (fn [a :- (t/U t/Num nil)]
             (if (= nil a)
               :kw
               a))
           [(t/U t/Num nil) :-> (t/U t/Num ':kw)])
  (is-tc-e (fn [a :- nil]
             (when (= nil a)
               :kw))
           [nil :-> ':kw])
  (is-tc-e (fn [a :- nil]
             (when (identical? nil a)
               :kw))
           [nil :-> ':kw])
  (is-tc-e (fn [a :- nil]
             (when (nil? a)
               :kw))
           [nil :-> ':kw]))

(deftest transducer-test
  (is (check-ns 'clojure.core.typed.test.transducer)))

(deftest TypeOf-test
  (is-tc-e (let [a :- t/Num, 1]
             (ann-form 2 (t/TypeOf a))))
  (is-tc-e (let [a :- t/Bool, true
                 a :- t/Num, 1]
             (ann-form 2 (t/TypeOf a))))
  (is-tc-err (let [a :- t/Num, 1
                   a :- t/Bool, true]
               (ann-form 2 (t/TypeOf a))))
  (is-tc-e (let [f (let [b :- t/Num, 1]
                     (fn [] :- (t/TypeOf b)
                       1))]
             (inc (f)))))

(deftest cf-throws-test
  (is (thrown? Throwable (cf (nil))))
  (is (thrown? Throwable (cf (clojure.core/fn [:- :a])))))

(deftest check-form-info-result-test
  (is (= 1 (:result (check-form-info '(do (do (do 1))))))))

;; here instead of typed.clj.analyzer to ensure they don't throw type errors
(deftest validate-pass-test
  (is (thrown-with-msg? ExceptionInfo
                        #"No such namespace: asdfds"
                        (tc-e asdfds/s)))
  (is (thrown-with-msg? ExceptionInfo
                        #"No such var: clojure.core"
                        (tc-e clojure.core/asdf))))

(deftest intersect-CountRange-test
  (is-clj (= (intersect-CountRange
               (make-CountRange 0 1)
               (make-CountRange 1))
             (intersect-CountRange
               (make-CountRange 1)
               (make-CountRange 0 1))
             (make-CountRange 1 1))))

(deftest HashMap-test
  (is-tc-e (new java.util.HashMap))
  (is-tc-e (new java.util.HashMap 1))
  (is-tc-e (new java.util.HashMap 1 (float 2.0)))
  (is-tc-e (new java.util.HashMap 1 (double 2.0)))
  #_ ;;TODO
  (is-tc-e (new java.util.HashMap 1 2.0))
  (is-tc-e (fn [e :- Float] (new java.util.HashMap 1 e)))
  #_ ;;TODO
  (is-tc-e (fn [e :- Double] (new java.util.HashMap 1 e)))
  #_ ;;TODO
  (is-tc-e (new java.util.HashMap 1 (ann-form 2.0 Double))))

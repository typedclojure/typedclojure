;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.base-env
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.base-env-clj-rclass :as base-rclass]
            [typed.clj.checker.ctor-override-env :as ctor-override-env]
            [typed.clj.checker.field-override-env :as field-override-env]
            [typed.clj.checker.method-override-env :as method-override-env]
            [typed.clj.checker.method-param-nilables :as method-param-nilables]
            [typed.clj.checker.method-return-nilables :as method-return-nilables]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.base-env-common :refer [delay-and-cache-env] :as common]
            [typed.cljc.checker.base-env-helper :as h]
            [typed.cljc.checker.datatype-ancestor-env :as datatype-ancestor-env]
            [typed.cljc.checker.datatype-env :as datatype-env]
            [typed.cljc.checker.declared-kind-env :as declared-kind-env]
            [typed.cljc.checker.fold-default]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.path-rep :as pe]
            [typed.cljc.checker.protocol-env :as protocol-env]
            [typed.cljc.checker.subst]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.var-env :as var-env])
  (:import (clojure.lang Named IMapEntry AMapEntry
                         LazySeq PersistentHashSet PersistentTreeSet PersistentList
                         APersistentMap ISeq IPersistentCollection
                         ILookup Indexed Associative #_ITransientSet
                         IRef Reduced)
           (java.util Comparator Collection)))

;; Dev notes
;; ---------
;;
;; To reload these type annotations *without* restarting the repl,
;; you should reload this file then run `(reset-clojure-envs!)`.
;;
;; There is some abuse of interning to get the type resolving correctly
;; in the annotations. The goal is to simulate we're inside `clojure.core.typed`.

(defn- aset-*-type [t]
  (impl/with-clojure-impl
    (let [arr-t (prs/parse-type `(~'Array ~t))
          rtn-type (prs/parse-type t)
          num-t (prs/parse-type `t/Num)]
      (apply r/make-FnIntersection
             (map r/make-Function
                  (loop [num 1
                         result []
                         dom [arr-t num-t]]
                    (if (> num 10)
                      result
                      (recur (inc num)
                             (conj result (conj dom rtn-type))
                             (conj dom num-t))))
                  (repeat rtn-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial type aliases

;(base-rclass/reset-rclass-env!)

(delay-and-cache-env ^:private init-protocol-env 
                     {}
   #_(protocol-mappings
clojure.java.io/IOFactory 
     [[]
      :methods
      {
       make-reader
       [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedReader]

       make-writer 
       [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedWriter]

       make-input-stream 
       [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedInputStream]

       make-output-stream
       [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedOutputStream]
       }]

     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

(defn ^:private count-type []
  (impl/with-clojure-impl
    (r/make-FnIntersection
      (r/make-Function
        [(prs/parse-type `(t/U (t/Seqable t/Any) clojure.lang.Counted))]
        (prs/parse-type `(t/U java.lang.Integer java.lang.Long))
        :object (obj/-path [(pe/CountPE-maker)] 0)))))

(defn ^:private nth-type []
  (impl/with-clojure-impl
    (prs/parse-type
      `(t/All [~'x ~'y]
            (t/IFn 
              [(t/U (Indexed ~'x) (t/SequentialSeqable ~'x)) t/AnyInteger :-> ~'x]
              [(t/U (Indexed ~'x) (t/SequentialSeqable ~'x) nil) t/AnyInteger ~'y :-> (t/U ~'x ~'y)]
              [(t/U (Indexed ~'x) (t/SequentialSeqable ~'x) nil) t/AnyInteger :-> (t/U ~'x nil)])))))

;; public -- used in type-ctors via requiring-resolve
(defn get-type []
  (impl/with-clojure-impl
    (prs/parse-type
      (let [x 'x
            y 'y]
        `(t/All [~x ~y]
                (t/IFn 
                  ;no default
                  [(t/Option (ILookup t/Any ~x)) t/Any :-> (t/Option ~x)]
                  [nil t/Any :-> nil]
                  [(t/Option java.util.Map) t/Any :-> (t/Option t/Any)]
                  [(t/Option (t/Set ~x)) t/Any :-> (t/Option ~x)]
                  [(t/Option String) t/Any :-> (t/Option Character)]
                  [(t/Option (~'ReadOnlyArray ~x)) t/Any :-> (t/Option ~x)]
                  ;;[(t/Option (ITransientSet ~x)) t/Any :-> (t/Option ~x)] ;;TODO transients nyi
                  [t/Any t/Any :-> t/Any]
                  ;default
                  [(t/Option (ILookup t/Any ~x)) t/Any ~y :-> (t/U ~x ~y)]
                  [nil t/Any ~y :-> ~y]
                  [(t/Option java.util.Map) t/Any t/Any :-> t/Any]
                  [(t/Option (t/Set ~x)) t/Any ~y :-> (t/U ~x ~y)]
                  [(t/Option String) t/Any ~y :-> (t/U Character ~y)]
                  [(t/Option (~'ReadOnlyArray ~x)) t/Any ~y :-> (t/U ~x ~y)]
                  ;;[(t/Option (ITransientSet ~x)) t/Any ~y :-> (t/U ~x ~y)] ;;TODO transients nyi
                  [t/Any t/Any t/Any :-> t/Any]))))))

(defn ^:private reduced?-type []
  (impl/with-clojure-impl
    (prs/parse-type
      `(t/Pred (Reduced t/Any)))))

(defn ^:private zero?-type []
  (impl/with-clojure-impl
    (prs/parse-type
      `[t/Num :-> t/Bool
        :filters {:then (~'is (t/Value 0) 0)
                  :else (~'!  (t/Value 0) 0)}])))

(defn ^:private compare-type []
  (impl/with-clojure-impl
    (prs/parse-type
      `[t/Any t/Any :-> t/Num])))

(def this-ns *ns*)

(delay-and-cache-env ^:private init-var-env
  ;(reset-alias-env!)
  (merge
   (h/var-mappings
     this-ns

clojure.core.typed/check-ns (t/IFn [t/Symbol -> t/Any]
                                [-> t/Any])
;; Internal annotations

;clojure.core.typed.current-impl/*current-impl* t/Any
clojure.core.typed.current-impl/clojure t/Any
clojure.core.typed.current-impl/clojurescript t/Any
clojure.core.typed/ann* [t/Any t/Any t/Any -> t/Any]
clojure.core.typed/untyped-var* [t/Any t/Any -> t/Any]
clojure.core.typed/declare-names* [t/Any -> t/Any]
clojure.core.typed/typed-deps* [t/Any -> t/Any]
clojure.core.typed/warn-on-unannotated-vars* [-> t/Any]
clojure.core.typed/ann-datatype* [t/Any t/Any t/Any t/Any -> t/Any]
clojure.core.typed/ann-protocol* [t/Any t/Any t/Any -> t/Any]
clojure.core.typed/ann-record* [t/Any t/Any t/Any t/Any -> t/Any]
clojure.core.typed/ann-pdatatype* [t/Any t/Any t/Any t/Any -> t/Any]
clojure.core.typed/declare-datatypes* [t/Any -> t/Any]
clojure.core.typed/declare-protocols* [t/Any -> t/Any]
clojure.core.typed/non-nil-return* [t/Any t/Any -> t/Any]
clojure.core.typed/nilable-param* [t/Any t/Any -> t/Any]
clojure.core.typed/override-constructor* [t/Any t/Any -> t/Any]
clojure.core.typed/override-method* [t/Any t/Any -> t/Any]
clojure.core.typed/typed-deps* [t/Any -> t/Any]
clojure.core.typed/load-if-needed [-> t/Any]
clojure.core.typed/*collect-on-eval* t/Any
; should always be special cased
;clojure.core.typed/var>* [t/Any -> (t/Var2 t/Nothing t/Any)]

;; core annotations

clojure.core/*ns* t/Namespace
clojure.core/pop-thread-bindings [-> t/Any]
clojure.core/load [String * -> t/Any]
clojure.core/read-string [String -> t/Any]
clojure.core/read (t/IFn [-> t/Any]
                      [java.io.Reader -> t/Any]
                      [java.io.Reader t/Bool t/Any -> t/Any]
                      [java.io.Reader t/Bool t/Any t/Bool -> t/Any])
clojure.core/read-line [-> (t/U nil String)]

clojure.core/add-classpath [(t/U String java.net.URL) -> nil]

clojure.core/*1 t/Any
clojure.core/*2 t/Any
clojure.core/*3 t/Any
clojure.core/*e (t/U nil Throwable)
clojure.core/*agent* (t/U nil (t/Agent2 t/Nothing t/Any))
clojure.core/*allow-unresolved-vars* t/Any
clojure.core/*assert* t/Any
clojure.core/*data-readers* (t/Map t/Symbol (t/Var2 t/Nothing t/Any))
clojure.core/*default-data-reader-fn* (t/U nil [t/Any t/Any -> t/Any])
clojure.core/*fn-loader* t/Any
clojure.core/*math-context* t/Any
clojure.core/*source-path* String
clojure.core/*use-context-classloader* t/Any

clojure.core/alength [(ReadOnlyArray t/Any) -> t/AnyInteger]
clojure.core/aclone (t/All [x] [(ReadOnlyArray x) -> (Array x)])
clojure.core/aget (t/All [x]
                         (t/IFn [(ReadOnlyArray x) 
                                 t/AnyInteger -> x]
                                [(ReadOnlyArray (ReadOnlyArray x)) 
                                 t/AnyInteger t/AnyInteger -> x]
                                [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x))) 
                                 t/AnyInteger t/AnyInteger t/AnyInteger -> x]
                                [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))) 
                                 t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger -> x]
                                ; don't support unsound cases
                                [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))))
                                 t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger -> x]))

clojure.core/aset
(t/All [x]
  (t/IFn
    [(Array x) t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]))

clojure.core/macroexpand-1 [t/Any -> t/Any]
clojure.core/macroexpand [t/Any -> t/Any]

clojure.core/create-struct [t/Any * -> (t/Map t/Any t/Any)]

clojure.core/find-ns [t/Symbol -> t/Namespace]
clojure.core/create-ns [t/Symbol -> t/Namespace]
clojure.core/remove-ns [t/Symbol -> t/Namespace]

clojure.core/namespace [(t/U t/Symbol t/Str t/Keyword) -> (t/Option String)]
clojure.core/ns-name [(t/U t/Symbol t/Namespace) -> t/Symbol]
clojure.core/ns-map [(t/U t/Symbol t/Namespace) -> t/Symbol]
clojure.core/ns-aliases [(t/U t/Symbol t/Namespace) -> (t/Map t/Symbol t/Namespace)]
clojure.core/name [(t/U t/Str Named) -> t/Str]
clojure.core/the-ns [(t/U t/Symbol t/Namespace) -> t/Namespace]
clojure.core/in-ns [t/Symbol -> nil]
clojure.core/import [t/Any * -> nil]
clojure.core/identity (t/All [x] [x -> x
                                  :filters {:then (! (t/U nil false) 0)
                                            :else (is (t/U nil false) 0)}
                                  :object {:id 0}])
clojure.core/gensym (t/IFn [-> t/Symbol]
                           [(t/U t/Symbol t/Str) -> t/Symbol])
clojure.core/intern (t/IFn [(t/U t/Symbol t/Namespace) t/Symbol -> (t/Var2 t/Nothing t/Any)]
                        [(t/U t/Symbol t/Namespace) t/Symbol t/Any -> (t/Var2 t/Nothing t/Any)])


clojure.core/doall (t/All [[c :< (t/U nil (t/Seqable t/Any))]]
                          (t/IFn [c -> c]
                                 [t/AnyInteger c -> c]))
clojure.core/dorun (t/IFn [(t/Seqable t/Any) -> nil]
                          [t/AnyInteger (t/Seqable t/Any) -> nil])
clojure.core/iterate (t/All [x]
                            [[x -> x] x -> (t/ASeq x)])
clojure.core/memoize (t/All [x y ...]
                            [[y ... y -> x] -> [y ... y -> x]])

clojure.core/key (t/All [x]
                        [(IMapEntry x t/Any) -> x])
clojure.core/val (t/All [x]
                        [(IMapEntry t/Any x) -> x])

;clojure.core/juxt
;(t/All [b1 ...]
;(t/All [x r b2 ...]
;     (Fn [[b1 ... b1 -> b2] ... b2 -> [b1 ... b1 -> '[b2 ... b2]]]
;         [[b1 ... b1 -> r] * -> [b1 ... b1 -> (t/Vec r)]]
;         [[x * -> b2] ... b2 -> [x * -> '[b2 ... b2]]]
;         [[x * -> r] * -> [x * -> (t/Vec r)]])))


;TODO flip filters
clojure.core/complement (t/All [x] [[x -> t/Any] -> [x -> t/Bool]])
; should preserve filters
clojure.core/boolean [t/Any -> t/Bool]

clojure.core/filter (t/All [x y]
                           (t/IFn
                             [[x -> t/Any :filters {:then (is y 0)}] (t/Seqable x) -> (t/ASeq y)]
                             [[x -> t/Any :filters {:then (! y 0)}] (t/Seqable x) -> (t/ASeq (t/I x (t/Not y)))]
                             [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)]))
clojure.core/filterv (t/All [x y]
                            (t/IFn
                              [[x -> t/Any :filters {:then (is y 0)}] (t/Seqable x) -> (t/AVec y)]
                              [[x -> t/Any] (t/Seqable x) -> (t/AVec x)]))
clojure.core/remove (t/All [x y]
                           (t/IFn
                             [[x -> t/Any :filters {:else (is y 0)}] (t/Seqable x) -> (t/ASeq y)]
                             [[x -> t/Any :filters {:else (! y 0)}] (t/Seqable x) -> (t/ASeq (t/I x (t/Not y)))]
                             [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)]
                             ))


clojure.core/take-while (t/All [x y]
                               [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)])
clojure.core/drop-while (t/All [x]
                               [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)])

clojure.core/split-with
(t/All [x y z] 
       (t/IFn
         [[x -> t/Any :filters {:then (is y 0), :else (is z 0)}] (t/Seqable x) -> '[(t/ASeq y) (t/ASeq z)]]
         [[x -> t/Any] (t/Seqable x) -> '[(t/ASeq x) (t/ASeq x)]]))

clojure.core/split-at
(t/All [x y z] 
       [t/AnyInteger (t/Seqable x) -> '[(t/ASeq x) (t/ASeq x)]])

clojure.core/partition-all (t/All [x] 
                                  (t/IFn [t/Int (t/Seqable x) -> (t/ASeq (t/ASeq x))] 
                                         [t/Int t/Int (t/Seqable x) -> (t/ASeq (t/ASeq x))]))

clojure.core/partition (All [a] (IFn [t/AnyInteger (t/Seqable a) -> (t/ASeq (t/ASeq a))]
                                     [t/AnyInteger t/AnyInteger (t/Seqable a) -> (t/ASeq (t/ASeq a))]
                                     [t/AnyInteger t/AnyInteger t/AnyInteger (t/Seqable a) -> (t/ASeq (t/ASeq a))]))

clojure.core/repeatedly
(t/All [x]
       (t/IFn [[-> x] -> (t/ASeq x)]
              [t/AnyInteger [-> x] -> (t/ASeq x)]))


clojure.core/some (t/All [x y] [[x -> y] (t/Seqable x) -> (t/Option y)])

; Unions need to support dots for this to work:
;
; (t/All [t0 b ...]
;    (t/IFn [[t/Any -> t/Any :filters {:then (is t0 0) :else (! t0 0)}] 
;         [t/Any -> t/Any :filters {:then (is b 0) :else (! b 0)}] ... b
;         -> (t/IFn [t/Any -> t/Any :filters {:then (is (t/U t0 b ... b) 0) :else (! (t/U t0 b ... b) 0)}]
;                [t/Any * -> t/Any])]))
clojure.core/some-fn 
  (t/All [t0 t1 t2 t3 t4 t5]
    (t/IFn [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
         -> (t/IFn [t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}]
                [t/Any * -> t/Any])]
        [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
         [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
         -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1) 0) :else (! (t/U t0 t1) 0)}]
                [t/Any * -> t/Any])]
        [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
         [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
         [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
         -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2) 0) :else (! (t/U t0 t1 t2) 0)}]
                [t/Any * -> t/Any])]
        [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
         [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
         [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
         [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
         -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2 t3) 0) :else (! (t/U t0 t1 t2 t3) 0)}]
                [t/Any * -> t/Any])]
        [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
         [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
         [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
         [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
         [t/Any -> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
         -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2 t3 t4) 0) :else (! (t/U t0 t1 t2 t3 t4) 0)}]
                [t/Any * -> t/Any])]
        [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
         [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
         [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
         [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
         [t/Any -> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
         [t/Any -> t/Bool :filters {:then (is t5 0) :else (! t5 0)}]
         -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2 t3 t4 t5) 0) :else (! (t/U t0 t1 t2 t3 t4 t5) 0)}]
                [t/Any * -> t/Any])]
        [[t/Any -> t/Any] [t/Any -> t/Any] * -> [t/Any * -> t/Any]]))
clojure.core/every-pred
(t/All [t0 t1 t2 t3 t4 t5]
       (t/IFn [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1) 0) :else (! (t/I t0 t1) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2) 0) :else (! (t/I t0 t1 t2) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2 t3) 0) :else (! (t/I t0 t1 t2 t3) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any -> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2 t3 t4) 0) :else (! (t/I t0 t1 t2 t3 t4) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Any :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Any :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Any :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Any :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any -> t/Any :filters {:then (is t4 0) :else (! t4 0)}]
               [t/Any -> t/Any :filters {:then (is t5 0) :else (! t5 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2 t3 t4 t5) 0) :else (! (t/I t0 t1 t2 t3 t4 t5) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Any] [t/Any -> t/Any] * -> [t/Any * -> t/Any]]))

clojure.core/concat (t/All [x] [(t/Seqable x) * -> (t/ASeq x)])

clojure.core/set (t/All [x] [(t/Seqable x) -> (PersistentHashSet x)])
clojure.core/hash-set (t/All [x] [x * -> (PersistentHashSet x)])
clojure.core/hash-map (t/All [x y] [(t/HSequential [x y] :repeat true) <* -> (t/Map x y)])
clojure.core/sorted-map (t/All [x y] [(t/HSequential [x y] :repeat true) <* -> (t/Map x y)])
clojure.core/sorted-set (t/All [x] [x * -> (PersistentTreeSet x)])
clojure.core/sorted-set-by (t/All [x] [[x x -> t/AnyInteger] x * -> (PersistentTreeSet x)])
clojure.core/list (t/All [x] [x * -> (PersistentList x)])
clojure.core/list* (t/All [x] 
                          (t/IFn [(t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                                 [x x x x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]))

clojure.core/list? (t/Pred (t/List t/Any))

clojure.core/load-reader [java.io.Reader -> t/Any]

clojure.core/methods [t/Multi -> (t/Map t/Any t/Any)]

clojure.core/munge (t/IFn [t/Symbol -> t/Symbol]
                       [t/Any -> t/Any])

clojure.core/pos? [t/Num -> t/Bool]
clojure.core/neg? [t/Num -> t/Bool]

clojure.core/nthrest (t/All [x] [(t/Seqable x) t/AnyInteger 
                               -> (t/ASeq x)])

clojure.core/vector (t/All [r b ...]
                           (t/IFn [b ... b -> '[b ... b]]
                                  [r * -> (t/AVec r)]))
clojure.core/vec (t/All [x] [(t/Seqable x) -> (t/AVec x)])

clojure.core/not [t/Any -> t/Bool]
clojure.core/constantly (t/All [x] [x -> [t/Any * -> x]])

clojure.core/bound? [(t/Var2 t/Nothing t/Any) * -> t/Bool]
clojure.core/thread-bound? [(t/Var2 t/Nothing t/Any) * -> t/Bool]
clojure.core/bases [(t/Nilable Class) -> (t/NilableNonEmptyASeq Class)]

clojure.core/make-hierarchy [-> t/Hierarchy]
clojure.core/isa? (t/IFn [t/Any t/Any -> t/Bool]
                         [t/Hierarchy t/Any t/Any -> t/Bool])

clojure.core/disj
(t/All [x]
       (t/IFn [(t/SortedSet x) t/Any t/Any * -> (t/SortedSet x)]
              [(t/Set x) t/Any t/Any * -> (t/Set x)]))

clojure.core/assoc
     (t/All [m k v c ...]
          (t/IFn [m k v (t/HSeq [c c] :repeat true) <... c
                -> (t/Assoc m k v c ... c)]
;               [m k v (t/HSeq [k v] :repeat true) <*
;                -> (t/Assoc m k v)]
               [nil k v (t/HSeq [c c] :repeat true) <... c
                -> (t/Assoc nil k v c ... c)]
               [nil k v (t/HSeq [k v] :repeat true) <*
                -> (t/Map k v)]))
;     (t/All [b c d]
;       (Fn [(t/Map b c) b c -> (t/Map b c)]
;           [(t/Vec d) t/AnyInteger d -> (t/Vec d)]
;           [d b c (t/HSequential [b c] :repeat true) <* -> (t/Assoc d b c)]))

clojure.core/dissoc
(t/All [k v]
       [(t/Map k v) t/Any * -> (t/Map k v)])
)
    (h/var-mappings
      this-ns

clojure.core/zipmap
(t/All [k v]
       [(t/Seqable k) (t/Seqable v) -> (APersistentMap k v)])

clojure.core/keys
(t/All [k]
       [(t/Map k t/Any) -> (t/ASeq k) :object {:id 0 :path [Keys]}])

clojure.core/vals
(t/All [v]
       [(t/Map t/Any v) -> (t/ASeq v) :object {:id 0 :path [Vals]}])

;most useful case
clojure.core/comp
(t/All [x y b ...]
       [[x -> y] [b ... b -> x] -> [b ... b -> y]])


;apply: wishful thinking
;     (t/All [b1 ...]
;     (t/All [y b2 ...]
;          (t/IFn [[b1 ... b1 b2 ... b2 -> y] b1 ... b1 (t/HSequential [b2 ... b2]) -> y]
;              [[b1 ... b1 r * -> y] b1 ... b1 (t/Seqable r) -> y])))

clojure.core/apply
     (t/All [y a b c d r z ...]
          (t/IFn [[z ... z -> y] (t/U nil (t/HSequential [z ... z])) -> y]
              [[a z ... z -> y] a (t/U nil (t/HSequential [z ... z])) -> y]
              [[a b z ... z -> y] a b (t/U nil (t/HSequential [z ... z])) -> y]
              [[a b c z ... z -> y] a b c (t/U nil (t/HSequential [z ... z])) -> y]
              [[a b c d z ... z -> y] a b c d (t/U nil (t/HSequential [z ... z])) -> y]
              [[r * -> y] (t/Seqable r) -> y]
              [[a r * -> y] a (t/Seqable r) -> y]
              [[a b r * -> y] a b (t/Seqable r) -> y]
              [[a b c r * -> y] a b c (t/Seqable r) -> y]
              [[a b c d r * -> y] a b c d (t/Seqable r) -> y]
              ))

;partial: wishful thinking (replaces the first 4 arities)
; (t/All [b1 ...]
; (t/All [r b2 ...]
;    [[b1 ... b1 b2 ... b2 -> r] b1 ... b1 -> [b2 ... b2 -> r]]))

clojure.core/partial
(t/All [y a b c d z ...]
       (t/IFn [[z ... z -> y] -> [z ... z -> y]]
              [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a * -> y] a * -> [a * -> y]]))

clojure.core/str [t/Any * -> String]
clojure.core/prn-str [t/Any * -> String]
clojure.core/pr-str [t/Any * -> String]
clojure.core/newline [-> nil]

clojure.core/print [t/Any * -> nil]
clojure.core/println [t/Any * -> nil]
clojure.core/print-str [t/Any * -> String]
clojure.core/println-str [t/Any * -> String]
clojure.core/printf [String t/Any * -> nil]
clojure.core/format [String t/Any  * -> String]
clojure.core/pr [t/Any * -> nil]
clojure.core/prn [t/Any * -> nil]
clojure.core/flush [-> nil]
clojure.core/*print-length* (t/U nil false t/AnyInteger)
clojure.core/*print-level* (t/U nil false t/AnyInteger)
clojure.core/*verbose-defrecords* t/Bool
clojure.core/print-ctor [Object [Object java.io.Writer -> t/Any] java.io.Writer -> nil]

clojure.core/prefer-method [t/Multi t/Any t/Any -> t/Any]
clojure.core/print-simple [t/Any java.io.Writer -> nil]
clojure.core/char-escape-string (t/Map Character String)
clojure.core/char-name-string (t/Map Character String)
clojure.core/primitives-classnames (t/Map Class String)

clojure.core/namespace-munge [(t/U t/Symbol t/Namespace) -> String]

;clojure.core/find-protocol-impl ['{:on-interface Class
;                                   :impls ?}]


clojure.core/re-matcher [java.util.regex.Pattern String -> java.util.regex.Matcher]
clojure.core/re-groups [java.util.regex.Matcher -> (t/U nil String (t/Vec (t/Option String)))]
clojure.core/re-find (t/IFn [java.util.regex.Matcher -> (t/U nil String (t/Vec (t/Option String)))]
                              [java.util.regex.Pattern String -> (t/U nil String (t/Vec (t/Option String)))])
clojure.core/re-seq [java.util.regex.Pattern String -> (t/ASeq (t/U nil String (t/Vec (t/Option String))))]

clojure.core/subs (t/IFn [t/Str t/AnyInteger -> t/Str]
                         [t/Str t/AnyInteger t/AnyInteger -> t/Str])

;TODO
;clojure.core/spit [java.io.Writer t/Any]

clojure.core/future-call (t/All [x] [[-> x] -> (t/Future x)])

clojure.core/atom (t/All [x]
                         [x & :optional {:validator (t/Nilable [x -> t/Any]) :meta t/Any} -> (t/Atom2 x x)])

clojure.core/set-validator! (t/All [x]
                                 [(clojure.lang.IRef t/Any x) [x -> t/Any] -> nil])

clojure.core/deref (t/All [x y]
                          (t/IFn 
                            [(t/Deref x) -> x]
                            [(t/U (t/Deref t/Any) java.util.concurrent.Future) -> t/Any]
                            [(t/BlockingDeref x) t/AnyInteger y -> (t/U x y)]
                            [(t/U java.util.concurrent.Future (t/BlockingDeref t/Any)) t/AnyInteger t/Any -> t/Any]))

clojure.core/delay? (t/Pred (t/Delay t/Any))

clojure.core/future-cancelled? [java.util.concurrent.Future -> t/Bool]

clojure.core/future-cancel [java.util.concurrent.Future -> t/Any]

clojure.core/future? (t/Pred java.util.concurrent.Future)

clojure.core/future-done? [java.util.concurrent.Future -> t/Bool]

clojure.core/force (t/All [x]
                          (t/IFn [(t/Delay x) -> x]
                                 [t/Any -> t/Any]))

clojure.core/realized? [clojure.lang.IPending -> t/Bool]

clojure.core/select-keys (t/All [k v] [(t/Map k v) (t/Seqable t/Any) -> (t/Map k v)])

clojure.core/sort (t/All [x] 
                         (t/IFn [(t/Seqable x) -> (t/ASeq x)]
                                [(t/I Comparator [x x -> t/AnyInteger]) 
                                 (t/Seqable x) -> (t/ASeq x)]))
clojure.core/sort-by (t/All [a] (t/IFn [(t/Seqable a) -> (t/ASeq a)]
                                       [[a -> Number] (t/Seqable a) -> (ASeq a)]))

clojure.core/replicate (t/All [a] [t/AnyInteger a -> (t/ASeq a)])

clojure.core/reset! (t/All [w r]
                           [(t/Atom2 w r) w -> w])

clojure.core/swap! (t/All [w r b ...] 
                          [(t/Atom2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/compare-and-set!
(t/All [w]
       [(t/Atom2 w t/Any) t/Any w -> t/Bool])

clojure.core/set-validator!
(t/All [w]
       [(clojure.lang.IRef w t/Any) (t/Nilable [w -> t/Any]) -> t/Any])

clojure.core/get-validator
(t/All [w]
       [(clojure.lang.IRef w t/Any) -> (t/Nilable [w -> t/Any])])

clojure.core/alter-var-root (t/All [w r b ...] 
                              [(t/Var2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/method-sig [java.lang.reflect.Method -> '[t/Any (t/NilableNonEmptySeqable t/Any) t/Any]]
clojure.core/proxy-name [Class (t/Seqable Class) -> String]
clojure.core/get-proxy-class [Class * -> Class]
clojure.core/construct-proxy [Class t/Any * -> t/Any]
clojure.core/init-proxy [t/Proxy (t/Map String t/Any) -> t/Proxy]
clojure.core/update-proxy [t/Proxy (t/Map String t/Any) -> t/Proxy]
clojure.core/proxy-mappings [t/Proxy -> (t/Map String t/Any)]
clojure.core/proxy-call-with-super (t/All [x] [[-> x] t/Proxy String -> x])
clojure.core/bean [Object -> (t/Map t/Any t/Any)]

clojure.core/fnil
(t/All [x y z a b ...] (t/IFn [[x b ... b -> a] x -> [(t/Nilable x) b ... b -> a]]
                              [[x y b ... b -> a] x y -> [(t/Nilable x) (t/Nilable y) b ... b -> a]]
                              [[x y z b ... b -> a] x y z -> [(t/Nilable x) (t/Nilable y) (t/Nilable z) b ... b -> a]]))

clojure.core/symbol
(t/IFn [(t/U t/Symbol t/Str) -> t/Symbol]
       [(t/U nil t/Str) t/Str -> t/Symbol])

clojure.core/keyword
(t/IFn [(t/U t/Keyword t/Symbol t/Str) -> t/Keyword 
        :object {:id 0 :path [Keyword]}
        :filters {:then tt
                  :else ff}]
       [nil -> nil 
        :object {:id 0 :path [Keyword]}
        :filters {:then ff
                  :else tt}]
       [t/Any -> (t/U nil t/Keyword) 
        :object {:id 0 :path [Keyword]}
        :filters {:then (is (t/U t/Keyword t/Symbol t/Str) 0)
                  :else (! (t/U t/Keyword t/Symbol t/Str) 0)}]
       [t/Str t/Str -> t/Keyword
        :filters {:then tt
                  :else ff}])

clojure.core/find-keyword
(t/IFn [(t/U t/Keyword t/Symbol t/Str) -> (t/Option t/Keyword)]
       [t/Str t/Str -> (t/Option t/Keyword)])

clojure.core/derive (t/IFn [(t/U t/Symbol t/Keyword Class) (t/U t/Symbol t/Keyword) -> nil]
                           [t/Hierarchy (t/U t/Symbol t/Keyword Class) (t/U t/Symbol t/Keyword) -> t/Hierarchy])

clojure.core/compare [t/Any t/Any -> t/Num]

clojure.core/require [t/Any * -> nil]
clojure.core/use [t/Any * -> nil]
clojure.core/refer [t/Symbol & :optional {:exclude (t/Seqable t/Symbol)
                                        :only (t/Seqable t/Symbol)
                                        :rename (t/Map t/Symbol t/Symbol)}
                    -> nil]

clojure.core/*loaded-libs* (t/Ref1 (t/Set t/Symbol))

clojure.core/seq? (t/Pred (t/Seq t/Any))
clojure.core/set? (t/Pred (t/Set t/Any))
clojure.core/vector? (t/Pred (t/Vec t/Any))
clojure.core/nil? (t/Pred nil)
clojure.core/false? (t/Pred false)
clojure.core/true? (t/Pred true)
clojure.core/symbol? (t/Pred t/Symbol)
clojure.core/keyword? (t/Pred t/Keyword)
clojure.core/map? (t/Pred (t/Map t/Any t/Any))
clojure.core/boolean? (t/Pred t/Bool)

; would be nice
; (t/Pred (t/Not nil))
clojure.core/some? [t/Any -> t/Bool :filters {:then (! nil 0)
                                             :else (is nil 0)}]
)
    (h/var-mappings
      this-ns

clojure.core/cast (t/All [x] [Class x -> x])

clojure.core/associative? (t/Pred (clojure.lang.Associative t/Any t/Any t/Any))
clojure.core/coll? (t/Pred (t/Coll t/Any))
      ;TODO should these be parameterized?
clojure.core/sequential? (t/Pred t/Sequential)
;clojure.core/sorted? (t/Pred Sorted)
clojure.core/meta [t/Any -> (t/Nilable (t/Map t/Any t/Any))]
clojure.core/with-meta (t/All [[x :< clojure.lang.IObj]]
                              [x (t/Nilable (t/Map t/Any t/Any)) -> x])
clojure.core/vary-meta (t/All [[x :< clojure.lang.IObj] b ...]
                              [x [(t/Nilable (t/Map t/Any t/Any)) b ... b -> (t/Nilable (t/Map t/Any t/Any))] b ... b -> x])

clojure.core/reset-meta! [clojure.lang.IReference (t/Nilable (t/Map t/Any t/Any)) -> (t/Nilable (t/Map t/Any t/Any))]
clojure.core/alter-meta! 
(t/All [b ...]
       [clojure.lang.IReference
        [(t/Nilable (t/Map t/Any t/Any)) b ... b ->
         (t/Nilable (t/Map t/Any t/Any))] b ... b -> (t/Nilable (t/Map t/Any t/Any))])

clojure.core/commute
      (t/All [w r b ...] 
           [(t/Ref2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/alter
      (t/All [w r b ...] 
           [(t/Ref2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/cycle
(t/All [x]
       [(t/Seqable x) -> (t/ASeq x)])

clojure.core/compile [t/Symbol -> t/Symbol]

clojure.core/comparator
      (t/All [x y]
           [[x y -> t/Any] -> (t/I Comparator [x y -> t/AnyInteger])])

clojure.core/destructure [t/Any -> t/Any]

clojure.core/distinct (t/All [x] [(t/Seqable x) -> (t/ASeq x)])

clojure.core/string? (t/Pred t/Str)
clojure.core/char? (t/Pred Character)

clojure.string/split
     (t/IFn [String java.util.regex.Pattern -> (t/AVec String)]
         [String java.util.regex.Pattern t/AnyInteger -> (t/AVec String)])

clojure.string/join
     (t/IFn [(t/Seqable t/Any) -> String]
            [t/Any (t/Seqable t/Any) -> String])

clojure.string/upper-case
      [CharSequence -> String]

clojure.string/blank? [(t/U nil String) -> t/Bool]
clojure.string/capitalize [String -> String]
clojure.string/lower-case [String -> String]
clojure.string/replace (t/IFn [String String String -> String]  [String Character Character -> String]  [String java.util.regex.Pattern (t/U String [String -> String]) -> String] )
clojure.string/replace-first (t/IFn [String String String -> String]  [String Character Character -> String]  [String java.util.regex.Pattern (t/U String [String -> String]) -> String] )
clojure.string/reverse [String -> String]
clojure.string/trim [String -> String]
clojure.string/trimr [String -> String]
clojure.string/triml [String -> String]

 clojure.data/diff [t/Any t/Any -> '[t/Any t/Any t/Any]]
 clojure.instant/read-instant-date [String -> java.util.Date]
 clojure.instant/read-instant-calendar [String -> java.util.GregorianCalendar]
 clojure.instant/read-instant-timestamp [String -> java.sql.Timestamp]
 clojure.repl/apropos [(t/U String java.util.regex.Pattern) -> (t/Seq t/Symbol)]
 clojure.repl/demunge [String -> String]
 clojure.repl/source-fn [t/Symbol -> (t/U String nil)]
 clojure.template/apply-template [(t/Vec t/Any) t/Any (t/Seqable t/Any) -> t/Any]
 clojure.set/difference (t/All [x] [(t/Set x) (t/Set t/Any) * -> (t/Set x)])
 clojure.set/subset? [(t/Set t/Any) (t/Set t/Any) -> t/Bool]
 clojure.set/superset? [(t/Set t/Any) (t/Set t/Any) -> t/Bool]
 clojure.set/join (t/IFn [(t/Set (t/Map t/Any t/Any)) (t/Set (t/Map t/Any t/Any)) -> (t/Set (t/Map t/Any t/Any))]
                       [(t/Set (t/Map t/Any t/Any)) (t/Set (t/Map t/Any t/Any)) (t/Map t/Any t/Any) -> (t/Set (t/Map t/Any t/Any))])

 ; would be nice
;(t/All [[m :> (t/Map t/Any t/Any)] k]
;     [(t/Set m) (t/Seqable k) -> (t/Map (t/Map k (Get m k)) (t/Set m))]
;     )
 clojure.set/index (t/All [x y]
                    [(t/Set (t/Map x y)) (t/Seqable t/Any) -> (t/Map (t/Map t/Any t/Any) (t/Set (t/Map x y)))]
                    )
 clojure.set/map-invert (t/All [a b] [(t/Map a b) -> (t/Map b a)])

 ;would be nice, not quite correct though
; (t/All [x y [m :< (t/Map x y)] k]
;    [(t/Set m) (t/Vec k) -> (t/Set (t/Map k (Get m k)))])
 clojure.set/project (t/All [x y]
                      [(t/Set (t/Map x y)) (t/Vec t/Any) -> (t/Set (t/Map x y))])
 clojure.set/rename (t/All [x y]
                      [(t/Set (t/Map x y)) (t/Map t/Any x) -> (t/Set (t/Map x y))])
 clojure.set/rename-keys (t/All [x y]
                          [(t/Map x y) (t/Map t/Any x) -> (t/Map x y)])
 ;like filter
clojure.set/select (t/All [x y]
                           (t/IFn
                             [[x -> t/Any :filters {:then (is y 0)}] (t/Set x) -> (t/Set y)]
                             [[x -> t/Any :filters {:then (! y 0)}] (t/Set x) -> (t/Set (t/I x (t/Not y)))]
                             [[x -> t/Any] (t/Set x) -> (t/Set x)]))
 
; FIXME should be [String [t/Any -> t/Any] -> String]
clojure.string/escape [String (t/U (t/Map t/Any t/Any) [t/Any -> t/Any]) -> String]
clojure.string/split-lines [String -> (t/Vec String)]

clojure.test/function? [t/Any -> t/Bool]
clojure.test/assert-any [t/Any t/Any -> t/Any]
clojure.test/do-report [t/Any -> t/Any]
clojure.test/run-tests [t/Symbol * -> (t/Map t/Any t/Any)]
clojure.test/run-all-tests (t/IFn [-> (t/Map t/Any t/Any)]
                                [java.util.regex.Pattern * -> (t/Map t/Any t/Any)])
clojure.test/successful? [(t/U nil (t/Map t/Any t/Any)) -> t/Bool]
clojure.test/compose-fixtures [[[-> t/Any] -> t/Any] [[-> t/Any] -> t/Any] -> [[-> t/Any] -> t/Any]]
clojure.test/testing-vars-str [(t/Map t/Any t/Any) -> String]
clojure.test/testing-contexts-str [-> String]
clojure.test/test-ns [(t/U t/Namespace t/Symbol) -> (t/Map t/Any t/Any)]

clojure.test.tap/print-tap-plan [t/Any -> t/Any]
clojure.test.tap/print-tap-diagnostic [String -> t/Any]
clojure.test.tap/print-tap-pass [t/Any -> t/Any]
clojure.test.tap/print-tap-fail [t/Any -> t/Any]

clojure.java.javadoc/add-local-javadoc [t/Any -> (t/List t/Any)]
clojure.java.javadoc/add-remote-javadoc [String t/Any -> (t/Map t/Any t/Any)]
clojure.java.javadoc/javadoc [t/Any -> t/Any]

clojure.edn/read-string [(t/U String nil) -> t/Any]

clojure.java.shell/sh [t/Any *
                       ;would be nice (combine * and kw args)
                       ; String *
                       ;& :optional {:in t/Any  ;; any valid input to clojure.java.io/copy
                       ;             :inc-enc String :out-env (t/U ':bytes String)
                       ;             :env (t/U (Array String) (t/Map t/Any t/Any))
                       ;             :dir (t/U String java.io.File)}
                       -> '{:exit String
                            :out (t/U (Array byte) String)
                            :err String}]

clojure.java.browse/browse-url [t/Any -> t/Any]

clojure.java.io/delete-file (t/IFn [t/Any
                                  ;; FIXME any arg that c.j.io/file accepts
                                  #_String 
                                  -> t/Any]
                                 [t/Any t/Any -> t/Any])

clojure.stacktrace/e [-> t/Any]
clojure.stacktrace/print-cause-trace [Throwable -> t/Any]
clojure.stacktrace/print-stack-trace [Throwable -> t/Any]
clojure.stacktrace/print-throwable [Throwable -> t/Any]
clojure.stacktrace/root-cause [Throwable -> Throwable]

;; FIXME keyword arguments
clojure.reflect/reflect [t/Any t/Any * -> (t/Map t/Any t/Any)]

clojure.inspector/atom? [t/Any -> t/Bool]
clojure.inspector/collection-tag [t/Any -> t/Keyword]
clojure.inspector/tree-model [t/Any -> t/Any]
clojure.inspector/old-table-model [(t/Seqable t/Any) -> t/Any]
clojure.inspector/inspect [t/Any -> javax.swing.JFrame]
clojure.inspector/inspect-tree [t/Any -> javax.swing.JFrame]
clojure.inspector/inspect-table [(t/Seqable t/Any) -> javax.swing.JFrame]

clojure.pprint/cl-format [(t/U java.io.Writer nil t/Bool) String t/Any * -> (t/U nil String)]
clojure.pprint/fresh-line [-> t/Any]
clojure.pprint/get-pretty-writer [java.io.Writer -> java.io.Writer]

clojure.main/demunge [String -> String]
clojure.main/repl-prompt [-> t/Any]
clojure.main/repl-read [t/Any t/Any -> t/Any]
clojure.main/repl-caught [Throwable -> t/Any]
clojure.main/repl-exception [Throwable -> t/Any]
clojure.main/root-cause [Throwable -> Exception]
clojure.main/repl [& :optional {:init [-> t/Any]
                                :need-prompt [-> t/Any]
                                :prompt [-> t/Any]
                                :flush [-> t/Any]
                                :read [t/Any t/Any -> t/Any]
                                :eval [t/Any -> t/Any]
                                :print [t/Any -> t/Any]
                                :caught [Throwable -> t/Any]}
                   -> t/Any]
clojure.main/main [t/Any * -> t/Any]
clojure.main/load-script [String -> t/Any]

clojure.walk/keywordize-keys [t/Any -> t/Any]
clojure.walk/macroexpand-all [t/Any -> t/Any]
clojure.walk/postwalk [[t/Any -> t/Any] t/Any -> t/Any]
clojure.walk/postwalk-demo [t/Any -> t/Any]
clojure.walk/postwalk-replace [(t/Map t/Any t/Any) t/Any -> t/Any]
clojure.walk/prewalk [[t/Any -> t/Any] t/Any -> t/Any]
clojure.walk/prewalk-demo [t/Any -> t/Any]
clojure.walk/prewalk-replace [(t/Map t/Any t/Any) t/Any -> t/Any]
clojure.walk/stringify-keys [t/Any -> t/Any]
clojure.walk/walk [[t/Any -> t/Any] [t/Any -> t/Any] t/Any -> t/Any]

clojure.zip/zipper [[t/Any -> t/Any] [(t/Seqable t/Any) -> (t/U nil (t/Seq t/Any))] 
                    [t/Any (t/U nil (t/Seq t/Any)) -> t/Any]
                    t/Any 
                    -> (t/Vec t/Any)]
clojure.zip/seq-zip [t/Any -> (t/Vec t/Any)]
clojure.zip/vector-zip [t/Any -> (t/Vec t/Any)]
clojure.zip/xml-zip [t/Any -> (t/Vec t/Any)]
clojure.zip/node [(t/Vec t/Any) -> t/Any]
clojure.zip/branch? [(t/Vec t/Any) -> t/Bool]
clojure.zip/children [(t/Vec t/Any) -> (t/U nil (t/Seq t/Any))]
clojure.zip/root [(t/Vec t/Any) -> t/Any]
clojure.zip/rightmost [(t/Vec t/Any) -> (t/Vec t/Any)]
clojure.zip/right [(t/Vec t/Any) -> t/Any]
clojure.zip/up [(t/Vec t/Any) -> (t/U nil (t/Vec t/Any))]
clojure.zip/rights [(t/Vec t/Any) -> t/Any]
clojure.zip/replace [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/down [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/left [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/lefts [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/leftmost [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/append-child [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/branch? [(t/Vec t/Any) -> t/Bool]
clojure.zip/end? [(t/Vec t/Any) -> t/Bool]
clojure.zip/insert-child [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/insert-left [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/insert-right [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/next [(t/Vec t/Any) -> (t/Vec t/Any)]
clojure.zip/prev [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]

;; more to say here
clojure.zip/path [(t/Vec t/Any) -> t/Any]

clojure.zip/remove [(t/Vec t/Any) -> (t/Vec t/Any)]

clojure.core/interpose (t/All [x] [x (t/Seqable x) -> (t/ASeq x)])
clojure.core/interleave (t/All [x] [(t/Seqable x) (t/Seqable x) (t/Seqable x) * -> (t/ASeq x)])

clojure.core/repeat (t/All [x] 
                           (t/IFn [x -> (t/ASeq x)]
                                  [t/AnyInteger x -> (t/ASeq x)]))

;clojure.core/every? (t/All [x y] 
;                         (t/IFn [[x -> t/Any :filters {:then (is y 0)}] (t/Coll x) -> t/Bool
;                              :filters {:then (is (t/Coll (t/I x y)) 1)}]
;                             ; argument could be nil
;                             [[x -> t/Any :filters {:then (is y 0)}] (t/U nil (t/Coll x)) -> t/Bool
;                              :filters {:then (is (t/U nil (t/Coll (t/I x y))) 1)}]
;                             [[x -> t/Any] (t/Seqable x) -> t/Bool]))
clojure.core/every? (t/All [x y]
                           (t/IFn [[x -> t/Any :filters {:then (is y 0)}] (t/Coll x) -> t/Bool
                                   :filters {:then (is (t/Coll y) 1)}]
                                  ; argument could be nil
                                  [[x -> t/Any :filters {:then (is y 0)}] (t/U nil (t/Coll x)) -> t/Bool
                                   :filters {:then (is (t/U nil (t/Coll y)) 1)}]
                                  [[x -> t/Any] (t/Seqable x) -> t/Bool]))

clojure.core/range
(t/IFn [-> (t/ASeq t/AnyInteger)]
       [t/Num -> (t/ASeq t/AnyInteger)]
       [t/AnyInteger t/Num -> (t/ASeq t/AnyInteger)]
       [t/Num t/Num -> (t/ASeq t/Num)]
       [t/AnyInteger t/Num t/AnyInteger -> (t/ASeq t/AnyInteger)]
       [t/Num t/Num t/Num -> (t/ASeq t/Num)])

clojure.core/class (t/IFn [nil -> nil :object {:id 0 :path [Class]}]
                            [Object -> Class :object {:id 0 :path [Class]}]
                            [t/Any -> (t/Option Class) :object {:id 0 :path [Class]}])

; need better metadata support if this even has a chance of working
; like class
clojure.core/type [t/Any -> t/Any]

clojure.core/seq (t/All [x]
                        (t/IFn 
                          [(t/NonEmptyColl x) -> (t/NonEmptyASeq x)
                           :filters {:then tt
                                     :else ff}]
                          [(t/Option (t/Coll x)) -> (t/NilableNonEmptyASeq x)
                           :filters {:then (& (is t/NonEmptyCount 0)
                                              (! nil 0))
                                     :else (| (is nil 0)
                                              (is t/EmptyCount 0))}]
                          [(t/Seqable x) -> (t/NilableNonEmptyASeq x)]))

; t/Seqable [[x :variance :covariant]
;          :count [l :variance :covariant :< AnyCountRange]
;          :to-seq [sfn :kind (t/TFn [[x :variance :covariant]]
;                               (t/I IWithMeta (IMeta nil) (ISeq x) (ICollection x) 
;                                  IEmptyableCollection ISequential))]]

; clojure.core/seq (t/All [x
;                        [sfn :kind [* -> *]]
;                    (t/IFn
;                      [(t/Seqable x :count (t/CountRange 1) :to-seq sfn) -> (sfn x)]
;                      [(t/Seqable x :count AnyCountRange :to-seq sfn) -> (t/U nil (sfn x))]))

clojure.core/empty? (t/IFn [(t/Option (t/HSequential [t/Any *])) -> t/Bool
                            :filters {:then (| (is t/EmptyCount 0)
                                               (is nil 0))
                                      :else (is t/NonEmptyCount 0)}]
                           [(t/Option (t/Coll t/Any)) -> t/Bool
                            :filters {:then (| (is t/EmptyCount 0)
                                               (is nil 0))
                                      :else (is t/NonEmptyCount 0)}]
                           [(t/Seqable t/Any) -> t/Bool])

clojure.core/map
(t/All [c a b ...]
       (t/IFn [[a :-> c] :-> (t/Transducer a c)]
              [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
              [[a b ... b -> c] (t/Seqable a) (t/Seqable b) ... b -> (t/ASeq c)]))

clojure.core/mapv
(t/All [c a b ...]
       (t/IFn [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyAVec c)]
              [[a b ... b -> c] (t/Seqable a) (t/Seqable b) ... b -> (t/AVec c)]))

clojure.core/mapcat
(t/All [c a b ...]
       (t/IFn
         [[a :-> (t/Seqable c)] :-> (t/Transducer a c)]
         [[a b ... b -> (t/Seqable c)] (t/Seqable a) (t/Seqable b) ... b -> (t/ASeq c)]))

clojure.core/pmap
(t/All [c a b ...]
       (t/IFn [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
              [[a b ... b -> c] (t/Seqable a) (t/Seqable b) ... b -> (t/ASeq c)]))

clojure.core/pcalls
      (t/All [r]
           [[-> r] * -> (t/ASeq r)])

#_#_
clojure.core/halt-when
(t/All [a d]
  [[a :-> t/Any] :-> (t/Transducer a a)]
  [[a :-> t/Any] (t/U nil [t/Any a :-> a]) :-> (t/Transducer a a)])

clojure.core/frequencies (t/All [a] [(t/Seqable a) -> (t/Map a t/AnyInteger)])

clojure.core/*clojure-version* '{:major t/Any
                                 :minor t/Any
                                 :incremental t/Any
                                 :qualifier t/Any}

clojure.core/clojure-version [-> String]

clojure.core/promise
        (t/All [x]
           [-> (t/Promise x)])

clojure.core/deliver (t/All [x] [(t/Promise x) x -> (t/U nil (t/Promise x))])

clojure.core/flatten [(t/Seqable t/Any) -> (t/Seq t/Any)]

clojure.core/map-indexed
(t/All [x y] [[t/AnyInteger x -> y] (t/Seqable x) -> (t/ASeq y)])

clojure.core/keep-indexed
     (t/All [a c] [[t/Num a -> (t/U nil c)] (t/Seqable a) -> (t/Seq c)])

clojure.core/keep (t/All [a b] [[a -> (t/Option b)] (t/Coll a) -> (t/Option (t/ASeq b))])

clojure.core/seqable? (t/Pred (t/Seqable Any))

clojure.core/merge-with
(t/All [k v]
       (t/IFn [[v v -> v] nil * -> nil]
              [[v v -> v] (t/Map k v) * -> (t/Map k v)]
              [[v v -> v] (t/Option (t/Map k v)) * -> (t/Option (t/Map k v))]))

clojure.core/reduce
     (t/All [a c]
          (t/IFn 
            ;Without accumulator
            ; default
            ; (reduce + my-coll)
            [[a c -> (t/U (Reduced a) a)] (t/NonEmptySeqable c) -> a]
            [(t/IFn [a c -> (t/U (Reduced a) a)] [-> (t/U (Reduced a) a)]) (t/Seqable c) -> a]
            ; default
            ; (reduce + 3 my-coll)
            ; (reduce (fn [a b] a) (reduced 1) nil) 
            ; ;=> (reduced 1)
            [[a c -> (t/U (Reduced a) a)] a (t/Seqable c) -> a]))

clojure.core/reduce-kv
(t/All [a c k v]
       [[a k v -> (t/U (Reduced a) a)] a (t/Option (Associative t/Any k v)) -> a])

clojure.core/reductions
(t/All [a b] (t/IFn [[a b -> a] (t/Seqable b) -> (t/ASeq a)]
                    [[a b -> a] a (t/Seqable b) -> (t/ASeq a)]))

clojure.core/reduced (t/All [x] [x -> (Reduced x)])

#_(comment
  clojure.core/reduce
       (t/All [a c d]
            (t/IFn 
              ;Without accumulator
              ; empty coll, f takes no args
              ; (reduce + []) => 0, (reduce + nil) => 0
              [[-> c] (t/U nil (t/I (ExactCount 0) (t/Seqable c))) -> c]
              ; coll count = 1, f is not called
              ; (reduce + [1]) => 1
              [t/Any (t/I (ExactCount 1) (t/Seqable c)) -> c]
              ; coll count >= 2
              ; (reduce + [1 2]) => 3
              [[c c -> c] (t/I (t/CountRange 2) (t/Seqable c)) -> c]
              ; default
              ; (reduce + my-coll)
              [(t/IFn [c c -> c] [-> c]) (t/Seqable c) -> c]
              ;With accumulator
              ; empty coll, f not called, returns accumulator
              ; (reduce + 3 []) => 3
              [t/Any a (t/U nil (t/I (ExactCount 0) (t/Seqable t/Any))) -> a]
              ; default
              ; (reduce + 3 my-coll)
              [[a c -> a] a (t/Seqable c) -> a]))
  )

;should be special cased
clojure.core/not= [t/Any t/Any * -> t/Bool]

clojure.core/first
(t/All [x]
       (t/IFn [(t/HSequential [x t/Any *]) -> x
               :object {:id 0 :path [(Nth 0)]}]
              [(t/Option (t/EmptySeqable x)) -> nil]
              [(t/NonEmptySeqable x) -> x]
              [(t/Seqable x) -> (t/Option x)]))

clojure.core/second
(t/All [x]
       (t/IFn [(t/HSequential [t/Any x t/Any *]) -> x
               :object {:id 0 :path [(Nth 1)]}]
              [(t/Option (t/I (t/Seqable x) (t/CountRange 0 1))) -> nil]
              [(t/I (t/Seqable x) (t/CountRange 2)) -> x]
              [(t/Seqable x) -> (t/Option x)]))

clojure.core/ffirst
(t/All [x]
       [(t/Seqable (t/Seqable x)) -> (t/Nilable x)])

clojure.core/nfirst
(t/All [x]
       [(t/Seqable (t/Seqable x)) -> (t/NilableNonEmptyASeq x)])

clojure.core/group-by
(t/All [x y]
     [[x -> y] (t/Seqable x) -> (t/Map y (t/Vec x))])

clojure.core/fnext
(t/All [x]
       [(t/Seqable x) -> (t/Option x)])

clojure.core/nnext
(t/All [x]
       [(t/Seqable x) -> (t/NilableNonEmptyASeq x)])

clojure.core/nthnext
(t/All [x]
       (t/IFn [nil t/AnyInteger -> nil]
              [(t/Seqable x) t/AnyInteger -> (t/NilableNonEmptyASeq x)]))

clojure.core/rest
(t/All [x]
       [(t/Seqable x) -> (t/ASeq x)])

clojure.core/last
(t/All [x]
       (t/IFn [(t/NonEmptySeqable x) -> x]
              [(t/Seqable x) -> (t/U nil x)]))

clojure.core/butlast
(t/All [x]
       [(t/Seqable x) -> (t/ASeq x)])

clojure.core/next
(t/All [x]
       (t/IFn [(t/Option (t/Coll x)) -> (t/NilableNonEmptyASeq x)
               :filters {:then (& (is (t/CountRange 2) 0)
                                  (! nil 0))
                         :else (| (is (t/CountRange 0 1) 0)
                                  (is nil 0))}]
              [(t/Seqable x) -> (t/NilableNonEmptyASeq x)]))

clojure.core/into
(t/All [x y :named [a]]
       (t/IFn [(t/Map x y) (t/Seqable (t/Seqable (IMapEntry x y))) -> (t/Map x y)]
              [(t/Vec x) (t/Seqable x) -> (t/Vec x)]
              [(t/Set x) (t/Seqable x) -> (t/Set x)]
              [(t/Coll t/Any) (t/Seqable t/Any) -> (t/Coll t/Any)]
              ; transducer arities
              [(t/Map x y) (t/Transducer a (t/Nilable '[x y])) (t/Seqable a) -> (t/Map x y)]
              [(t/Vec x) (t/Transducer y x) (t/Seqable y) -> (t/Vec x)]
              [(t/Set x) (t/Transducer y x) (t/Nilable (t/Seqable y)) -> (t/Set x)]
              [(t/Coll t/Any) (t/Transducer y t/Any) (t/Nilable (t/Seqable y)) -> (t/Coll t/Any)]))

clojure.core/conj
;     (t/All [e
;           [Arg :< (t/TFn [[x :variance :covariant]] t/Any)]
;           [Res :< (t/TFn [[x :variance :covariant]]
;                     (t/Coll t/Any))]]
;          (t/IFn [(clojure.lang.IPersistentCollection e Arg Res) (Arg e) (Arg e) * -> (Res e)]
;              [nil e e * -> (clojure.lang.PersistentList e)]))


(t/All [x y]
       (t/IFn [(t/Vec x) x x * -> (t/Vec x)]
              [(APersistentMap x y)
               (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
               (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) *
               -> (APersistentMap x y)]
              [(t/Map x y)
               (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
               (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) * -> (t/Map x y)]
              [(t/Set x) x x * -> (t/Set x)]
              [(t/ASeq x) x x * -> (t/ASeq x)]
              [nil x x * -> (clojure.lang.PersistentList x)]
              [(t/Coll t/Any) t/Any t/Any * -> (t/Coll t/Any)]
              ))

; IPersistentCollection [[x :variance :covariant]
;                        :conj-fn [conj-fn :kind (t/TFn [[x :variance :covariant]] (IPersistentCollection x))]
;                        :empty-fn [empty-fn :kind (t/TFn [] (IPersistentCollection t/Nothing :count (ExactCount 0)))]]

; clojure.core/conj
;   (t/All [x conj-fn]
;     [(IPersistentCollection x :conj-fn conj-fn) x -> (conj-fn x)]
;     [nil x -> (PersistentList x)]
;     [(t/U nil (IPersistentCollection x :conj-fn conj-fn)) x -> (t/U nil (conj-fn x))])

; clojure.core/empty
;   (t/All [x empty-fn]
;      [(IPersistentCollection t/Any :empty-fn empty-fn) -> (empty-fn)]
;      [nil -> nil]
;      [(t/U nil (IPersistentCollection t/Any :empty-fn empty-fn)) -> (t/U nil (empty-fn))])

clojure.core/sequence
(t/All [a b]
       (t/IFn [(t/Nilable (t/Seqable a)) -> (t/Seq a)]
              [(t/Transducer a b) (t/Nilable (t/Seqable a)) :-> (t/Seqable b)]))
clojure.core/find
(t/All [x y]
       [(t/Nilable (clojure.lang.Associative t/Any x y)) t/Any -> (t/Nilable (t/HVec [x y]))])

)
    (h/var-mappings
      this-ns

clojure.core/get-in
(t/IFn [t/Any (t/Nilable (t/Seqable t/Any)) -> t/Any]
       [t/Any (t/Nilable (t/Seqable t/Any)) t/Any -> t/Any])

clojure.core/assoc-in
    [(t/Nilable (Associative t/Any t/Any t/Any)) (t/Seqable t/Any) t/Any -> t/Any]

;FIXME maps after the first can always be nil
clojure.core/merge 
(t/All [k v]
       (t/IFn [nil * -> nil]
              [(t/Map k v) (t/Map k v) * -> (t/Map k v)]
              [(t/Option (t/Map k v)) * -> (t/Option (t/Map k v))]))

;more to be said here?
clojure.core/contains? [(t/Option (t/Seqable t/Any)) t/Any -> t/Bool]

clojure.core/= [t/Any t/Any * -> t/Bool]
clojure.core/identical? [t/Any t/Any -> t/Bool]
clojure.core/distinct? [t/Any t/Any * -> t/Bool]

clojure.core/decimal? (t/Pred BigDecimal)

clojure.core/denominator [clojure.lang.Ratio -> t/Num]

clojure.core/mod (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                        [t/Num t/Num -> t/Num])

clojure.core/var-get (t/All [r] [(t/Var2 t/Nothing r) -> r])
clojure.core/var-set (t/All [w] [(t/Var2 w t/Any) w -> w])

clojure.core/supers [Class -> (t/U nil (t/I t/NonEmptyCount (t/Set Class)))]

clojure.core/take-nth (t/All [x] [t/AnyInteger (t/U nil (t/Seqable x)) -> (t/ASeq x)])

clojure.core/shuffle (t/All [x] 
                            (t/IFn [(t/I (Collection x) (t/Seqable x)) -> (t/Vec x)]
                                   [(Collection x) -> (t/Vec x)]))

clojure.core/special-symbol? [t/Any -> t/Bool]

clojure.core/integer? (t/Pred t/AnyInteger)
clojure.core/number? (t/Pred t/Num)
clojure.core/var? (t/Pred (t/Var2 t/Nothing t/Any))
clojure.core/class? (t/Pred Class)

clojure.core/resolve (t/IFn [t/Symbol -> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
                         ; should &env arg be more accurate?
                         [t/Any t/Symbol -> (t/U (t/Var2 t/Nothing t/Any) Class nil)])

clojure.core/ns-resolve (t/IFn [(t/U t/Symbol t/Namespace) t/Symbol -> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
                            ; should &env arg be more accurate?
                            [(t/U t/Symbol t/Namespace) t/Any t/Symbol -> (t/U (t/Var2 t/Nothing t/Any) Class nil)])

clojure.core/extenders [t/Any -> (t/U nil (t/Seqable (t/U Class nil)))]

clojure.core/+ (t/IFn [Long * -> Long]
                    [(t/U Long Double) * -> Double]
                    [t/AnyInteger * -> t/AnyInteger]
                    [t/Num * -> t/Num])
clojure.core/- (t/IFn [Long Long * -> Long]
                    [(t/U Long Double) (t/U Long Double) * -> Double]
                    [t/AnyInteger t/AnyInteger * -> t/AnyInteger]
                    [t/Num t/Num * -> t/Num])
clojure.core/* (t/IFn [Long * -> Long]
                    [(t/U Long Double) * -> Double]
                    [t/AnyInteger * -> t/AnyInteger]
                    [t/Num * -> t/Num])
clojure.core// (t/IFn [Double Double * -> Double]
                    [t/Num t/Num * -> t/Num])

clojure.core/+' (t/IFn [t/AnyInteger * -> t/AnyInteger]
                     [t/Num * -> t/Num])
clojure.core/-' (t/IFn [t/AnyInteger t/AnyInteger * -> t/AnyInteger]
                     [t/Num t/Num * -> t/Num])
clojure.core/*' (t/IFn [t/AnyInteger * -> t/AnyInteger]
                    [t/Num * -> t/Num])
clojure.core/quot (t/IFn [Long Long -> Long]
                         [(t/U Long Double) (t/U Long Double) -> Double]
                         [t/AnyInteger t/AnyInteger -> t/AnyInteger] 
                         [t/Num t/Num -> t/Num])

clojure.core/unchecked-inc (t/IFn [t/AnyInteger -> t/AnyInteger]
                                [t/Num -> t/Num])
clojure.core/unchecked-inc-int [t/Num -> t/AnyInteger]
clojure.core/unchecked-dec (t/IFn [t/AnyInteger -> t/AnyInteger]
                                [t/Num -> t/Num])
clojure.core/unchecked-dec-int [t/Num -> t/AnyInteger]
clojure.core/unchecked-subtract (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                     [t/Num t/Num -> t/Num])
clojure.core/unchecked-subtract-int [t/Num t/Num -> t/AnyInteger]
clojure.core/unchecked-negate (t/IFn [t/AnyInteger -> t/AnyInteger]
                                   [t/Num -> t/Num])
clojure.core/unchecked-negate-int [t/Num -> t/AnyInteger]
clojure.core/unchecked-add (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                [t/Num t/Num -> t/Num])
clojure.core/unchecked-add-int [t/Num t/Num -> t/AnyInteger]
clojure.core/unchecked-multiply (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                     [t/Num t/Num -> t/Num])
clojure.core/unchecked-multiply-int [t/Num t/Num -> t/AnyInteger]
clojure.core/unchecked-divide-int [t/Num t/Num -> t/AnyInteger]
clojure.core/unchecked-remainder-int [t/Num t/Num -> t/AnyInteger]
clojure.core/rem (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                        [t/Num t/Num -> t/Num])
clojure.core/inc (t/IFn [Long -> Long]
                        [Double -> Double]
                        [t/AnyInteger -> t/AnyInteger]
                        [t/Num -> t/Num])
clojure.core/dec (t/IFn [Long -> Long]
                        [Double -> Double]
                        [t/AnyInteger -> t/AnyInteger]
                        [t/Num -> t/Num])

clojure.core/inc' (t/IFn [t/AnyInteger -> t/AnyInteger]
                       [t/Num -> t/Num])
clojure.core/dec' (t/IFn [t/AnyInteger -> t/AnyInteger]
                          [t/Num -> t/Num])

clojure.core/rationalize [t/Num -> t/Num]

clojure.core/bit-not [t/AnyInteger -> t/AnyInteger]
clojure.core/bit-and [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-or [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-xor [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-and-not [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-clear [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-set [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-flip [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-test [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-shift-left [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-shift-right [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/unsigned-bit-shift-right [t/AnyInteger t/AnyInteger -> t/AnyInteger]

clojure.core/even? [t/AnyInteger -> t/Bool]
clojure.core/odd? [t/AnyInteger -> t/Bool]

clojure.core/peek (t/All [x]
                         (t/IFn [(t/I t/NonEmptyCount (t/Stack x)) -> x]
                                [(t/Stack x) -> x]))
clojure.core/pop (t/All [x]
                      (t/IFn
                        [(t/List x) -> (t/List x)]
                        [(t/Vec x) -> (t/Vec x)]
                        [(t/Stack x) -> (t/Stack x)]))

clojure.core/get-thread-bindings
    [-> (t/Map (t/Var2 t/Nothing t/Any) t/Any)]
clojure.core/bound-fn*
    (t/All [r b ...]
         [[b ... b -> r] -> [b ... b -> r]])

clojure.core/find-var
    [t/Symbol -> (t/U nil (t/Var2 t/Nothing t/Any))]

clojure.core/agent
    (t/All [x] [x & :optional {:validator (t/U nil [x -> t/Any]) :meta t/Any
                             :error-handler (t/U nil [(t/Agent1 x) Throwable -> t/Any])
                             :error-mode (t/U ':continue ':fail)} 
              -> (t/Agent1 x)])

clojure.core/set-agent-send-executor!
    [java.util.concurrent.ExecutorService -> t/Any]

clojure.core/set-agent-send-off-executor!
    [java.util.concurrent.ExecutorService -> t/Any]

clojure.core/send-via (t/All [w r b ...] 
                           [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])

clojure.core/send (t/All [w r b ...] 
                           [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])

clojure.core/send-off (t/All [w r b ...] 
                           [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])

clojure.core/await [(t/Agent2 t/Nothing t/Any) * -> nil]
clojure.core/await-for [t/AnyInteger (t/Agent2 t/Nothing t/Any) * -> t/Bool]
clojure.core/await1 (t/All [w r] [(t/Agent2 w r) -> (t/Agent2 w r)])

clojure.core/release-pending-sends [-> t/AnyInteger]

clojure.core/add-watch
(t/All [x [a :< (IRef t/Nothing x)]]
       (t/IFn 
         ; this arity remembers the type of reference we pass to the function
         [a t/Any [t/Any a x x -> t/Any] -> t/Any]
         ; if the above cannot be inferred, 
         [(IRef t/Nothing x) t/Any [t/Any (IRef t/Nothing x) x x -> t/Any] -> t/Any]))

clojure.core/remove-watch [(IRef t/Nothing t/Any) t/Any -> t/Any]

clojure.core/agent-error [(t/Agent2 t/Nothing t/Any) -> (t/U nil Throwable)]

clojure.core/restart-agent
(t/All [w]
     ; w is invariant
     [(t/Agent2 w t/Any) w & :optional {:clear-actions t/Any} -> t/Any])

clojure.core/set-error-handler!
(t/All [w r]
    [(t/Agent2 w r) [(t/Agent2 w r) Throwable -> t/Any] -> t/Any])

clojure.core/error-handler
(t/All [w r]
    [(t/Agent2 w r) -> (t/U nil [(t/Agent2 w r) Throwable -> t/Any])])

clojure.core/set-error-mode!
    [(t/Agent2 t/Nothing t/Any) (t/U ':fail ':continue) -> t/Any]

clojure.core/error-mode
    [(t/Agent2 t/Nothing t/Any) -> t/Any]

clojure.core/agent-errors
    [(t/Agent2 t/Nothing t/Any) -> (t/U nil (t/ASeq Throwable))]
clojure.core/clear-agent-errors
    [(t/Agent2 t/Nothing t/Any) -> t/Any]

clojure.core/shutdown-agents [-> t/Any]

clojure.core/take
(t/All [x]
       (t/IFn [t/Int :-> (t/Transducer x x)]
              [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/ASeq x)]))

clojure.core/drop
(t/All [x]
       (t/IFn [t/Int :-> (t/Transducer x x)]
              [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/ASeq x)]))

clojure.core/take-last
     (t/All [x]
       [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/NilableNonEmptyASeq x)])

clojure.core/drop-last
     (t/All [x]
       [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/ASeq x)])

clojure.core/hash [t/Any -> t/AnyInteger]
clojure.core/hash-combine [t/AnyInteger t/Any -> t/AnyInteger]

clojure.core/ifn? (t/Pred clojure.lang.IFn)
clojure.core/fn? (t/Pred t/Fn)

clojure.core/instance? [Class t/Any -> t/Bool]

clojure.core/cons
(t/All [x]
       [x (t/Option (t/Seqable x)) -> (t/ASeq x)])

clojure.core/reverse
(t/All [x]
       [(t/Option (t/Seqable x)) -> (t/ASeq x)])

clojure.core/rseq
(t/All [x]
       [(clojure.core.typed/Reversible x) -> (t/NilableNonEmptyASeq x)])

;coercions
clojure.core/bigdec [(t/U String t/Num) -> BigDecimal]
clojure.core/bigint [(t/U String t/Num) -> clojure.lang.BigInt]
clojure.core/biginteger [(t/U String t/Num) -> java.math.BigInteger]
clojure.core/boolean [t/Any -> t/Bool]
clojure.core/byte [(t/U Character t/Num) -> Byte]
clojure.core/char [(t/U Character t/Num) -> Character]
clojure.core/double [t/Num -> Double]
clojure.core/float [t/Num -> Float]
clojure.core/int [(t/U Character t/Num) -> Integer]
clojure.core/long [(t/U Character t/Num) -> Long]
clojure.core/num [t/Num -> t/Num]
clojure.core/short [(t/U Character t/Num) -> Short]

;array ctors
clojure.core/boolean-array (t/IFn [(t/U t/Num (t/Seqable t/Bool)) -> (Array boolean)]
                                  [t/Num (t/U t/Bool (t/Seqable t/Bool)) -> (Array boolean)])
clojure.core/byte-array (t/IFn [(t/U t/Num (t/Seqable Byte)) -> (Array byte)]
                               [t/Num (t/U Byte (t/Seqable Byte)) -> (Array byte)])
clojure.core/char-array (t/IFn [(t/U t/Num (t/Seqable Character)) -> (Array char)]
                               [t/Num (t/U t/Num (t/Seqable Character)) -> (Array char)])
clojure.core/short-array (t/IFn [(t/U t/Num (t/Seqable Short)) -> (Array short)]
                                [t/Num (t/U Short (t/Seqable Short)) -> (Array short)])
clojure.core/int-array (t/IFn [(t/U t/Num (t/Seqable t/Num)) -> (Array int)]
                              [t/Num (t/U t/Num (t/Seqable t/Num)) -> (Array int)])
clojure.core/double-array (t/IFn [(t/U t/Num (t/Seqable t/Num)) -> (Array double)]
                                 [t/Num (t/U t/Num (t/Seqable t/Num)) -> (Array double)])

;cast to java array
;; TODO rethink input and output types. eg.,
;;      clojure.core/booleans [(ReadyOnlyArray boolean) -> (t/U nil (Array boolean))]
;; TODO objects??
;;      clojure.core/objects [(ReadyOnlyArray Object) -> (t/U nil (ReadyOnlyArray Object))]
;;                                  
;; TODO propagate to Numbers/booleans etc
;clojure.core/booleans [t/Any -> (t/U nil (Array boolean))]
;clojure.core/bytes [t/Any -> (t/U nil (Array byte))]
;clojure.core/chars [t/Any -> (t/U nil (Array char))]
;clojure.core/shorts [t/Any -> (t/U nil (Array short))]
;clojure.core/ints [t/Any -> (t/U nil (Array int))]
;clojure.core/longs [t/Any -> (t/U nil (Array long))]
;clojure.core/floats [t/Any -> (t/U nil (Array float))]
;clojure.core/doubles [t/Any -> (t/U nil (Array double))]

clojure.core/max-key (t/All [x] 
                            [[x -> t/Num] x x x * -> x])
clojure.core/min-key (t/All [x] 
                            [[x -> t/Num] x x x * -> x])

clojure.core/< [t/Num t/Num * -> t/Bool]

clojure.core/<= [t/Num t/Num * -> t/Bool]

clojure.core/> [t/Num t/Num * -> t/Bool]

clojure.core/>= [t/Num t/Num * -> t/Bool]

clojure.core/== [t/Num t/Num * -> t/Bool]

clojure.core/max (t/IFn [Long Long * -> Long]
                        [Double Double * -> Double]
                        [t/Num t/Num * -> t/Num])
clojure.core/min (t/IFn [Long Long * -> Long]
                        [Double Double * -> Double]
                        [t/Num t/Num * -> t/Num])

clojure.core/ref (t/All [x] [x & :optional {:validator (t/U nil [x -> t/Any]) :meta (t/U nil (t/Map t/Any t/Any))
                                          :min-history (t/U nil t/AnyInteger)
                                          :max-history (t/U nil t/AnyInteger)}
                           -> (clojure.lang.Ref x x)])

clojure.core/rand (t/IFn [-> t/Num]
                         [t/Num -> t/Num])

clojure.core/rand-int [t/Int -> t/Int]

clojure.core/ex-info (t/IFn [(t/U nil String) (t/Map t/Any t/Any) -> t/ExInfo]
                            [(t/U nil String) (t/Map t/Any t/Any) (t/U nil Throwable) -> t/ExInfo])

clojure.core/ex-data (t/IFn [t/ExInfo -> (t/Map t/Any t/Any)]
                            [t/Any -> (t/Nilable (t/Map t/Any t/Any))])


;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (t/Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (t/Pred (IChunk t/Any)).
clojure.core/chunked-seq? [t/Any -> t/Any]
clojure.core/chunk-first 
     (t/All [x]
          ;should be IChunkedSeq -> IChunk
          [(t/Seqable x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-rest
     (t/All [x]
          ;should be IChunkRest -> t/Seq
          [(clojure.lang.Seqable x) -> (t/ASeq x)])
clojure.core/chunk-buffer
     (t/All [x]
          [(t/U Integer Long) -> (clojure.lang.ChunkBuffer x)])
clojure.core/chunk
     (t/All [x]
          [(clojure.lang.ChunkBuffer x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-cons
     (t/All [x]
          [(clojure.lang.IChunk x) (t/Seqable x) -> (t/ASeq x)])
clojure.core/chunk-append
     (t/All [x]
          [(clojure.lang.ChunkBuffer x) x -> t/Any])
;;END CHUNK HACKS


clojure.core/subvec (t/All [x] 
                           (t/IFn [(t/Vec x) t/AnyInteger -> (t/Vec x)]
                                  [(t/Vec x) t/AnyInteger t/AnyInteger -> (t/Vec x)]))

clojure.core/alias [t/Symbol t/Symbol -> nil]
clojure.core/all-ns [-> (t/NilableNonEmptyASeq t/Namespace)]

clojure.core/*file* String
clojure.core/*command-line-args* (t/NilableNonEmptyASeq String)
clojure.core/*warn-on-reflection* t/Bool
clojure.core/*compile-path* String
clojure.core/*compile-files* t/Bool
clojure.core/*unchecked-math* t/Bool
clojure.core/*compiler-options* (t/Map t/Any t/Any)
clojure.core/*in* java.io.Reader
clojure.core/*out* java.io.Writer
clojure.core/*err* java.io.Writer
clojure.core/*flush-on-newline* t/Bool
clojure.core/*print-meta* t/Bool
clojure.core/*print-dup* t/Bool
clojure.core/*print-readably* t/Bool
clojure.core/*read-eval* (t/U ':unknown t/Bool)

clojure.core/trampoline 
       (t/All [r b ...]
         [[b ... b -> (t/Rec [f] (t/U r [-> (t/U f r)]))]
          b ... b -> r])


;; math.numeric-tower

clojure.math.numeric-tower/floor
(t/IFn [t/AnyInteger -> t/AnyInteger]
    [t/Num -> t/Num])

clojure.math.numeric-tower/abs
(t/IFn [t/AnyInteger -> t/AnyInteger]
    [t/Num -> t/Num])

;; core.match

clojure.core.match/backtrack Exception

clojure.core/eval [t/Any -> t/Any]
clojure.core/rand-nth (t/All [x] [(t/U (Indexed x) (t/SequentialSeqable x)) -> x])

clojure.pprint/pprint (t/IFn [t/Any -> nil]
                           [t/Any java.io.Writer -> nil])

      )
(h/var-mappings
  this-ns
clojure.set/union (t/All [x] [(t/Set x) * -> (t/Set x)])
clojure.set/intersection (t/All [x] [(t/Set x) (t/Set x) * -> (t/Set x)])
clojure.set/difference (t/All [x] [(t/Set x) (t/Set t/Any) * -> (t/Set x)])


clojure.repl/pst (t/IFn [-> nil]
                      [(t/U t/Int Throwable) -> nil]
                      [Throwable t/Int -> nil])
clojure.repl/print-doc [t/Symbol -> t/Any]
clojure.repl/find-doc [(t/U String java.util.regex.Pattern) -> t/Any]
clojure.repl/source-fn [t/Any -> (t/U nil String)]
clojure.java.javadoc/javadoc [Object -> t/Any]
complete.core/completions
(t/IFn [t/Any -> t/Any]
     [t/Any t/Any -> t/Any])
  )
    {'clojure.core/count (count-type)
     'clojure.core/aset-boolean (aset-*-type 'boolean)
     'clojure.core/aset-byte (aset-*-type 'byte)
     'clojure.core/aset-char (aset-*-type 'char)
     'clojure.core/aset-short (aset-*-type 'short)
     'clojure.core/aset-int (aset-*-type 'int)
     'clojure.core/aset-long (aset-*-type 'long)
     'clojure.core/aset-float (aset-*-type 'float)
     'clojure.core/aset-double (aset-*-type 'double)
     'clojure.core/nth (nth-type)
     'clojure.core/get (get-type)
     'clojure.core/reduced? (reduced?-type)
     'clojure.core/zero? (zero?-type)
     'clojure.core/compare (compare-type)
     }
))


;(comment
;  (aget my-array 0 1 2)
;  (aget (aget my-array 0) 1 2)
;  (aget (aget (aget my-array 0) 1) 2)
;
;  (App [(Associative a b) c d -> (Associative (t/U a c) (t/U b d))]
;       (App [(Associative a b) c d -> (Associative (t/U a c) (t/U b d))]
;            (App [(Associative a b) c d -> (Associative (t/U a c) (t/U b d))]
;                 (Associative t/Keyword t/Num)
;                 :a 1)
;            :b 2)
;       :c 3)
;
;  (assoc my-map :a 1 :b 2 :c 3)
;  (assoc (assoc my-map :a 1) :b 2 :c 3)
;  (assoc (assoc (assoc my-map :a 1) :b 2) :c 3)
;
;  clojure.core/aset
;       (Label [rec]
;              (t/All [w [v :< w] :dotted [b]]
;                   [(Array w _) t/AnyInteger v -> v]
;                   [(Array _ r) t/AnyInteger b ... b
;                    :recur (rec r b ... b)]))
;
;  clojure.core/aget 
;       (Label [rec]
;              (t/All [x :dotted [b]] 
;                   (t/IFn [(Array _ x) t/AnyInteger -> x]
;                       [(Array _ x) t/AnyInteger b ... b
;                        :recur 
;                        (rec x b ... b)])))
;
;  clojure.core/assoc 
;       (t/All [[h <: (t/Map t/Any t/Any)]
;             a b e ...2]
;         [h k ...2 a b -> (t/Assoc h k ...2 a b)])
;
;       (Label [rec]
;              (t/All [[h :< (HMap {})] x y [k :< (t/I AnyValue t/Keyword)] [e :< k] :dotted [b]]
;                   [h k v -> (t/I h (HMap k v))]
;                   [(Associative y x) y x -> (Associative y x)]
;                   [h k v b ... b
;                    :recur (rec (t/I h (HMap {k v})) b ... b)]
;                   [(Associative y x) y x b ... b
;                    :recur (rec (Associative y x) b ... b)]
;                   ))
;
;  clojure.core/dissoc
;       (Label [rec]
;              (t/All [[m :< (Associative _ _)] :dotted [b]]
;                   [nil t/Any * -> nil]
;                   [m -> m]
;                   [m k b ... b
;                    :recur
;                    (rec (t/I m (HMap {} :without [k])) b ... b)]))
;
;  (update-in {:a {:b 1}} [:a :b] inc)
;  (update-in 
;    (update-in {:a {:b 1}} [:a] inc) 
;    [:b] 
;    inc)
;
;  clojure.core/update-in
;       (FixedPoint
;         (t/All [[x :< (t/U nil (Associative t/Any t/Any))] k [l :< k] v r e
;               :dotted [a b]]
;              (t/IFn [(HMap {l v}) (t/HSequential [k]) [v a ... a -> r] a ... a -> (t/I x (HMap {l r}))]
;                  [(HMap {l r}) (t/HSequential [k b ... b]) [v a ... a -> e] a ... a
;                   :recur
;                   [r (t/HSequential [b ... b]) [v a ... a -> e] a ... a]])))
;
;  ;clojure.core/get-in 
;  ;     (Label [rec]
;  ;       (t/All [[x :< (t/U nil (Associative t/Any t/Any))] k :dotted [b]]
;  ;            (t/IFn [x (t/HSequential []) -> x]
;  ;                [x (t/HSequential []) _ -> x]
;  ;                [(t/U nil (Associative _ y) (t/HSequential [k b ... b]) a -> x
;  ;                ;TODO
;  ;                [(t/U nil (Associative t/Any y)) (t/HSequential [k]) -> (t/U nil x)]
;  ;                    ))))
;
;  clojure.core/partial 
;       (Label [rec]
;              (t/All [x [a :< x] r :dotted [b c]]
;                   (t/IFn [[x c ... c -> r] a -> [c ... c -> r]]
;                       [[x c ... c -> r] a b ... b
;                        :recur
;                        (rec [c ... c -> r] b ... b)])))
;
;  ;                                [[y -> x] [b ... b -> y] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [b ... b -> z] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [b ... b -> k] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [b ... b -> l] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [m -> l] [b ... b -> m] -> [b ... b -> x]]
;
;  clojure.core/juxt
;                  (t/All [y b ... c ...]
;                       [[b ... b -> y] [b ... b -> c] ... c -> [b ... b -> (DottedVec y c ... c)]])
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nocheck env

(delay-and-cache-env ^:private init-var-nochecks
  (set (keys (init-var-env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method param annotations

(delay-and-cache-env ^:private init-method-nilable-param-env {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method return annotations

(delay-and-cache-env ^:private init-method-nonnilable-return-env
  (h/method-nonnilable-return-mappings

java.lang.Object/getClass #{0}
clojure.lang.Compiler/munge :all
java.lang.Class/getName :all
java.lang.Class/forName :all

java.lang.Object/toString :all
java.lang.String/toUpperCase :all
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override annotations

(delay-and-cache-env ^:private init-method-override-env
  ;(reset-alias-env!)
  (merge
    {'clojure.lang.RT/nth (nth-type)
     'clojure.lang.RT/isReduced (reduced?-type)
     'clojure.lang.RT/isZero (zero?-type)
     'clojure.lang.Util/compare (compare-type)
     }
    (h/method-override-mappings

clojure.lang.Indexed/nth
  (t/All [x y]
       (t/IFn [(Indexed x) t/AnyInteger -> x]
           [(Indexed x) t/AnyInteger y -> (t/U x y)]))


;what about combinations of references and primitives?
clojure.lang.RT/box
(t/All [x]
     (t/IFn [Character -> Character]
          [Integer -> Integer]
          [Short -> Short]
          [t/Bool -> t/Bool]
          [Byte -> Byte]
          [Long -> Long]
          [Float -> Float]
          [Double -> Double]
          [(t/U Byte Short Integer Long) -> t/AnyInteger]
          [(t/U Float Double) -> t/Num]
          [nil -> nil]
          [x -> x]))

clojure.lang.RT/booleanCast [t/Any -> t/Bool]

clojure.lang.Numbers/char_array (t/IFn [(t/U t/Num (t/Seqable Character)) -> (Array char)]
                                       [t/Num (t/U t/Num (t/Seqable Character)) -> (Array char)])


clojure.lang.LockingTransaction/runInTransaction
                 (t/All [x]
                   [[-> x] -> x])

;array ops
clojure.lang.RT/alength [(ReadOnlyArray t/Any) -> Integer]

clojure.lang.RT/aget (t/All [o]
                        [(ReadOnlyArray o) Integer -> o])

clojure.lang.RT/aset (t/All [i o]
                          [(Array2 i o) t/AnyInteger i -> o])

;numbers
clojure.lang.Numbers/add (t/IFn [Long Long -> Long]
                              [Double Double -> Double]
                              [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                              [t/Num t/Num -> t/Num])
clojure.lang.Numbers/inc (t/IFn [Long -> Long]
                              [Double -> Double]
                              [t/AnyInteger -> t/AnyInteger]
                              [t/Num -> t/Num])
clojure.lang.Numbers/dec (t/IFn [Long -> Long]
                              [Double -> Double]
                              [t/AnyInteger -> t/AnyInteger]
                              [t/Num -> t/Num])
clojure.lang.Numbers/quotient (t/IFn [Long Long -> Long]
                                   [(t/U Long Double) (t/U Long Double) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [t/Num t/Num -> t/Num])
clojure.lang.Numbers/incP (t/IFn [Long -> (t/U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [t/AnyInteger -> t/AnyInteger]
                               [t/Num -> t/Num])
clojure.lang.Numbers/decP (t/IFn [Long -> (t/U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [t/AnyInteger -> t/AnyInteger]
                               [t/Num -> t/Num])
clojure.lang.Numbers/unchecked_inc (t/IFn [Long -> Long]
                                        [Double -> Double]
                                        [t/AnyInteger -> t/AnyInteger]
                                        [t/Num -> t/Num])
clojure.lang.Numbers/unchecked_dec (t/IFn [Long -> Long]
                                        [Double -> Double]
                                        [t/AnyInteger -> t/AnyInteger]
                                        [t/Num -> t/Num])
clojure.lang.Numbers/unchecked_int_inc [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_dec [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_negate [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_subtract [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_add [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_minus (t/IFn 
                                       ; negate
                                       [Long -> Long]
                                       [Double -> Double]
                                       [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                       [t/Num t/Num -> t/Num]
                                       ; subtract
                                       [Long Long -> Long]
                                       [(t/U Long Double) (t/U Long Double) -> Double]
                                       [t/AnyInteger -> t/AnyInteger]
                                       [t/Num -> t/Num])
clojure.lang.Numbers/minus (t/IFn
                             ; negate
                             [Long -> Long]
                             [Double -> Double]
                             [t/AnyInteger -> t/AnyInteger]
                             [t/Num -> t/Num]
                             ;minus
                             [Long Long -> Long]
                             [(t/U Double Long) (t/U Double Long) -> Long]
                             [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                             [t/Num t/Num -> t/Num])
clojure.lang.Numbers/unchecked_multiply (t/IFn [Long Long -> Long]
                                             [(t/U Long Double) (t/U Long Double) -> Double]
                                             [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                             [t/Num t/Num -> t/Num])
clojure.lang.Numbers/unchecked_int_multiply [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_divide [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_remainder [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/remainder [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/multiply (t/IFn [Long Long -> Long]
                                   [(t/U Double Long) (t/U Double Long) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [t/Num t/Num -> t/Num])
clojure.lang.Numbers/divide (t/IFn [Long Long -> Long]
                                   [(t/U Double Long) (t/U Double Long) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [t/Num t/Num -> t/Num])
      ;bit-not
clojure.lang.Numbers/not [t/AnyInteger -> Long]
;bit-and
clojure.lang.Numbers/and [t/AnyInteger t/AnyInteger -> Long]
;bit-or
clojure.lang.Numbers/or [t/AnyInteger t/AnyInteger -> Long]
;bit-xor
clojure.lang.Numbers/xor [t/AnyInteger t/AnyInteger -> Long]
;bit-and-not
clojure.lang.Numbers/andNot [t/AnyInteger t/AnyInteger -> Long]
; unsigned-bit-shift-right 
clojure.lang.Numbers/unsignedShiftRight [t/AnyInteger t/AnyInteger -> Long]

clojure.lang.Numbers/max (t/IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                           [t/Num t/Num -> t/Num])
clojure.lang.Numbers/min (t/IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                           [t/Num t/Num -> t/Num])

clojure.lang.Numbers/lt [t/Num t/Num -> t/Bool]
clojure.lang.Numbers/lte [t/Num t/Num -> t/Bool]
clojure.lang.Numbers/gt [t/Num t/Num -> t/Bool]
clojure.lang.Numbers/gte [t/Num t/Num -> t/Bool]

clojure.lang.Numbers/isNeg [t/Num -> t/Bool]
clojure.lang.Numbers/isPos [t/Num -> t/Bool]

; this is overloaded in interesting ways, but this is good enough for destructuring purposes
clojure.lang.PersistentHashMap/create [(t/U nil (ISeq t/Any) java.util.Map (ReadOnlyArray Object)) -> (t/Map t/Any t/Any)]

clojure.lang.RT/floatCast  [t/Num -> Float]
clojure.lang.RT/byteCast   [(t/U Character t/Num) -> Byte]
clojure.lang.RT/charCast   [(t/U Character t/Num) -> Character]
clojure.lang.RT/doubleCast [t/Num -> Double]
clojure.lang.RT/intCast    [(t/U Character t/Num) -> Integer]
clojure.lang.RT/longCast   [(t/U Character t/Num) -> Long]
clojure.lang.RT/shortCast  [(t/U Character t/Num) -> Short]

clojure.lang.RT/uncheckedFloatCast  [t/Num -> Float]
clojure.lang.RT/uncheckedByteCast   [(t/U Character t/Num) -> Byte]
clojure.lang.RT/uncheckedCharCast   [(t/U Character t/Num) -> Character]
clojure.lang.RT/uncheckedIntCast    [(t/U Character t/Num) -> Integer]
clojure.lang.RT/uncheckedLongCast   [(t/U Character t/Num) -> Long]
clojure.lang.RT/uncheckedShortCast  [(t/U Character t/Num) -> Short]

clojure.lang.Numbers/num   [t/Num -> t/Num]
    )
    {'clojure.lang.RT/count (count-type)
     'clojure.lang.RT/get (get-type)
     }))

(comment
  clojure.lang.IFn/invoke (t/All [r a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 arest]
                               (t/IFn
                                 [[-> r] -> r]
                                 [[a0 -> r] a0 -> r]
                                 [[a0 a1 -> r] a0 a1 -> r]
                                 [[a0 a1 a2 -> r] a0 a1 a2 -> r]
                                 [[a0 a1 a2 a3 -> r] a0 a1 a2 a3 -> r]
                                 [[a0 a1 a2 a3 a4 -> r] a0 a1 a2 a3 a4 -> r]
                                 [[a0 a1 a2 a3 a4 a5 -> r] a0 a1 a2 a3 a4 a5 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 -> r] a0 a1 a2 a3 a4 a5 a6 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 -> r] a0 a1 a2 a3 a4 a5 a6 a7 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 arest * -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 (t/Seqable arest) -> r]
                                 )))

(delay-and-cache-env ^:private init-field-override-env
  (h/field-override-mappings
clojure.lang.PersistentArrayMap/EMPTY (t/HMap :complete? true)
))

(delay-and-cache-env ^:private init-ctor-override-env
  ;(reset-alias-env!)
  (h/ctor-override-mappings

clojure.lang.LazySeq (t/All [x]
                          [[-> (t/Seqable x)] -> (LazySeq x)])
clojure.lang.Delay (t/All [x]
                        [[-> x] -> (clojure.lang.Delay x)])
    ))

;; not added in refresh
(delay-and-cache-env ^:private init-declared-kinds {})

;; not added in refresh
(delay-and-cache-env ^:private init-datatype-env {})

;; not added in refresh
(delay-and-cache-env ^:private init-datatype-ancestor-env {})

(defn reset-clojure-envs! []
  (impl/with-clojure-impl
    ;(reset-alias-env!)
    (base-rclass/reset-rclass-env!)
    (var-env/reset-var-type-env! (init-var-env) (init-var-nochecks))
    (method-return-nilables/reset-nonnilable-method-return-env! (init-method-nonnilable-return-env))
    (method-param-nilables/reset-method-nilable-param-env! (init-method-nilable-param-env))
    (method-override-env/reset-method-override-env! (init-method-override-env))
    (field-override-env/reset-field-override-env! (init-field-override-env))
    (ctor-override-env/reset-constructor-override-env! (init-ctor-override-env))
    (protocol-env/reset-protocol-env! (init-protocol-env))
    (declared-kind-env/reset-declared-kinds! (init-declared-kinds))
    (datatype-env/reset-datatype-env! (init-datatype-env))
    (datatype-ancestor-env/reset-datatype-ancestors! (init-datatype-ancestor-env)))
  nil)

(defn refresh-core-clojure-envs! []
  (impl/with-clojure-impl
    ;(refresh-core-alias-env!)
    (base-rclass/reset-rclass-env!)
    (protocol-env/merge-protocol-env! (init-protocol-env))
    (var-env/refresh-var-type-env! (init-var-env) (init-var-nochecks))
    (method-param-nilables/merge-method-nilable-param-env! (init-method-nilable-param-env))
    (method-return-nilables/merge-nonnilable-method-return-env! (init-method-nonnilable-return-env))
    (method-override-env/merge-method-override-env! (init-method-override-env))
    (field-override-env/merge-field-override-env! (init-field-override-env))
    (ctor-override-env/merge-constructor-override-env! (init-ctor-override-env)))
  nil)

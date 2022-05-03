;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.doc
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.pprint :as pp]
            [clojure.core.typed.coerce-utils :as coerce]
            [typed.clojure :as-alias t]
            [typed.cljc.checker.name-env :as name-env]
            [typed.cljc.checker.var-env :as var-env]
            [typed.clj.checker.rclass-env :as rcls]
            [typed.clj.checker.parse-unparse :as prs]))

(def special-doc
  {`t/Any {:doc "Any is the top type that contains all possible values."
           :forms '[Any]
           ::special-type true}
   `t/AnyValue {:doc "AnyValue contains all Value singleton types"
                :forms '[AnyValue]
                ::special-type true}
   `t/TCError {:doc "TCError is the type of a type error in the type checker. Use only after
                    a type error has been thrown. Only ever use this type in a custom typing rule."
               :forms '[TCError]
               ::special-type true}
   `t/U {:doc "U represents a union of types"
         :forms '[(U type*)]
         ::special-type true}
   `t/TypeOf {:doc "(alpha) Resolves to the type of the var (lazily) or local (eagerly) named by sym."
              :forms '[(TypeOf sym)]
              ::special-type true}
   `t/Nothing {:doc "Nothing is the bottom type that has no values."
               :forms '[Nothing]
               ::special-type true}
   `t/I {:doc "I represents an intersection of types"
         :forms '[(I type*)]
         ::special-type true}
   `t/Val {:doc "A singleton type for a constant value."
           :forms '[(Val Constant)
                    'Constant]
           ::special-type true}
   `t/Value {:doc "A singleton type for a constant value."
             :forms '[(Value Constant)
                      'Constant]
             ::special-type true}
   `t/CountRange {:doc "A type representing a range of counts for a collection"
                  :forms '[(CountRange Integer)
                           (CountRange Integer Integer)]
                  ::special-type true}
   `t/ExactCount {:doc "A type representing a precise count for a collection"
                  :forms '[(ExactCount Integer)]
                  ::special-type true}
   `t/Difference {:doc "Difference represents a difference of types.

                       (Difference t s) is the same as type t with type s removed.

                       eg. (Difference (U Num nil) nil)  => Num"
                  :forms '[(Difference type type type*)]
                  ::special-type true}
   `t/HVec {:doc "HVec is a type for heterogeneous vectors.
                 It extends typed.clojure/Vec and is a subtype
                 of typed.clojure/HSequential."
            :forms '[(HVec [fixed*] :filter-sets [FS*] :objects [obj*])
                     (HVec [fixed* type :*] :filter-sets [FS*] :objects [obj*])
                     (HVec [fixed* type :.. bound] :filter-sets [FS*] :objects [obj*])
                     '[fixed*]
                     '[fixed* type :*]
                     '[fixed* type :.. bound]]
            ::special-type true}
   `t/HMap {:doc "HMap is a type for heterogeneous maps."
            :forms '[(HMap :mandatory {Constant Type*}
                           :optional  {Constant Type*}
                           :absent-keys #{Constant*}
                           :complete? Boolean)
                     '{Constant Type*}]
            ::special-type true}
   `t/HSequential {:doc "HSequential is a type for heterogeneous sequential persistent collections.
                        It extends IPersistentCollection and Sequential"
                   :forms '[(HSequential [fixed*] :filter-sets [FS*] :objects [obj*])
                            (HSequential [fixed* rest *] :filter-sets [FS*] :objects [obj*])
                            (HSequential [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
                   ::special-type true}
  `t/HSeq {:doc "HSeq is a type for heterogeneous seqs"
           :forms '[(HSeq [fixed*] :filter-sets [FS*] :objects [obj*])
                    (HSeq [fixed* rest *] :filter-sets [FS*] :objects [obj*])
                    (HSeq [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
           ::special-type true}
  `t/HList {:doc "HList is a type for heterogeneous lists. Is a supertype of HSeq that implements IPersistentList."
            :forms '[(HList [fixed*] :filter-sets [FS*] :objects [obj*])
                     (HList [fixed* rest *] :filter-sets [FS*] :objects [obj*])
                     (HList [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
            ::special-type true}
  `t/HSet {:doc "HSet is a type for heterogeneous sets.
                Takes a set of simple values. By default
                :complete? is true.

                eg. (HSet #{:a :b :c} :complete? true)"
           :forms '[(HSet #{fixed*} :complete? Boolean)]
           ::special-type true}
  `t/IFn {:doc "An ordered intersection type of function arities."
          :forms '[(IFn ArityVec+)
                   [fixed* -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                   [fixed* rest * -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                   [fixed* drest ... bound -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]]
          ::special-type true}

  `t/Pred {:doc "A predicate for the given type.

                eg. Type for integer?: (Pred Int)"
           :forms '[(Pred type)]
           ::special-type true}
  `t/Assoc {:doc "A type representing an assoc operation"
            :forms '[(Assoc type type-pairs*)]
            ::special-type true}
  `t/Dissoc {:doc "A type representing a dissoc operation"
             :forms '[(Dissoc type type*)]
             ::special-type true}
  `t/Get {:doc "A type representing a get operation"
          :forms '[(Get type type)
                   (Get type type type)]
          ::special-type true}
  `t/Rec {:doc "A recursive type"
          :forms '[(Rec binder type)]
          ::special-type true}
  `t/All {:doc "A polymorphic binder"
          :forms '[(All binder type)]
          ::special-type true}
  `t/TFn {:doc "A type function"
          :forms '[(TFn binder type)]
          ::special-type true}})

(defn type-doc* [tsyn]
  (cond
    (prs/quoted? tsyn) (cond
                         (-> tsyn meta :fake-quote) (with-out-str
                                                      (println "Attaching ^:fake-quote metadata to a quoted type is equivalent to the type itself.
                                                               ie., ^:fake-quote 'TYPE <=> TYPE
                                                               This is usually done to prevent the type being evaluated when using metadata annotations."))
                         ((some-fn number? keyword? symbol? string?) (second tsyn)) (type-doc* `t/Val)
                         (vector? (second tsyn)) (type-doc* `t/HVec)
                         (map? (second tsyn)) (type-doc* `t/HMap))
    (symbol? tsyn) (let [rsym (impl/impl-case
                                :clojure (prs/resolve-type-clj->sym tsyn)
                                :cljs (prs/resolve-type-cljs tsyn))]
                     (if-some [[k {:keys [doc forms]}] (find special-doc rsym)]
                       (with-out-str
                         (println "Special type:" k)
                         (println doc)
                         (println "Forms:" forms))
                       (if-some [[alias-k alias-ty] (name-env/find-type-name-entry rsym)]
                         (with-out-str
                           (println "Type alias" alias-k)
                           (pp/pprint (prs/unparse-type (force alias-ty)))
                           (println "Metadata:")
                           (pp/pprint (meta alias-k)))
                         (if-some [ty (impl/impl-case
                                        :clojure (rcls/get-rclass rsym)
                                        :cljs nil)]
                           (with-out-str
                             (println "Class annotation for" rsym)
                             (pp/pprint (prs/unparse-type ty)))
                           (let [cls (impl/impl-case
                                       :clojure (prs/resolve-type-clj rsym)
                                       :cljs nil)]
                             (if (class? cls)
                               (with-out-str
                                 (println "Class" (coerce/Class->symbol cls)))
                               (if-some [ty (var-env/lookup-Var-nofail rsym)]
                                 (with-out-str
                                   (println "Printing the annotation for var" rsym "(see typed.clojure/TypeOf for usage in type syntax)")
                                   (pp/pprint (prs/unparse-type ty))))))))))
    (vector? tsyn) (type-doc* `t/IFn)
    ((some-fn boolean? nil?) tsyn) (type-doc* `t/Val)))

(defn type-doc-clj* [v]
  ((requiring-resolve 'clojure.core.typed/load-if-needed))
  ((requiring-resolve 'clojure.core.typed/register!))
  (impl/with-clojure-impl
    (binding [prs/*unparse-type-in-ns* (ns-name *ns*)]
      (type-doc* v))))

(defn type-doc-clj [v] (println (type-doc-clj* v)))

(defn type-doc-cljs* [v]
  ((requiring-resolve 'cljs.core.typed/load-if-needed))
  ((requiring-resolve 'cljs.core.typed/register!))
  (impl/with-cljs-impl
    (type-doc* v)))

(defn type-doc-cljs [v] (println (type-doc-cljs* v)))

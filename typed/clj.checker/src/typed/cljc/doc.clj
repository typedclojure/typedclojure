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
            [clojure.string :as str]
            [typed.clojure :as-alias t]
            [typed.cljc.runtime.env-utils :refer [force-env]]
            [typed.cljc.checker.name-env :as name-env]
            [typed.cljc.checker.var-env :as var-env]
            [typed.clj.checker.rclass-env :as rcls]
            [typed.cljc.checker.check.meta-ann :as meta-ann]
            [typed.clj.checker.parse-unparse :as prs]))

(def special-doc
  (sorted-map
    `t/Any {:doc "Any is the top type that contains all possible values."
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
    `t/HMap {:doc "HMap is a type for heterogeneous maps.
                  :mandatory/:optional  map of keywords to types for mandatory/optional entries for this map.
                  :absent-keys  set of keywords known to be absent in this map.
                  :complete?  if true, map cannot have extra keys beyond those other options. Defaults to false.
                  Quoted syntax specifies mandatory keys."
             :forms '[(HMap :mandatory {Constant Type*}
                            :optional {Constant Type*}
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
    `t/IFn {:doc "An ordered intersection type of function arities.
                 :filters  a pair of propositions. :then/:else is true when return value is true/false. See :doc/filter-syntax.
                 :object  symbolic representation of return value in terms of lexical environment. See :doc/object-syntax."
            :forms '[(IFn ArityVec+)
                     [fixed* :-> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                     [fixed* rest :* :-> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                     [fixed* rest :+ :-> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                     [fixed* opt :? :-> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                     [fixed* drest :.. bound :-> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]]
            ::special-type true}

    `t/Pred {:doc "A predicate for the given type.

                  eg. Type for integer?: (Pred Int)"
             :forms '[(Pred type)]
             ::special-type true}
    `t/Assoc {:doc "A type representing an assoc operation"
              :forms '[(Assoc type type-pairs*)]
              ::special-type true}
    `t/Merge {:doc "A type representing a merge operation"
              :forms '[(Merge types*)]
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
            ::special-type true}))

(defn type-doc* [tsyn]
  (cond
    (prs/quoted? tsyn) (cond
                         (-> tsyn meta :fake-quote) (type-doc* :fake-quote)
                         ((some-fn number? keyword? symbol? string?) (second tsyn)) (type-doc* `t/Val)
                         (vector? (second tsyn)) (type-doc* `t/HVec)
                         (map? (second tsyn)) (type-doc* `t/HMap))
    (keyword? tsyn) (some-> (case tsyn
                              (:-> :* :? :.. :filters :then :else :object :id :path) (type-doc* `t/IFn)
                              :fake-quote (str "Attaching ^:fake-quote metadata to a quoted type is equivalent to the type itself.\n"
                                               "ie., ^:fake-quote 'TYPE <=> TYPE\n\n"
                                               "This is usually done to prevent the type being evaluated when using metadata annotations.\n"
                                               "See also: :doc/metadata-caveats")
                              :doc/index (str "Documentation index\n"
                                              " :doc/{type-}syntax   An overview of the syntax for types.\n"
                                              " :doc/filter-syntax   An overview of the syntax for propositions.\n"
                                              " :doc/object-syntax   An overview of the syntax for symbolic objects.\n"
                                              " :doc/meta{data}   How to use use metadata to annotate expressions.\n")
                              (:doc/syntax :doc/type-syntax)
                              (str "Type syntax overview\n\n"
                                   "nil, true, false, are shorthand for t/Val.\n\n"
                                   "Quoted booleans, nil, keywords, symbols and numbers are shorthand for t/Val.\n\n"
                                   "Lists (T1 T2 T3) are type function applications. See t/TFn.\n\n"
                                   "Vectors like [T1 :-> T2] stand for function types. See t/IFn.\n\n"
                                   "Quoted vectors like '[T1 T2] stand for heterogenous vector types. See t/HVec.\n\n"
                                   "Quoted maps like '{:k1 T1 :k2 T2} stand for heterogenous map types. See t/HMap.\n\n"
                                   "A set of special primitive types are identified by qualified symbols.\n"
                                   "Some are used in a list like (t/Val 1), others stand alone like t/AnyValue.\n"
                                   "See :doc/specials for the full list.\n\n"
                                   "Non-special symbols resolve to type aliases defined by t/defalias or platform-specific types\n"
                                   "like classes.")
                              (:doc/filter-syntax
                                :doc/prop-syntax 
                                :doc/proposition-syntax) (str "Proposition syntax\n\n"
                                                              "tt   true proposition\n"
                                                              "ff   false proposition\n"
                                                              "(is T obj-id obj-path-elem ...)  Object has type T. See :doc/object-syntax\n"
                                                              "(! T obj-id obj-path-elem ...)  Object does not has type T. See :doc/object-syntax\n"
                                                              "(& P1 P2 ...)   conjunction\n"
                                                              "(| P1 P2 ...)   disjunction\n"
                                                              "(when P1 P2)   implication\n")
                              :doc/object-syntax (str "Object syntax\n\n"
                                                      "{:id obj-id :path [obj-path-elem ...]}\n\n"
                                                      "Object id is a symbol identifying an in-scope lexical variable or a natural\n"
                                                      "number identifying an argument of the inner-most wrapping function type.\n\n"
                                                      "Path elements are tags describing how this value was derived from the object id:\n"
                                                      "  Count  result of calling (clojure.core/count %)\n"
                                                      "  Class  result of calling (clojure.core/class %)\n"
                                                      "  (Nth idx)  result of calling (clojure.core/nth % idx)\n"
                                                      "  Key  result of calling (clojure.core/key %)\n"
                                                      "  Keys  result of calling (clojure.core/keys %)\n"
                                                      "  Vals  result of calling (clojure.core/vals %)\n"
                                                      "  Keyword  result of calling (clojure.core/keyword %)\n")
                              :doc/specials (str "Special types\n"
                                                 (str/join "\n" (map #(str "  " %) (keys special-doc))))
                              (:doc/meta :doc/metadata) (str "Typed Clojure can be configured by adding metadata to expressions. The following keys are supported:\n"
                                                             (str/join "\n" (map #(str "- " (:k %)) meta-ann/meta-keys))
                                                             "\n"
                                                             "If combined on the same expression, they will processed left-to-right based on the order of the above list.\n"
                                                             "\n"
                                                             "eg., ^::t/ignore ^{::t/unsafe-cast T} foo\n"
                                                             "       works like\n"
                                                             "     ^{::t/unsafe-cast T} (do ^::t/ignore foo)\n"
                                                             "\n"
                                                             "In Clojure, combining (:require [typed.clojure :as-alias t]) with metadata annotations avoids a runtime dependency on Typed Clojure.\n"
                                                             "See example project: https://github.com/typedclojure/typedclojure/tree/main/example-projects/zero-deps\n"
                                                             "\n"
                                                             "See also: :doc/metadata-caveats")
                              :doc/metadata-caveats (str "Be aware that certain forms behave differently with metadata attached. eg., collection literals, functions, and reify\n"
                                                         "evaluate metadata and attaches it to the final value. If in doubt, wrap expressions in (do):\n"
                                                         "eg., ^::t/OP (do EXP)\n"
                                                         "     (-> EXP ^::t/OP (do))\n"
                                                         "     (->> EXP ^::t/OP (do))\n"
                                                         "\n\n"
                                                         "When annotating metadata inside a syntax quote, it will be automatically namespaced.\n"
                                                         "This is usually exactly what you want, except for local type variables (see t/All, t/TFn).\n"
                                                         "As for local bindings, use auto gensymming in this case:\n"
                                                         "(meta `^{::t/- (t/All [x#] x#)} a)\n"
                                                         ";=> {:typed.clojure/- (typed.clojure/All [x__57308__auto__] x__57308__auto__)}\n"
                                                         "\n"
                                                         "See also: :doc/meta, :fake-quote") 
                              ::t/dbg (str "When attached to an expression as metadata, prints type debugging information on an expression during check-ns.\n"
                                           "eg., ^::t/dbg foo\n"
                                           "     ^{::t/dbg \"optional printed string\"} foo\n"
                                           "See also: :doc/metadata-caveats")
                              ::t/ignore (str "When attached to an expression as metadata, don't check the expression and ascribe type t/Any.\n"
                                              "eg., ^::t/ignore (complex :expression)\n"
                                              "See also: :typed.clojure/unsafe-cast, :doc/metadata-caveats")
                              ::t/unsafe-cast (str "When attached to an expression as metadata, check the expression with no expected type and ascribe mapped type.\n"
                                                   "eg., ^{::t/unsafe-cast t/Int} (do nil) ;; Force the type checker to infer nil as t/Int\n"
                                                   "See also: :typed.clojure/ignore, :doc/metadata-caveats")
                              ::t/- (str "When attached to an expression as metadata, mapped value is ascribed as expected type of expression.\n"
                                         "eg., ^{::t/- t/Int} foo\n\n"
                                         "When attached to function parameters or arg vectors, ascribes types to those positions.\n"
                                         "eg., (fn ^{::t/- t/Int} [^{::t/- t/Int} foo] ...)\n\n"
                                         "When attached to function name, ascribes types to entire function.\n"
                                         "eg., (fn ^{::t/- [t/Int :-> t/Int]} my-fn [foo] ...)\n\n"
                                         "See also: :doc/metadata-caveats")
                              nil))
    (symbol? tsyn) (let [rsym (impl/impl-case
                                :clojure (prs/resolve-type-clj->sym tsyn)
                                :cljs (prs/resolve-type-cljs tsyn))]
                     (if-some [[k {:keys [doc forms]}] (find special-doc rsym)]
                       (with-out-str
                         (println "Special type:" k)
                         (println doc)
                         (println "Forms:" forms))
                       (if-some [[alias-k alias-ty] (name-env/find-type-name-entry rsym)]
                         (let [alias-ty (force-env alias-ty)]
                           (with-out-str
                             (println "Type alias" alias-k)
                             (pp/pprint (if (keyword? alias-ty)
                                          alias-ty
                                          (prs/unparse-type alias-ty)))
                             (println "Metadata:")
                             (pp/pprint (meta alias-k))))
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

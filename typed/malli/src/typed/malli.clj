;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; converts types to malli.
(ns ^:typed.clojure typed.malli
  "Public API for typed ops in `malli.core`.

  In general these operations:
  - are macros instead of functions
  - take Typed Clojure types instead of malli schemas
  - helps the type checker infer their results
  
  See also:
  - typed.malli.generator
  - typed.malli.swagger
  - typed.malli.json-schema"
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.clojure :as t]
            [clojure.core.typed.unsafe :as unsafe]
            [malli.core :as m]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(defmacro validate
  "Validate v as type t. Type checker infers as a predicate
  on t.
  
  eg.,
    (validate t/AnyInteger 1) => true
    (validate t/AnyInteger nil) => false"
  [t v]
  `((unsafe/ignore-with-unchecked-cast
      m/validate
      [t/Any t/Any :-> t/Bool :filters {:then (~'is ~t 1)
                                        :else (~'! ~t 1)}])
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
    ~v))

(defmacro validator
  "Create validator for type t. Type checker infers as a predicate
  on t.
  
  eg.,
    ((validator t/AnyInteger) 1) => true
    ((validator t/AnyInteger) nil) => false"
  [t]
  `((unsafe/ignore-with-unchecked-cast
      m/validator
      [t/Any :-> [t/Any :-> t/Bool :filters {:then (~'is ~t 0)
                                             :else (~'! ~t 0)}]])
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)))

(defmacro defvalidator
  "Def validator called name for type t. Type checker infers as a predicate
  on t.

  eg.,
    (defvalidator AnyInteger? t/AnyInteger)
    (AnyInteger? 1) => true
    (AnyInteger? nil) => false"
  [name t]
  `(do (t/ann ~(vary-meta name assoc :no-check true) [t/Any :-> t/Bool
                                                      :filters {:then (~'is ~t 0)
                                                                :else (~'! ~t 0)}])
       (def ~name (validator ~t))))

(defmacro explain
  "Explain validation failure of v conforming to type t. Type checker infers as a predicate
  on \"not t\".
  
  (explain t/AnyInteger 1) => nil
  (explain t/AnyInteger nil)
  =>
  {:schema :int,
   :value nil,
   :errors
   ({:path [],
     :in [],
     :schema :int,
     :value nil,
     :type nil,
     :message nil})}"
  [t v]
  `((unsafe/ignore-with-unchecked-cast
      m/explain
      [t/Any t/Any :-> (t/Nilable '{:schema t/Any
                                    :value t/Any
                                    :errors (t/Seqable t/Any)})
       :filters {:then (~'! ~t 1)
                 :else (~'is ~t 1)}])
     ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
     ~v))

(defmacro parse
  "Parse value v as type t. Type checker infers the shape of
  return value.
  
  eg.,
    (parse t/AnyInteger 1) ;=> 1
    (parse t/AnyInteger nil) ;=> :malli.core/invalid
    (parse (t/U ^{::name :int} t/AnyInteger
                ^{::name :bool} t/Bool)
           1)
    ;=> [:int 1]
    (parse (t/U ^{::name :int} t/AnyInteger
                ^{::name :bool} t/Bool)
           true)
    ;=> [:bool true]"
  [t v]
  `((unsafe/ignore-with-unchecked-cast
      m/parse
      [t/Any t/Any :-> (t/U '::m/invalid ~(-> t 
                                              ((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax))
                                              ((requiring-resolve 'typed.malli.schema-to-type/malli-syntax->parser-type))))])
    ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
    ~v))

(defmacro parser
  "Create parser for Typed Clojure type t. Type checker infers
  the type of the return value when used.
  
  eg.,
    ((parser t/AnyInteger) 1) ;=> 1
    ((parser t/AnyInteger) nil) ;=> :malli.core/invalid
    ((parser (t/U ^{::name :int} t/AnyInteger
                  ^{::name :bool} t/Bool))
     1)
    ;=> [:int 1]
    ((parser (t/U ^{::name :int} t/AnyInteger
                  ^{::name :bool} t/Bool))
     true)
    ;=> [:bool true]"
  [t]
  `(m/parser ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)))

(defmacro defparser
  "Def parser for Typed Clojure type t. Type checker infers
  the type of the return value when used.
  
  eg.,
    (defparser AnyInteger-parser t/AnyInteger)
    (AnyInteger-parser 1) ;=> 1
    (AnyInteger-parser nil) ;=> :malli.core/invalid"
  [name t]
  `(do (t/ann ~(vary-meta name assoc :no-check true)
              [t/Any :-> (t/U (t/Val ::m/invalid)
                              ~(-> t
                                   ((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax))
                                   ((requiring-resolve 'typed.malli.schema-to-type/malli-syntax->parser-type))))])
       (def ~name (parser ~t))))

;; less baked from here

(defmacro unparse [t v]
  `(m/unparse ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)
              ~v))

(defmacro unparser [t]
  `(m/unparser ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)))

(defmacro defunparser
  [name t]
  `(do (t/ann ~(vary-meta name assoc :no-check true)
              [t/Any :-> ~t])
       (def ~name (unparser ~t))))

(defmacro -instrument [tmap & args]
  `(m/-instrument ~(update tmap :schema (requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax))
                  ~@args))

(defmacro => [name t]
  `(m/=> ~name ~((requiring-resolve 'typed.malli.parse-type/type-syntax->malli-syntax) t)))

;; unbaked

#_
(defmacro decode [t v tr]
  `(m/decode ~(type-syntax->malli-syntax t)
             ~v
             ~tr))

#_
(defmacro encode [t v tr]
  `(m/encode ~(type-syntax->malli-syntax t)
             ~v
             ~tr))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.spec
  "Public API"
  (:require [clojure.alpha.spec :as s]
            typed.clj.spec.impl))

(defmacro tfn
  "(tfn :binder
        (binder
          :x (bind-tv)
          :y (bind-tv))
        :body
        (map-of (tv :x) (tv :y)))

  A type-function that binds type variables over a body. Applied
  with `tapp`.
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tfn ~@args))))

(defmacro all
  "(all :binder
        (binder
          :x (bind-tv)
          :y (bind-tv))
        :body
        (map-of (tv :x) (tv :y)))

  A polymorphic type binding variables over a body. Applied with `inst`.
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(all ~@args))))

(defmacro binder
  "(binder
     :x (bind-tv :kind (spec-between)
                 :position #{:output})
     :y (bind-tv :kind (spec-between)
                 :position #{:output}))

  A binder of type variables. Generates a map of substitutions
  for the binder. Conforming a `binder` against a map of
  substitutions will ignore extra variables in substitution and
  generate any missing instantiations.

  Defaults:
  - :kind     (spec-between)
  - :position #{:input :output}
  - :name     nil
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(binder ~@args))))

(defmacro tapp
  "(tapp tfn {:x int? :y boolean?})

  Apply a type function (see: `tfn`) with a substitution.

  Conforms subst against the :binder of tfn and returns the :body
  of tfn after applying conformed subst.
  "
  [tfn args-map]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tapp ~tfn ~args-map))))

(defmacro inst
  "(inst poly {:x int? :y boolean?})

  Instantiate a polymorphic type (see: `all`) with a substitution.

  Conforms subst against the :binder of poly (which may infer missing
  bindings in substitution) and returns the :body of poly after applying
  conformed subst.
  "
  [poly subst]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(inst ~poly ~subst))))

(defmacro tv
  "(tv :a
       :wrap identity)

  An uninstantiated type variable. Acts as a placeholder and must be
  substituted away before use.

  Optionally takes a function that wraps the type variable after instantiation.

  Defaults:
  - :wrap    clojure.core/identity
  "
  [tv & opt]
  {:pre [(simple-keyword? tv)]}
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tv ~tv ~@opt))))

(defmacro fold-binders
  "(fold-binders (coll-of (tv :a)) :a
                 :wrap vec)

  Takes a template and type variable name tv.

  When tv is instantiated to a sequence of instantiations,
  instantiates a vector template's using the instantiations pairwise,
  and then returns the result of passing that vector to :wrap.

  Defaults:
  - :wrap  (fn [xs]
             (s/cat ~@(mapcat (fn [i x]
                                [(keyword (str (name tv) i)) x])
                              (range)
                              xs)))
  "
  [template tv & opt]
  {:pre [(simple-keyword? tv)]}
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(fold-binders ~template ~tv ~@opt))))

(defmacro spec-between
  "(spec-between :lower lower :upper upper)

  Generates and conforms to symbolic specs between specs lower and upper.
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(spec-between ~@args))))

(defmacro bind-tv
  "(bind-tv :name :x
            :kind (spec-between))

  A template for all the instantiations of a type variable.
  If :dotted true, uses :regex to generate the sequence of instantions
  of tv.

  :name enables self-recursion.

  :kind represents the set of possible values to instantiate
  the type variable with. By default this is the set of all
  specs `s/spec?`.

  Defaults:
  - :kind     (spec-between)
  - :name     nil
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(bind-tv ~@args))))

(defmacro fcase
  "(fcase
     #{0 1} (s/fspec :args (s/cat :first (s/? integer?)) :ret integer?)
     2 (s/fspec :args (s/cat :first integer? :second boolean?) :ret integer?)
     (s/fspec :args (s/cat :first integer? :second boolean? :more (s/* integer?)) :ret integer?))

  Checked in order. LHS can be integer or set of integers.
  Final case is default.
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(fcase ~@args))))

(defmacro ^{:deprecated "1.0.12"} reduced-of
  "Deprecated: Use `typed.spec.clojure.core/reduced-spec` as a direct replacement."
  [s]
  (println (str "DEPRECATED: " `reduced-of ", use typed.spec.clojure.core/reduced-spec"))
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(reduced-of ~s))))

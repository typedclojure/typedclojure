;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.spec1.util
  (:require [clojure.spec.alpha :as s1]))

(defn explicate [nsym form]
  (binding [*ns* (the-ns nsym)]
    (#'s1/res form)))

(defmulti expand-spec
  "Create a symbolic spec map from an explicated spec form. This is an
  extension point for adding new spec ops. Generally, consumers should
  instead call `resolve-spec`. For anything other than symbolic spec maps,
  return the same object unchanged."
  (fn [qform] (when (or (list? qform) (seq? qform)) (first qform))))

(defmethod expand-spec :default [o] o)

(defmulti create-spec
  "Create a spec object from an explicated spec map. This is an extension
  point for adding new spec ops. Generally, consumers should instead call
  `resolve-spec`."
  (fn [smap] (when (map? smap) (:clojure.spec/op smap))))

(defmethod create-spec :default [o] o)

(defn resolve-spec [qform]
  (cond
    (keyword? qform) (#'s1/reg-resolve qform)
    (qualified-symbol? qform) (s1/spec-impl qform
                                            (eval qform)
                                            nil
                                            nil)
    (or (list? qform) (seq? qform)) (let [created (create-spec (expand-spec qform))]
                                      (if (identical? qform created)
                                        (eval qform)
                                        created))
    (set? qform) (s1/spec-impl qform
                               qform
                               nil
                               nil)
    :else (throw (ex-info (str "typed.clj.spec1.util: unhandled spec " (pr-str qform))
                          {}))))

(comment
  (eval '(clojure.spec.alpha/fspec :args (clojure.spec.alpha/cat :fn (clojure.spec.alpha/fspec :args (clojure.spec.alpha/cat :x #{[22 -18.15625 -1]}) :ret #{-n01_/c}) :coll (clojure.spec.alpha/coll-of #{[22 -18.15625 -1]})) :ret (clojure.spec.alpha/coll-of #{-n01_/c})))
  )

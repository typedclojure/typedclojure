;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.experimental.infer-vars
  (:require 
    [clojure.core.typed.util-vars :as vs]
    [typed.cljc.checker.type-rep :as r]
    [typed.cljc.checker.type-ctors :as c]
    [typed.clj.checker.parse-unparse :as prs]
    [clojure.core.typed.current-impl :as impl]
    [typed.cljc.runtime.env :as env]))

(defn add-inferred-type 
  "Add type t to the pool of inferred types of var vsym in namespace ns."
  [nsym vsym t]
  {:pre [(symbol? nsym)
         (symbol? vsym)
         (r/Type? t)]
   :post [(nil? %)]}
  (env/swap-checker! update-in
                     [:inferred-unchecked-vars nsym vsym]
                     (fnil conj [])
                     (binding [vs/*verbose-types* true]
                       (prs/unparse-type t)))
  nil)

(defn inferred-var-in-ns
  [nsym vsym]
  {:pre [(symbol? nsym)
         (symbol? vsym)]
   :post [(r/Type? %)]}
  (if-some [ts (seq (get-in (env/deref-checker) [:inferred-unchecked-vars nsym vsym]))]
    (apply c/Un (map prs/parse-type ts))
    r/-any))

(defn using-alias-in-ns [nsym vsym]
  {:pre [(symbol? nsym)
         (symbol? vsym)
         (namespace vsym)]
   :post [(symbol? %)]}
  (if-let [alias (some-> nsym find-ns (prs/alias-in-ns (symbol (namespace vsym))))]
    (symbol (str alias) (name vsym))
    vsym))

(defn prepare-inferred-untyped-var-expression
  "Return an expression to eval in namespace nsym, which declares
  untyped var vsym as its inferred type."
  [nsym vsym]
  (let [t (inferred-var-in-ns nsym vsym)]
    (prs/with-unparse-ns nsym
      (list (using-alias-in-ns nsym 'clojure.core.typed/ann)
            (using-alias-in-ns nsym vsym)
            (prs/unparse-type t)))))

(defn infer-unannotated-vars
  "Return a vector of syntax that can be spliced into the given namespace,
  that annotates the inferred untyped variables."
  [nsym]
  (mapv (fn [vsym] (prepare-inferred-untyped-var-expression nsym vsym))
        (keys (get-in (env/deref-checker) [:inferred-unchecked-vars nsym]))))

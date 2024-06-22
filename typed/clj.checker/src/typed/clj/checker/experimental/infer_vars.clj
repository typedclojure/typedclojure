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
    [typed.clj.runtime.env :as clj-env]
    [typed.clj.checker.utils :refer [->opts]]
    [typed.cljc.checker.type-rep :as r]
    [typed.cljc.checker.type-ctors :as c]
    [typed.clj.checker.parse-unparse :as prs]
    [clojure.core.typed.current-impl :as impl]
    [typed.cljc.runtime.env :as env]))

(defn add-inferred-type
  "Add type t to the pool of inferred types of var vsym in namespace ns."
  [checker nsym vsym t opts]
  {:pre [(symbol? nsym)
         (symbol? vsym)
         (r/Type? t)]
   :post [(nil? %)]}
  (env/swap-checker! checker update-in
                     [:inferred-unchecked-vars nsym vsym]
                     (fnil conj [])
                     (prs/unparse-type t (assoc opts ::vs/verbose-types true)))
  nil)

(defn inferred-var-in-ns
  [checker nsym vsym opts]
  {:pre [(symbol? nsym)
         (symbol? vsym)]
   :post [(r/Type? %)]}
  (if-some [ts (seq (get-in (env/deref-checker checker) [:inferred-unchecked-vars nsym vsym]))]
    (c/Un (map #(prs/parse-type % opts) ts) opts)
    r/-any))

(defn using-alias-in-ns [nsym vsym opts]
  {:pre [(symbol? nsym)
         (symbol? vsym)
         (namespace vsym)]
   :post [(symbol? %)]}
  (if-let [alias (some-> nsym find-ns (prs/alias-in-ns (symbol (namespace vsym)) opts))]
    (symbol (str alias) (name vsym))
    vsym))

(defn prepare-inferred-untyped-var-expression
  "Return an expression to eval in namespace nsym, which declares
  untyped var vsym as its inferred type."
  [checker nsym vsym opts]
  (let [t (inferred-var-in-ns checker nsym vsym opts)]
    (prs/with-unparse-ns nsym
      (list (using-alias-in-ns nsym 'clojure.core.typed/ann opts)
            (using-alias-in-ns nsym vsym opts)
            (prs/unparse-type t opts)))))

(defn infer-unannotated-vars
  "Return a vector of syntax that can be spliced into the given namespace,
  that annotates the inferred untyped variables."
  ([nsym] (infer-unannotated-vars clj-env/clj-checker-atom nsym
                                  (assoc (->opts) ::prs/parse-type-in-ns nsym)))
  ([checker nsym opts]
   (mapv (fn [vsym] (prepare-inferred-untyped-var-expression checker nsym vsym opts))
         (keys (get-in (env/deref-checker checker) [:inferred-unchecked-vars nsym])))))

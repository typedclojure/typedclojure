;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.lex-env
  (:require [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.path-type :as path-type]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(t/defalias LexEnv (t/Map t/Sym r/Type))
(t/defalias PropSet (t/Set fr/Filter))
(t/defalias AliasEnv (t/Map t/Sym obj/RObject))

(def lex-env? (con/hash-c? con/local-sym? r/Type?))
(def prop-set? (con/set-c? fr/Filter?))
(def alias-env? (con/hash-c? con/local-sym? obj/RObject?))

(u/def-type PropEnv [l :- LexEnv
                     props :- PropSet
                     aliases :- AliasEnv]
  "A lexical environment l, props is a set of known propositions"
  [(lex-env? l)
   (prop-set? props)
   (alias-env? aliases)])

(defn -PropEnv 
  ([] (-PropEnv {} #{} {}))
  ([l props]
   (-PropEnv l props {}))
  ([l props aliases]
   (PropEnv-maker
     l 
     (set props)
     aliases)))

(defn init-lexical-env []
  (-PropEnv))

(defn lexical-env []
  vs/*lexical-env*)

(set-validator! #'vs/*lexical-env* (fn [a]
                                     (or (nil? a)
                                         (PropEnv? a))))

(defn lookup-alias [sym & {:keys [env]}]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? PropEnv?) env)]
   :post [(obj/RObject? %)]}
  (or (get-in (or env (lexical-env)) [:aliases sym])
      (obj/-id-path sym)))

(defn lookup-local [sym]
  {:pre [(con/local-sym? sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [; see if sym is an alias for an object
        ; if not (-id-path sym) is returned
        obj (lookup-alias sym)
        [alias-path alias-id] (cond
                                (obj/Path? obj) [(:path obj) (:id obj)]
                                (obj/EmptyObject? obj) [nil sym]
                                :else (err/int-error (str "what is this? " (pr-str obj))))
        _ (assert (pr/path-elems? alias-path))
        _ (assert (fr/name-ref? alias-id))
        lt (get-in (lexical-env) [:l alias-id])]
    ;(prn "lex-env" (lexical-env))
    (some-> lt
            (path-type/path-type alias-path))))

(defn merge-locals [env new]
  {:pre [(PropEnv? env)]
   :post [(PropEnv? %)]}
  (-> env
      (update :l into new)))

(defmacro with-locals [locals & body]
  `(binding [vs/*lexical-env* (merge-locals (lexical-env) ~locals)]
     (do ~@body)))

; take an environment and (depending on the new object given) either record
; and alias to an existing local or extend the type env directly.
(defn extend-env [env id t o]
  {:pre [(PropEnv? env)
         (con/local-sym? id)
         (r/Type? t)
         (obj/RObject? o)]
   :post [(PropEnv? %)]}
  (cond
    ; no aliasing to add
    (obj/EmptyObject? o)
    (-> env
        (assoc-in [:l id] t))

    (obj/Path? o)
    (-> env
        (assoc-in [:aliases id] o)
        ; if we have an empty path, add a "normal" entry to our
        ; type environment. Not sure why this is needed, Andrew K added
        ; it to TR because tests were failing.
        (cond-> (empty? (:path o)) (assoc-in [:l (:id o)] t)))
    :else (err/int-error (str "what is this? " (pr-str o)))))

(ns typed.cljc.checker.custom-assertions
  (:refer-clojure :exclude [defn defn- fn assert])
  (:require [clojure.core :as core]))

(defmacro assert [& args]
  (when (= (System/getProperty "typed.clojure.internal-assertions") "true")
    `(core/assert ~@args)))

(core/defn- expand-pre-post-conditions [arity-body]
  (if-some [body (when (map? (first arity-body))
                   (next arity-body))]
    (let [{:keys [pre post] :as m} (first arity-body)]
      (concat
       (dissoc (first arity-body) :pre :post)
       (map (core/fn [assertion] (list `assert assertion)) pre)
       (if post
         `((let [~'% ~(if (< 1 (count body))
                        `(do ~@body)
                        (first body))]
             ~@(map (core/fn [c] `(assert ~c)) post)
             ~'%))
         body)))
    arity-body))

(core/defn- replace-fn-like [macro args]
  (letfn [(expand-one [[arglist & body]]
            (list* arglist (expand-pre-post-conditions body)))]
    (let [[before after] (split-with (complement coll?) args)]
      (if (vector? (first after))
        `(~macro ~@before ~@(expand-one after))
        `(~macro ~@before ~@(map expand-one after))))))

(defmacro ^{:typed.clojure/check-like 'clojure.core/fn} [& args]
  (replace-fn-like `core/fn args))
(alter-meta! #'fn merge (select-keys (meta #'core/fn) [:arglists :doc :forms]))

(defmacro ^{:typed.clojure/check-like 'clojure.core/defn} defn [& args]
  (replace-fn-like `core/defn args))
(alter-meta! #'defn merge (select-keys (meta #'core/defn) [:arglists :doc :forms]))

(defmacro ^{:typed.clojure/check-like 'clojure.core/defn-} defn- [& args]
  (replace-fn-like `core/defn- args))
(alter-meta! #'defn- merge (select-keys (meta #'core/defn-) [:arglists :doc :forms]))

(comment
  (defn a [] {:pre [1]})
  (defn a [] {:pre [1]} {})
  )

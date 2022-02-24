;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.base-env-common
  "Utilities for all implementations of the type checker"
  (:require [typed.clj.checker.parse-unparse :as prs]))

(defmacro delay-and-cache-env [sym & body]
  (let [generator-sym (symbol (str "generator-" sym))
        cache-sym (symbol (str "cache-" sym))
        thread-bindings (symbol (str "thread-bindings-" sym))
        interface-sym sym]
    `(do
       (def ~thread-bindings (get-thread-bindings))
       (defn ~(with-meta generator-sym {:private true}) []
         ; switch namespace to where this def is defined
         ; Also helps parse CLJS syntax.
         (let [r# (with-bindings ~thread-bindings
                    ~@body)]
           ;(prn "r" r#)
           r#))
       ; cache is original nil, then is updated only once
       (def ~(with-meta cache-sym {:private true})
         (atom nil))
       (defn ~interface-sym []
         (if-let [hit# (deref ~cache-sym)]
           hit#
           (let [calc# (~generator-sym)]
             (reset! ~cache-sym calc#)))))))


(defn parse-cljs-ann-map
  [ann-map]
  (into {}
        (map (fn [[sym ann]]
               [(symbol "cljs.core" (name sym))
                (prs/parse-type ann)])
             ann-map)))


(defn parse-clj-ann-map
  [ann-map]
  (let [conveyed-parse (bound-fn* prs/parse-type)]
    (into {}
          (map (fn [[sym ann]]
                 [sym (delay (conveyed-parse ann))])
               ann-map))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.runtime.env)

(def ^:dynamic *checker* nil)

(defn checker-or-nil []
  {:post [(or #?(:clj (instance? clojure.lang.IAtom2 %)
                 :cljs (instance? Atom %))
              (nil? %))]}
  *checker*)

(defn checker []
  (let [c *checker*]
    (assert (or #?(:clj (instance? clojure.lang.IAtom2 c)
                   :cljs (instance? Atom c))
                (delay? c))
            (str "No checker state: " (pr-str c)))
    c))

(defn empty-checker []
  {})

(defn init-checker
  ([] (init-checker (empty-checker)))
  ([init]
   (atom init :validator map?)))

(defn deref-checker []
  {:post [(map? %)]}
  @(checker))

(defn swap-checker! [& args]
  (apply swap! (checker) args))

(defn swap-checker-vals! [& args]
  (apply swap-vals! (checker) args))

;; isolate *checker* for consistent reads
(defmacro with-pinned-env [& body]
  `(binding [*checker* (atom (deref-checker))]
     ~@body))

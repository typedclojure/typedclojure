;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.runtime.env)

(defn checker [opts]
  (let [c (::checker opts)]
    (assert (or #?(:clj (instance? clojure.lang.IAtom2 c)
                   :cljr (instance? clojure.lang.IAtom2 c)
                   :cljs (instance? Atom c))
                (delay? c))
            (str "No checker state: " (pr-str c) " " (pr-str (class opts)) " " (keys opts)))
    c))

(defn empty-checker []
  {})

(defn init-checker
  ([] (init-checker (empty-checker)))
  ([init] (atom init)))

(defn deref-checker [checker]
  {:post [(map? %)]}
  @checker)

(defn swap-checker! [c & args]
  (apply swap! c args))

(defn swap-checker-vals! [c & args]
  (apply swap-vals! c args))

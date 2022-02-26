;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.ann-macros.clojure
  (:require [typed.clojure :as t]))

(defmacro anns [& body]
  (clojure.core/let [pairs (partition 2 body)
                     _ (assert (even? (count body))
                               (vec (first (filter (comp (complement qualified-symbol?) first)
                                                   pairs))))]
    `(do ~@(map (clojure.core/fn [[n t]]
                  (assert (symbol? n) [n t])
                  `(t/ann ~n ~t))
                pairs))))

(defmacro ann-protocols [& body])

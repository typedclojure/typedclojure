;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; copied from clojure.tools.analyzer.passes.add-binding-atom
(ns typed.cljc.analyzer.passes.add-binding-atom
  (:require [typed.cljc.analyzer.ast :refer [prewalk]]
            [typed.cljc.analyzer.passes.uniquify :refer [uniquify-locals]]))

(defn add-binding-atom
  "Adds a map-containing-atom to every local binding--the same
   atom will be shared between all occurrences of that local.

   The atom is added as the :atom field of the node."
  {:pass-info {:walk :pre :depends #{#'uniquify-locals} :state (fn [] (atom {}))}}
  ([ast] (prewalk ast (partial add-binding-atom (atom {}))))
  ([state ast]
     (case (:op ast)
       :binding
       (let [a (atom {})]
         (swap! state assoc (:name ast) a)
         (assoc ast :atom a))
       :local
       (if-some [a (@state (:name ast))]
         (assoc ast :atom a)
         ;; handle injected locals
         (let [a (or (get-in ast [:env :locals (:name ast) :atom])
                     (atom {}))]
           (swap! state assoc (:name ast) a)
           (assoc ast :atom a)))
       ast)))

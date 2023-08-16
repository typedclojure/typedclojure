;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.jvm.fix-case-test
(ns typed.clj.analyzer.passes.fix-case-test
  (:require [typed.cljc.analyzer.passes.add-binding-atom :as add-binding-atom]))

(defn fix-case-test
  "If the node is a :case-test, annotates in the atom shared
   by the binding and the local node with :case-test"
  {:pass-info {:walk :pre :depends #{#'add-binding-atom/add-binding-atom}}}
  [ast]
  (when (:case-test ast)
    (swap! (:atom ast) assoc :case-test true))
  ast)

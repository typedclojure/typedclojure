;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from clojure.tools.analyzer.passes.jvm.constant-lifter
(ns ^:typed.clojure typed.clj.analyzer.passes.constant-lifter
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.passes.constant-lifter :as orig]
            [typed.cljc.analyzer.passes.elide-meta :as elide-meta]
            [typed.cljc.analyzer.utils :as cu]
            [typed.clj.analyzer.passes.analyze-host-expr :as analyze-host-expr]))

(defn constant-lift*
  [ast opts]
  (if (= :var (:op ast))
    (let [{:keys [var env form meta]} ast]
     (if (cu/constant? var meta)
       (let [val @var]
         (assoc (ana2/analyze-const val env (cu/classify val) opts)
           :form form))
       ast))
    (orig/constant-lift ast opts)))

(defn constant-lift
  "Like typed.cljc.analyzer.passes.constant-lifter/constant-lift but
   transforms also :var nodes where the var has :const in the metadata
   into :const nodes and preserves tag info"
  {:pass-info {:walk :post :depends #{} :after #{#'elide-meta/elide-meta #'analyze-host-expr/analyze-host-expr}}}
  [ast opts]
  (merge (constant-lift* ast opts)
         (select-keys ast [:tag :o-tag :return-tag :arglists])))

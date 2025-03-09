;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.check.try
  (:require [typed.cljc.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.object-rep :as o]
            [typed.cljc.checker.type-ctors :as c]))

(defn combine-rets [rs opts]
  {:pre [(seq rs)
         (every? r/TCResult? rs)]
   :post [(r/TCResult? %)]}
  (r/ret (c/Un (map r/ret-t rs) opts)
         (fo/-FS (fo/-or (map (comp :then r/ret-f) rs) opts)
                 (fo/-or (map (comp :else r/ret-f) rs) opts))
         (if (apply = (map r/ret-o rs))
           (r/ret-o (first rs))
           o/-empty))) 

; filters don't propagate between components of a `try`, nor outside of it.
(defn check-try [{:keys [body catches finally] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(vector? (:catches %))
          (-> % u/expr-type r/TCResult?)]}
  (let [chk #(check-expr % expected opts)
        cbody (chk body)
        ;_ (prn "cbody ret" (u/expr-type cbody))
        ;_ (prn cbody)
        ccatches (mapv chk catches)
        ;_ (prn "ccatches ret" (mapv u/expr-type ccatches))
        ; finally result is thrown away
        cfinally (some-> finally (check-expr nil opts))
        ret (below/maybe-check-below
              (combine-rets
                (map u/expr-type (concat [cbody] ccatches))
                opts)
              expected
              (assoc opts ::vs/current-expr expr))]
    ;(prn "try ret" ret)
    (assoc expr
           :body cbody
           :catches ccatches
           :finally cfinally
           u/expr-type ret)))

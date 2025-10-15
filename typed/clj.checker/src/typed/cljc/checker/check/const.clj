;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.check.const
  (:require [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.proposition-rep :as fl]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.constant-type :refer [constant-type]]
            [typed.cljc.checker.type-rep :as r]))

(defn filter-for-value [val]
  (if val
    (fo/-FS fl/-top fl/-bot)
    (fo/-FS fl/-bot fl/-top)))

(defn check-const
  "Given a :const node and an expected type returns a new :const
  node annotated with its type.
  
  quoted? should be true if this :const node is nested inside a
  :quote node, otherwise should be false"
  ([expr expected opts] (check-const expr expected false opts))
  ([{:keys [val] :as expr} expected quoted? opts]
   {:pre [(#{:const} (:op expr))
          ((some-fn nil? r/TCResult?) expected)]
    :post [(-> % u/expr-type r/TCResult?)]}
   (let [inferred-ret (r/ret (constant-type val quoted? opts)
                             (filter-for-value val)
                             obj/-empty)]
     (assoc expr
            u/expr-type (below/maybe-check-below inferred-ret expected opts)))))

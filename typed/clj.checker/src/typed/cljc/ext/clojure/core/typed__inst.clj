;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.ext.clojure.core.typed__inst
  "Type rule for clojure.core.typed/inst"
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.inst :as inst]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defn -unanalyzed-special__inst
  [{[_ pform & targs-syn :as form] :form :keys [env] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)]}
    (let [_ (when-not (next form)
              (err/int-error "Wrong arguments to inst" opts))
          ptype (-> pform (ana2/unanalyzed env)
                    check-expr u/expr-type r/ret-t)]
      (assoc expr
             u/expr-type (inst/inst-from-targs-syn ptype targs-syn (cu/expr-ns expr opts) expected opts))))

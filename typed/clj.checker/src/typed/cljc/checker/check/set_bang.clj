;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.set-bang
  (:require [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.subtype :as sub]))

(defn check-set! [check {:keys [target val env] :as expr} expected]
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [ctarget (check target expected)
          cval (check val (-> ctarget u/expr-type r/ret-t r/ret))]
      (assoc expr
             u/expr-type (u/expr-type cval)
             :target ctarget
             :val cval))))

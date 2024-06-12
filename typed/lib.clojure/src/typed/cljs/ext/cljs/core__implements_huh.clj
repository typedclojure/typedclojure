;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljs.ext.cljs.core__implements_huh
  "Typing rules for cljs.core/implements?"
  (:require [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.analyzer :as ana2]
            [typed.clojure :as t]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

;;==================
;; cljs.core/implements?

(defuspecial defuspecial__implements?
  "defuspecial implementation for cljs.core/implements?"
  [{:keys [form env] :as expr} expected {::check/keys [check-expr] :as opts}]
  (assert (= 3 (count form)))
  (let [[_ psym x] form
        ;;TODO grab the protocol's type (psym is syntax, not an expression)
        cx (check-expr (ana2/unanalyzed x env) nil opts)]
    (assoc expr
           u/expr-type (below/maybe-check-below
                         ;; TODO filters
                         (r/ret (c/-name `t/Bool))
                         expected
                         opts))))

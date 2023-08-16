;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.multi
  (:require [typed.cljc.checker.fold-rep :as fold]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.object-rep :as obj])
  (:import (typed.cljc.checker.type_rep Function)))

;; Multimethod definition

(fold/def-derived-fold IExpectedDispatchType
  expected-dispatch-type*
  [])

(fold/add-fold-case IExpectedDispatchType expected-dispatch-type*
  Function
  (fn [ty]
    (assoc ty :rng (r/make-Result r/-infer-any fl/-infer-FS obj/-infer-obj))))

;return the expected type for the dispatch fn of the given multimethod's expected type
;[Type -> Type]
(defn expected-dispatch-type [mm-type]
  {:pre [(r/AnyType? mm-type)]
   :post [(r/AnyType? %)]}
  (call-expected-dispatch-type*
    mm-type
    {:type-rec expected-dispatch-type}))

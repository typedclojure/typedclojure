;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.recur
  (:require [typed.cljc.checker.utils :as u]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check-below :as check-below]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.filter-ops :as fo]))

;Arguments passed to recur must match recur target exactly. Rest parameter
;equals 1 extra argument, either a Seqable or nil.
(defn check-recur [{args :exprs :keys [env] :as recur-expr} expected]
  {:post [(vector? (:exprs %))]}
  (let [{:keys [dom rest] :as recur-target} (or recur-u/*recur-target*
                                                (err/int-error (str "No recur target")))
        _ (assert (not ((some-fn :drest :kws) recur-target)) "NYI")
        fixed-args (cond-> args
                     rest butlast)
        rest-arg (when rest
                   (last args))
        rest-arg-type (when rest-arg
                        (impl/impl-case
                          :clojure (c/Un r/-nil (c/In (c/RClass-of clojure.lang.ISeq [rest])
                                                      (r/make-CountRange 1)))
                           :cljs (c/Un r/-nil (c/In (c/Protocol-of 'cljs.core/ISeq [rest])
                                                    (r/make-CountRange 1)))))
        cargs (mapv check-expr args (map r/ret
                                         (concat dom
                                                 (some-> rest-arg-type vector))))
        _ (when-not (and (= (count fixed-args) (count dom))
                         (= (boolean rest) (boolean rest-arg)))
            (err/tc-delayed-error 
              (str "Wrong number of arguments to recur:"
                   " Expected: " ((if rest inc identity) 
                                  (count dom))
                   " Given: " ((if rest-arg inc identity)
                               (count fixed-args)))))]
    (assoc recur-expr
           :exprs cargs
           u/expr-type (check-below/maybe-check-below
                         (r/ret r/-nothing (fo/-unreachable-filter))
                         expected))))

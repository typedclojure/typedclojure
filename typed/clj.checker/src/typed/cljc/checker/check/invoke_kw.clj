;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.invoke-kw
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check-below :as below]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.path-rep :as pe]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.object-rep :as obj]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.errors :as err]))

;[(U nil Expr) TCResult TCResult (Option TCResult) (Option TCResult) -> TCResult]
(defn invoke-keyword [expr kw-ret target-ret default-ret expected-ret opts]
  {:pre [(r/TCResult? kw-ret)
         (r/TCResult? target-ret)
         ((some-fn nil? r/TCResult?) default-ret)
         ((some-fn nil? r/TCResult?) expected-ret)
         ((some-fn nil? map?) expr)]
   :post [(r/TCResult? %)]}
  (let [targett (c/-resolve (r/ret-t target-ret) opts)
        kwt (r/ret-t kw-ret)
        defaultt (or (some-> default-ret r/ret-t)
                     r/-nil)]
    (cond
      ;Keyword must be a singleton with no default
      (c/keyword-value? kwt)
      (let [{path-hm :path id-hm :id :as o} (when (obj/Path? (r/ret-o target-ret))
                                              (r/ret-o target-ret))
            o (or o (r/ret-o target-ret))
            _ (assert ((some-fn obj/Path? obj/EmptyObject?) o))
            this-pelem (pe/-kpe (:val kwt))
            val-type (c/find-val-type targett kwt defaultt #{} opts)]
        (let [opts (update opts ::vs/current-env #(or expr %))]
          (below/maybe-check-below
            (if (or (r/Bottom? targett)
                    (not (r/Bottom? val-type)))
              (r/ret val-type
                     (fo/-FS (if (and (obj/Path? o)
                                      (= r/-nil defaultt))
                               ;; if val-type is falsey, this will simplify to ff
                               (let [obj (obj/-path (concat path-hm [this-pelem]) id-hm)]
                                 (fo/-and
                                   [(fo/-filter-at val-type obj)
                                    (fo/-not-filter-at r/-falsy obj)]
                                   opts))
                               fl/-top)
                             (if (and (obj/Path? o)
                                      (= r/-nil defaultt))
                               (fo/-or [(fo/-filter (c/make-HMap opts {:absent-keys #{kwt}}) id-hm path-hm) ; this map doesn't have a kwt key or...
                                        (fo/-filter r/-falsy id-hm (concat path-hm [this-pelem]))]  ; this map has a false kwt key
                                       opts)
                               fl/-top))
                     (if (and (obj/Path? o) (= r/-nil defaultt))
                       (update o :path #(seq (concat % [this-pelem])))
                       obj/-empty))
              (do (u/tc-warning (str "Keyword lookup gave bottom type: "
                                     (:val kwt) " " (prs/unparse-type targett opts))
                                opts)
                  (r/ret r/-any)))
            expected-ret
            opts)))

      :else (err/int-error (str "keyword-invoke only supports keyword lookup, no default. Found " 
                              (prs/unparse-type kwt opts))
                           opts))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.collect-utils
  (:require [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]))

(defn protocol-method-var-ann [mt names bnds opts]
  (cond
    (r/Poly? mt) (let [outer-names names
                       inner-names (concat (c/Poly-fresh-symbols* mt))]
                   (c/Poly* (concat outer-names inner-names)
                            (concat bnds (c/Poly-bbnds* inner-names mt opts))
                            (c/Poly-body* inner-names mt opts)
                            {:named (:named mt)}
                            opts))

    (r/PolyDots? mt) (let [outer-names names
                           inner-names (concat (c/PolyDots-fresh-symbols* mt))]
                       (c/PolyDots* (concat outer-names inner-names)
                                    (concat bnds (c/PolyDots-bbnds* inner-names mt opts))
                                    (c/PolyDots-body* inner-names mt opts)
                                    {:named (:named mt)}
                                    opts))
    :else (let [outer-names names]
            (c/Poly* outer-names
                     bnds
                     mt
                     opts))))

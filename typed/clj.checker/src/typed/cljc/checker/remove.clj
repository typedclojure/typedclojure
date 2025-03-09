;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.remove
  (:require [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.type-rep :as r]))

;; also not yet correct
;; produces old without the contents of rem
;[Type Type -> Type]
(defn remove* [old rem opts]
  (let [old (c/fully-resolve-type old opts)
        rem (c/fully-resolve-type rem opts)
        initial (if (sub/subtype? old rem opts)
                  (r/Bottom) ;the empty type
                  (cond
                    ;FIXME TR also tests for App? here. ie (or (r/Name? old) (App? old))
                    (r/Name? old) ;; must be different, since they're not subtypes 
                    ;; and n must refer to a distinct struct type
                    old
                    (r/Union? old) (let [l (:types old)]
                                     (c/Un (map (fn [e] (remove* e rem opts)) l) opts))
                    (r/Mu? old) (remove* (c/unfold old opts) rem opts)
                    (r/Poly? old) (let [vs (c/Poly-fresh-symbols* old)
                                        b (c/Poly-body* vs old opts)]
                                    (c/Poly* vs 
                                             (c/Poly-bbnds* vs old opts)
                                             (remove* b rem opts)
                                             opts))
                    :else old))]
    (if (sub/subtype? old initial opts) old initial)))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc
  typed.cljc.checker.free-ops
  (:require [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.type-rep :as r]
            [typed.clojure :as t]
            [typed.cljc.checker.tvar-env :as tvar]
            [typed.cljc.checker.tvar-bnds :as bnds])
  (:import (typed.cljc.checker.type_rep F Bounds Regex TypeFn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

;(t/ann free-with-name [t/Sym -> (t/U nil F)])
;(defn free-with-name
;  "Find the free with the actual name name, as opposed to
;  the alias used for scoping"
;  [name]
;  {:pre [(symbol? name)]
;   :post [((some-fn nil? r/F?) %)]}
;  (some (fn> [[_ {{fname :name :as f} :F}] :- '[t/Sym FreeEntry]]
;          (t/ann-form fname t/Sym)
;          (when (= name fname)
;            f))
;        *free-scope*))

(t/ann free-with-name-bnds [t/Sym t/Any -> (t/U nil Bounds Regex TypeFn)])
(defn free-with-name-bnds
  "Find the bounds for the free with the actual name name, as opposed to
  the alias used for scoping"
  [name opts]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Kind?) %)]}
  (bnds/lookup-tvar-bnds name opts))

(t/ann free-in-scope [t/Sym t/Any -> (t/U nil F)])
(defn free-in-scope
  "Find the free scoped as name"
  [name {::tvar/keys [current-tvars] :as opts}]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (get current-tvars name))

(t/ann free-in-scope-bnds [t/Sym t/Any -> (t/U nil (t/U Bounds Regex TypeFn))])
(defn free-in-scope-bnds
  "Find the bounds for the free scoped as name"
  [name opts]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Kind?) %)]}
  (when-let [f (free-in-scope name opts)]
    (bnds/lookup-tvar-bnds (:name f) opts)))

;; slow
(def frees-map? map? #_(con/hash-c? symbol? (con/hmap-c? :F r/F? :bnds (some-fn r/Bounds? r/Regex?))))

; we used to have scopes and bounds in the same map. To avoid changing the interface,
; with-free-mappings now handles frees-map to do scoping and bounds in separate bindings.
;
; Once this works we should use a more consistent interface
;
;frees-map :- '{t/Sym '{:F F :bnds (U Bounds Regex TypeFn)}}
(defn with-free-mappings
  [opts frees-map]
  (let [_ (assert (frees-map? frees-map) frees-map)
        scoped-names (keys frees-map)
        vals (vals frees-map)
        fresh-names (mapv (comp :name :F) vals)
        bndss (mapv :bnds vals)]
    (-> opts
        (tvar/with-extended-new-tvars scoped-names fresh-names)
        (bnds/with-extended-bnds fresh-names bndss))))

(def bounded-frees-key? r/F?)
(def bounded-frees-val? r/Kind?)
;; extremely slow
(def bounded-frees? map? #_(con/hash-c? bounded-frees-key? bounded-frees-val?))

(defn with-bounded-frees
  "Scopes bfrees, a map of instances of F to their bounds, inside body."
  [opts bfrees]
  (let [_ (assert (bounded-frees? bfrees) bfrees)]
    (with-free-mappings opts
      (reduce-kv (fn [m f bnds]
                   (assert (bounded-frees-key? f) (class f))
                   (assert (bounded-frees-val? bnds) (class bnds))
                   (assoc m (:name f) {:F f :bnds bnds}))
                 {} bfrees))))

(defn with-free-symbols
  "Scopes sfrees, a sequence of symbols, inside body as free variables, with default bounds."
  [opts sfrees]
  (with-free-mappings
    opts
    (into {} (map (fn [f]
                    [f {:F (r/F-maker f) :bnds r/no-bounds}]))
          sfrees)))

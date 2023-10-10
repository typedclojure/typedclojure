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
  (:import (typed.cljc.checker.type_rep F Bounds Regex)))

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

(t/ann free-with-name-bnds [t/Sym -> (t/U nil Bounds)])
(defn free-with-name-bnds
  "Find the bounds for the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (bnds/lookup-tvar-bnds name))

(t/ann free-in-scope [t/Sym -> (t/U nil F)])
(defn free-in-scope
  "Find the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (tvar/*current-tvars* name))

(t/ann free-in-scope-bnds [t/Sym -> (t/U nil (t/U Bounds Regex))])
(defn free-in-scope-bnds
  "Find the bounds for the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds? r/Regex?) %)]}
  (when-let [f (free-in-scope name)]
    (bnds/lookup-tvar-bnds (:name f))))

;; slow
(def frees-map? map? #_(con/hash-c? symbol? (con/hmap-c? :F r/F? :bnds (some-fn r/Bounds? r/Regex?))))

; we used to have scopes and bounds in the same map. To avoid changing the interface,
; with-free-mappings now handles frees-map to do scoping and bounds in separate bindings.
;
; Once this works we should use a more consistent interface
;
;frees-map :- '{t/Sym '{:F F :bnds (U Bounds Regex)}}
(defmacro with-free-mappings
  [frees-map & body]
  `(let [frees-map# ~frees-map
         _# (assert (frees-map? frees-map#)
                    frees-map#)
         scoped-names# (keys frees-map#)
         fresh-names# (map (comp :name :F) (vals frees-map#))
         bndss# (map :bnds (vals frees-map#))]
     (tvar/with-extended-new-tvars scoped-names# fresh-names#
       (bnds/with-extended-bnds fresh-names# bndss#
         ~@body))))

;; extremely slow
(def bounded-frees? map? #_(con/hash-c? r/F? (some-fn r/Bounds? r/Regex?)))

(defn with-bounded-frees* [bfrees bfn]
  (let [_ (assert (bounded-frees? bfrees) bfrees)]
    (with-free-mappings (into {} (for [[f bnds] bfrees]
                                   [(:name f) {:F f :bnds bnds}]))
      (bfn))))

(defmacro with-bounded-frees
  "Scopes bfrees, a map of instances of F to their bounds, inside body."
  [bfrees & body]
  `(with-bounded-frees* ~bfrees (fn [] (do ~@body))))

(defn with-frees* [frees bfn]
  (with-free-mappings (into {} (for [f frees]
                                 [(:name f) {:F f :bnds r/no-bounds}]))
    (bfn)))

(defmacro with-frees
  "Scopes frees, which are instances of F, inside body, with
  default bounds."
  [frees & body]
  `(with-frees* ~frees
     (fn [] (do ~@body))))

(defmacro with-free-symbols
  "Scopes sfrees, a sequence of symbols, inside body as free variables, with default bounds."
  [sfrees & body]
  `(with-free-mappings (into {} (for [f# ~sfrees]
                                  [f# {:F (r/F-maker f#) :bnds r/no-bounds}]))
     ~@body))

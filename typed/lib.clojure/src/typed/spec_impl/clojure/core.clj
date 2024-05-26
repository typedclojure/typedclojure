;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.spec-impl.clojure.core
  "Implementation details for typed.spec.clojure.core"
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.spec.clojure.core :as api]
            [clojure.alpha.spec.gen :as gen]))

;; reduced-spec

(defmethod s/expand-spec `api/reduced-spec
  [[_ s & more]]
  {:pre [(not more)]}
  {:clojure.spec/op `api/reduced-spec
   :s s})

(defn- reduced-spec-impl [s gfn]
  (reify
    Spec
    (conform* [_ x settings-key settings]
      (if (reduced? x)
        (let [res (conform* (s/resolve-spec s)
                            @x
                            settings-key
                            settings)]
          (if (s/invalid? res)
            res
            (if (identical? res x)
              x
              (reduced res))))
        ::s/invalid))
    (unform* [_ x]
      (assert nil "TODO")
      )
    (explain* [_ path via in x settings-key settings]
      (assert nil "TODO")
      )
    (gen* [_ overrides path rmap]
      (if gfn
        (gfn)
        (gen/fmap reduced (-> s s/resolve-spec s/gen))))
    (with-gen* [_ gfn]
      (reduced-spec-impl s gfn))
    (describe* [_]
      `(api/reduced-spec ~s))))

(defmethod s/create-spec `api/reduced-spec
  [{:keys [s]}]
  (reduced-spec-impl s nil))

;; atom-spec

(defmethod s/expand-spec `api/atom-spec
  [[_ & {:keys [write read]
         :or {write `any?
              read `any?}}]]
  {:clojure.spec/op `api/atom-spec
   :write write
   :read read})

(def ^:private gen-sized (delay (requiring-resolve 'clojure.test.check.generators/sized)))

(defn- atom-spec-impl [write read gfn]
  (reify
    Spec
    (conform* [_ x settings-key settings]
      (if (and (instance? clojure.lang.IAtom2 x)
               (instance? clojure.lang.IDeref x)
               (s/valid? (-> read s/resolve-spec) @x))
        x
        ::s/invalid))
    (unform* [_ x]
      (assert nil "TODO atom-spec unform"))
    (explain* [_ path via in x settings-key settings]
      (assert nil
              (str "TODO atom-spec explain "
                   {:path path :via via :in in :x x :settings-key settings-key :settings settings})))
    (gen* [_ overrides path rmap]
      (if gfn
        (gfn)
        (gen/fmap (fn [init]
                    ; read is trusted, write is checked
                    (let [a (atom init)
                          deref-impl (fn [] @a)
                          reset!-impl (fn [w]
                                        (assert (s/valid? (-> write s/resolve-spec) w))
                                        (reset! a w))
                          swap!-impl (fn [f & args]
                                       (swap! a
                                              (fn [r]
                                                (let [w (apply f r args)]
                                                  (assert (s/valid? (-> write s/resolve-spec) w))
                                                  w))))
                          swap-vals!-impl (fn [f & args]
                                            (swap-vals! a
                                                        (fn [r]
                                                          (let [w (apply f r args)]
                                                            (assert (s/valid? (-> write s/resolve-spec) w))
                                                            w))))]
                      (reify
                        clojure.lang.IDeref
                        (deref [_] (deref-impl))

                        clojure.lang.IAtom
                        (reset [_ v] (reset!-impl v))

                        (swap [_ f] (swap!-impl f))
                        (swap [_ f a1] (swap!-impl f a1))
                        (swap [_ f a1 a2] (swap!-impl f a1 a2))
                        (swap [_ f a1 a2 args] (apply swap!-impl f a1 a2 args))

                        clojure.lang.IAtom2
                        (swapVals [_ f] (swap-vals!-impl f))
                        (swapVals [_ f a1] (swap-vals!-impl f a1))
                        (swapVals [_ f a1 a2] (swap-vals!-impl f a1 a2))
                        (swapVals [_ f a1 a2 args] (apply swap-vals!-impl f a1 a2 args)))))
                  (-> read s/resolve-spec s/gen))))
    (with-gen* [_ gfn]
      (atom-spec-impl write read gfn))
    (describe* [_]
      `(api/atom-spec :write ~write
                      :read ~read))))

(defmethod s/create-spec `api/atom-spec
  [{:keys [write read]}]
  (atom-spec-impl write read nil))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.spec-impl.clojure.core
  "Implementation details for typed.spec.clojure.core"
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
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



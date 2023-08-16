;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.spec.gen
  "Defines test.check generators for working with
  the spec subtyping lattice."
  (:require [typed.clj.spec.subtype :as sub]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as gen]))

(declare subtype-generator supertype-generator)

;TODO filter out (s/or) results when appropriate
(defn- subtype-generator* [upper direction]
  (let [[subtype-generator direct-subtypes]
        (case direction
          :sub [subtype-generator sub/direct-subtypes]
          :sup [supertype-generator sub/direct-supertypes])]
    (gen/frequency
      (into []
            (remove nil?)
            [(when-some [gs (not-empty
                              ;FIXME how does sorting change with supertype-generator?
                              (sort-by
                                (comp - first)
                                (map (juxt #(get (meta %) ::frequency 1)
                                           gen/return)
                                     (direct-subtypes upper))))]
               [10
                (gen/bind
                  (gen/frequency gs)
                  subtype-generator)])
             [1 (gen/return upper)]]))))

(defn subtype-generator
  "Alpha - subject to change

  Returns a test.check generator that generates all the known
  subtypes of upper, a spec form."
  [upper]
  (subtype-generator* upper :sub))

(defn supertype-generator
  "Alpha - subject to change

  Returns a test.check generator that generates all the known
  supertypes of lower, a spec form."
  [lower]
  (subtype-generator* lower :sup))

;;TODO generate s/and and s/or specs and use overlap + subtype
;; info to simplify

(comment
  (gen/sample (subtype-generator `any?) 100)
  (gen/sample (subtype-generator `(s/fspec :args (s/* (s/or))
                                           :ret any?))
              100)
  (gen/sample (subtype-generator `(s/cat))
              100)
  (gen/sample (subtype-generator `(s/cat :a any?))
              100)
  (gen/sample (subtype-generator `(s/* any?))
              100)
  (gen/sample (subtype-generator `(s/? any?))
              100)
  (gen/sample (subtype-generator `map?) 100)
  (gen/sample (supertype-generator `map?) 100)
  )

; s/cat
(comment
  (mapv (comp s/describe s/resolve-spec)
        (gen/sample (subtype-generator `(s/cat :a any?))
                    100))
  (mapv (comp s/describe s/resolve-spec)
        (gen/sample (subtype-generator `(s/cat :a any?
                                               :b boolean?))
                    100))
  )

; s/?
(comment
  (mapv (comp s/describe s/resolve-spec)
        (gen/sample (subtype-generator `(s/? any?))
                    100))
  )

; s/*
(comment
  (mapv (comp s/describe s/resolve-spec)
        (gen/sample (subtype-generator `(s/* any?))
                    100))
  )

(ns ^:typed.clojure typed-test.clj.spec.subtype
  (:require [typed.clj.spec.subtype :as sub]
            [clojure.alpha.spec :as s]))

;TODO turn into tests
(do
  (sub/direct-subtypes `true?)
  (sub/direct-subtypes `any?)
  (sub/direct-subtypes `map?)
  (sub/direct-supertypes `nil?)
  (sub/direct-subtypes `integer?)
  (sub/direct-subtypes `int?)
  (sub/direct-subtypes `nat-int?)

  (sub/direct-subtypes `map?)
  (sub/direct-subtypes `(s/map-of any? any?))
  (sub/direct-supertypes `(s/map-of any? any?))
  (sub/direct-supertypes `(s/map-of some? any?))
  (sub/direct-supertypes `(s/map-of some? some?))
  (sub/direct-supertypes `(s/tuple some?))
  (sub/direct-supertypes `(s/tuple))
  (sub/direct-subtypes `(s/tuple))
  (sub/direct-subtypes `some?)
  (sub/direct-subtypes `any?)
        )

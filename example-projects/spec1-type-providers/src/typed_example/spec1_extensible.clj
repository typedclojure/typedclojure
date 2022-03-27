(ns typed-example.spec1-extensible
  (:require [typed.clojure :as t]
            [clojure.spec.alpha :as s]))

;; here's some new spec1 schemas. we need to teach Typed Clojure what it means.
;; the namespace typed-example.register-spec1-extensions does this via `register-spec-syntax->type-extension`.
;; Typed Clojure will automatically load this namespace during checking since it's
;; added to the src/typedclojure_config.cljc file in the :spec1-extensions entry.
(t/tc-ignore
  (s/def ::over3
    #(and (pos-int? %) (< % 3)))
  (s/def ::over4
    #(and (pos-int? %) (< % 4))))

;; ::over3/::over4 schemas are registered in typed-example.register-spec1-extensions
(s/fdef foo :args (s/cat :t ::over3) :ret ::over4)
(defn foo [t] (inc t))

(foo 1)

(comment
  (t/check-ns-clj)
  )

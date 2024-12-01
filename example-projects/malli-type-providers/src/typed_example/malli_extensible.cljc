(ns typed-example.malli-extensible
  (:require [typed.clojure :as-alias t]
            [malli.core :as m]))

;; here's a new malli schema. we need to teach Typed Clojure what it means.
;; the namespace typed-example.register-malli-extensions does this via `register-malli->type-extension`.
;; Typed Clojure will automatically load this namespace during checking since it's
;; added to the src/typedclojure_config.cljc file in the :malli-extensions entry.
^::t/ignore
(def Over
  (m/-simple-schema
    {:compile
     (fn [{:keys [value]} _ _]
       (assert (int? value))
       {:type ::over
        :pred #(and (int? %) (> % value))
        :type-properties {:error/fn (fn [error _] (str "should be over " value ", was " (:value error)))}})}))

;; [Over ...] schemas are of m/type ::over, which is registered in typed-example.register-malli-extensions
(m/=> foo [:=> [:cat [Over {:value 0}]] [Over {:value 1}]])
(defn foo [t] (inc t))

(foo 1)

(comment
  (t/check-ns-clj)
  )

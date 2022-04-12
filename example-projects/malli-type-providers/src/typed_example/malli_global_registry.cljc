(ns typed-example.malli-global-registry
  "How to use Malli's default global registry as a type
  provider to Typed Clojure."
  (:require [malli.core :as m]
            [malli.generator :as mg]
            [malli.util :as mu]
            [malli.registry :as mr]
            [typed.clojure :as t]))

;; set up a global registry. Typed Clojure evaluates each form in turn, so make
;; sure the registry is set before code that uses it is checked.
(t/tc-ignore
(def my-schema
  {::string string?
   ::some-schema
   [:map
    [:key1 string?]]

   ::some-other-schema
   [:map
    [:key2 [:ref ::some-schema]]]})

(mr/set-default-registry! (mr/composite-registry (mr/schemas m/default-registry)
                                                 (mu/schemas)
                                                 my-schema))
)

(m/=> my-func [:=> [:cat ::string] ::some-other-schema])
(defn my-func [arg]
  ;; input sanity check
  (t/ann-form arg t/Str) ;; malli ::string has been converted to t/Str
  {:key2 {:key1 arg}})

;; output sanity check
(if (-> "a" my-func :key2 :key1 string?)
  :yes
  ;; Typed Clojure deems this branch unreachable. only possible if Typed Clojure
  ;; used the Malli global registry to check this---ie., it works!
  (inc 'no))

(comment
  (t/check-ns-clj)
  )

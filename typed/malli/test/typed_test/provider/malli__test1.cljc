(ns ^:typed.clojure typed-test.provider.malli__test1
  (:require [malli.core :as m]
            [typed.clojure :as t]))

(m/=> foo [:=> [:cat :int] :int])
(defn foo [t] (inc t))

(def External :int)
(m/=> external [:=> [:cat External] :int])
(defn external [t] (inc t))

;; t/ann overrides m/=>
(m/=> choose-tc [:=> [:cat :nil] :int])
(t/ann choose-tc [t/AnyInteger :-> t/AnyInteger])
(defn choose-tc [t] (inc t))

^::t/ignore
(def PokemonLevel
  [:and :int [:>= 1] [:<= 100]])

(m/=> get-pokemon-level [:=> [:cat [:maybe PokemonLevel]] PokemonLevel])
(defn get-pokemon-level [pokemon-level]
  (or pokemon-level 1))

(comment
  (t/check-ns-clj)
  (t/check-ns-cljs)
  (m/function-schemas)
  )

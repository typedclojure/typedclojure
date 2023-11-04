(ns typed-example.clj-kondo-hooks
  (:require [clojure.core.typed :as ct]
            [typed.clojure :as t]))

#(t/ann-form (inc :expected-failure)
             (inc :unexpected-failure))
#(ct/ann-form (inc :expected-failure)
              (inc :unexpected-failure))

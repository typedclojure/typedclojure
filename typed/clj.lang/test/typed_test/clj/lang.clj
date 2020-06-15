(ns typed-test.clj.lang
  (:require [typed.clj.lang :as lang]
            [clojure.test :refer :all]))

(deftest stub
  (is (map? lang/lang-dispatch)))

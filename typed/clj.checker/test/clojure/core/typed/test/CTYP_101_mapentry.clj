(ns clojure.core.typed.test.CTYP-101-mapentry
  (:require [typed.clojure :as t :refer [ann]]))

(ann f [(t/Map t/Keyword t/Int) -> (t/Option (t/MapEntry t/Keyword t/Int))])
(defn f [m] (first m))

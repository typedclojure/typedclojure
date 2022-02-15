(ns cljs.core.typed.test.ann
  (:require [cljs.core.typed :as t :refer-macros [ann check-ns]]))

(ann foo cljs.core.typed/Num)
(def foo 1)

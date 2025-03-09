(ns ^:typed.clojure/ignore cycle-detection.main
  (:require [clojure.core.typed :as t]))

(t/ann foo [:-> t/Str])

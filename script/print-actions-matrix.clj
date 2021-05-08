#!/usr/bin/env bb

(require '[cheshire.core :as json])

(def all-submodules
  ["typed/cljc.analyzer"
   "typed/cljs.analyzer"
   "typed/clj.analyzer"
   #_"typed/cljs.checker"
   "typed/clj.checker"
   "typed/clj.reader"
   "typed/clj.refactor"
   "typed/clj.runtime"
   "typed/clj.annotator"
   "typed/clj.lang"
   "typed/clj.spec"
   "typed/lib.clojure"
   "typed/lib.core.async"])

(def clojure-stable "1.10.3")
(def clojure-next "1.11.0-master-SNAPSHOT")

(defn push-matrix []
  {:include (for [submodule all-submodules
                  clojure [clojure-stable
                           clojure-next]
                  jdk ["1.11"]]
              {:submodule submodule
               :clojure clojure
               :jdk jdk}) })

(defn schedule-matrix []
  {:include (for [submodule all-submodules
                  clojure [clojure-stable
                           clojure-next]
                  jdk ["1.8"
                       "1.11"
                       "1.15"]]
              {:submodule submodule
               :clojure clojure
               :jdk jdk})})

(defn matrix []
  (if (= "schedule" (System/getenv "GITHUB_EVENT_NAME"))
    (schedule-matrix)
    (push-matrix)))

(println
  (str "::set-output name=matrix::"
       (json/generate-string (matrix) {:pretty false})))

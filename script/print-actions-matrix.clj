#!/usr/bin/env bb

;; GITHUB_EVENT_NAME=schedule ./script/print-actions-matrix.clj
;; GITHUB_EVENT_NAME=push ./script/print-actions-matrix.clj

(require '[cheshire.core :as json]
         '[clojure.core.typed.contract-utils :as con])

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
(def clojure-next-alpha "1.11.0-alpha1")
(def clojure-next-snapshot "1.11.0-master-SNAPSHOT")

(def matrix? (con/hmap-c? :include (con/every-c? (con/hmap-c?
                                                   :submodule string?
                                                   :clojure string?
                                                   :jdk string?))))

(defn push-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule all-submodules
                  clojure [clojure-stable
                           clojure-next-alpha]
                  jdk ["11"]]
              {:submodule submodule
               :clojure clojure
               :jdk jdk})})

(defn schedule-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule all-submodules
                  clojure [clojure-stable
                           clojure-next-snapshot]
                  jdk ["8"
                       "11"
                       "16"]]
              {:submodule submodule
               :clojure clojure
               :jdk jdk})})

(defn matrix []
  {:post [(matrix? %)]}
  (if (= "schedule" (System/getenv "GITHUB_EVENT_NAME"))
    (schedule-matrix)
    (push-matrix)))

(println
  (str "::set-output name=matrix::"
       (json/generate-string (matrix) {:pretty false})))

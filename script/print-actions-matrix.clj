#!/usr/bin/env bb

;; GITHUB_EVENT_NAME=schedule ./script/print-actions-matrix.clj
;; GITHUB_EVENT_NAME=push ./script/print-actions-matrix.clj

(require '[cheshire.core :as json]
         '[clojure.core.typed.contract-utils :as con]
         '[clojure.java.io :as io]
         '[clojure.set :as set])

(def all-testable-submodules
  (let [all-submodules (into (sorted-set)
                             (keep (fn [^java.io.File f]
                                     (when (.isDirectory f)
                                       (.getPath f))))
                             (.listFiles (io/file "typed")))
        exclusions #{}]
    (set/difference
      all-submodules
      exclusions)))

(def clojure-stable "1.10.3")
(def clojure-next-release "1.11.0-rc1")
(def clojure-next-snapshot "1.11.0-master-SNAPSHOT")

(def matrix? (con/hmap-c? :include (con/every-c? (con/hmap-c?
                                                   :submodule string?
                                                   :clojure string?
                                                   :jdk string?))))

(defn push-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule all-testable-submodules
                  clojure (cond-> [clojure-stable]
                            (= "typedclojure/typedclojure"
                               (System/getenv "GITHUB_REPOSITORY"))
                            (conj clojure-next-release))
                  jdk ["11"]]
              {:submodule submodule
               :clojure clojure
               :jdk jdk})})

(defn schedule-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule all-testable-submodules
                  clojure [clojure-stable
                           clojure-next-snapshot]
                  jdk ["8"
                       "11"
                       "17"]]
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

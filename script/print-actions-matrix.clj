#!/usr/bin/env bb

;; GITHUB_EVENT_NAME=schedule ./script/print-actions-matrix.clj
;; GITHUB_EVENT_NAME=push ./script/print-actions-matrix.clj

(require '[cheshire.core :as json]
         '[clojure.core.typed.contract-utils :as con]
         '[clojure.java.io :as io]
         '[clojure.set :as set]
         '[clojure.string :as str])

(def all-testable-submodules
  (into (sorted-set)
        (keep (fn [^java.io.File f]
                (when (.isDirectory f)
                  (.getPath f))))
        (concat (.listFiles (io/file "typed"))
                (.listFiles (io/file "example-projects")))))

;; TODO grab from typed.dev.helpers
(def clojure-stable "1.11.0")
(def clojure-next-release nil #_"1.12.0-alpha1")
(def clojure-next-snapshot "1.12.0-master-SNAPSHOT")

(def matrix? (con/hmap-c? :include (con/every-c? (con/hmap-c?
                                                   :submodule string?
                                                   :clojure string?
                                                   :jdk string?))))

(def slow-submodule-tests #{"typed/clj.checker"
                            "typed/clj.spec"
                            "typed/malli"
                            "typed/lib.clojure"})

(defn submodule-batches []
  (let [{slow-modules true fast-modules false} (group-by (comp boolean slow-submodule-tests)
                                                         all-testable-submodules)]
    (concat (map vector slow-modules)
            (partition-all 6 fast-modules))))

(defn push-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule (submodule-batches)
                  clojure (cond-> [clojure-stable]
                            (and (= "typedclojure/typedclojure"
                                    (System/getenv "GITHUB_REPOSITORY"))
                                 clojure-next-release)
                            (conj clojure-next-release))
                  jdk ["11"]]
              {:submodule (str/join " " submodule)
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

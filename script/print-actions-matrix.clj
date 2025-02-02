#!/usr/bin/env bb

;; GITHUB_EVENT_NAME=schedule ./script/print-actions-matrix.clj
;; GITHUB_EVENT_NAME=push ./script/print-actions-matrix.clj

(require '[cheshire.core :as json]
         '[clojure.core.typed.contract-utils :as con]
         '[clojure.java.io :as io]
         '[clojure.set :as set]
         '[clojure.string :as str])
(import '[java.util UUID])

(def all-testable-submodules
  (into (sorted-set)
        (keep (fn [^java.io.File f]
                (when (.isDirectory f)
                  (when-not (str/starts-with? (.getName f) ".") ;; remove hidden dirs
                    (.getPath f)))))
        (concat (.listFiles (io/file "typed"))
                (.listFiles (io/file "example-projects")))))

;; TODO grab from typed.dev.helpers
(def clojure-stable "1.12.0")
(def clojure-next-release nil #_"1.12.0-alpha1")
(def clojure-next-snapshot "1.13.0-master-SNAPSHOT")

(def matrix? (con/hmap-c? :include (con/every-c? (con/hmap-c?
                                                   :submodule_hash string?
                                                   :submodule string?
                                                   :clojure string?
                                                   :jdk string?))))

(def slow-submodule-tests #{"typed/clj.checker"
                            "typed/clj.spec"
                            "typed/malli"
                            "typed/lib.clojure"})

(defn submodule-batches []
  (let [;; analyzers use a different clojure version
        {analyzers true all-latest-clojure-submodules false} (group-by #(str/includes? % "analyzer") all-testable-submodules)
        {slow-modules true fast-modules false} (group-by (comp boolean slow-submodule-tests)
                                                         all-latest-clojure-submodules)
        _ (assert (= slow-submodule-tests (set slow-modules)))
        ;slow-splits (partition-all 2 slow-modules)
        ;; clj.checker and clj.spec are slowest
        ;slow-splits [["typed/clj.checker" "typed/malli"]
        ;             ["typed/clj.spec" "typed/lib.clojure"]]
        slow-splits [slow-modules]
        _ (assert (= (sort slow-submodule-tests) (sort (mapcat identity slow-splits))))
        ;fast-splits (split-at (quot (count fast-modules) 2) fast-modules)
        fast-splits [fast-modules analyzers]
        _ (let [expected-fast-splits-modules (sort (concat fast-modules analyzers))
                actual-fast-splits-modules (sort (mapcat identity fast-splits))]
            (assert (= expected-fast-splits-modules actual-fast-splits-modules)
                    [expected-fast-splits-modules actual-fast-splits-modules]))
        all-splits (concat slow-splits fast-splits)
        _ (assert (= (sort all-testable-submodules) (sort (mapcat identity all-splits))))]
    all-splits))

(defn submodule-hash [^String submodule]
  (str/replace (UUID/nameUUIDFromBytes (.getBytes submodule)) #"-" ""))

(defn push-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule-batch (submodule-batches)
                  :let [submodule (str/join " " submodule-batch)]
                  clojure (cond-> [clojure-stable]
                            (and (= "typedclojure/typedclojure"
                                    (System/getenv "GITHUB_REPOSITORY"))
                                 clojure-next-release)
                            (conj clojure-next-release)

                            (str/includes? submodule "analyzer")
                            (conj "1.9.0"))
                  jdk ["11"]]
              (array-map
                :submodule submodule
                :clojure clojure
                :jdk jdk
                ;; put last so the job title is readable
                :submodule_hash (submodule-hash submodule)))})

(defn schedule-matrix []
  {:post [(matrix? %)]}
  {:include (for [submodule all-testable-submodules
                  clojure (cond-> [clojure-stable
                                   clojure-next-snapshot]
                            (str/includes? submodule "analyzer")
                            (conj "1.9.0" "1.10.1"))
                  jdk ["8"
                       "11"
                       "17"
                       "21"]]
              (array-map
                :submodule submodule
                :clojure clojure
                :jdk jdk
                ;; put last so the job title is readable
                :submodule_hash (submodule-hash submodule)))})

(defn matrix []
  {:post [(matrix? %)]}
  (if (= "schedule" (System/getenv "GITHUB_EVENT_NAME"))
    (schedule-matrix)
    (push-matrix)))

(spit (System/getenv "GITHUB_OUTPUT")
      (str "matrix=" (json/generate-string (matrix) {:pretty false}) "\n"))

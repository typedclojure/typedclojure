(ns typed.dev.helpers
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^String repo-root (if (.exists (io/file "CODE_OF_CONDUCT.md")) "." ".."))

(defn typedclojure-stable-version []
  (-> (str repo-root "/stable-version")
      slurp
      str/trim))

(defn typedclojure-stable-sha []
  {:post [(= 40 (count %))]}
  (-> (str repo-root "/stable-sha")
      slurp
      str/trim))

(defn typedclojure-current-version []
  (-> (str repo-root "/current-version")
      slurp
      str/trim))

;; add new deps to `dev/resources/root-templates/dependabot/pom.xml` for dependabot support
(def selmer-input-map
  {:clojure-mvn-version "1.12.3"
   :clojure-next-release-mvn-version nil #_"1.12.0-alpha1"
   :clojure-next-snapshot-mvn-version "1.13.0-master-SNAPSHOT"
   :min-supported-jdk "21"
   :analyzer-clojure-mvn-version "1.9.0"
   :clojars-cdn-url "https://repo.clojars.org"
   :clojars-url "https://clojars.org/repo"
   :typedclojure-scm-connection "scm:git:https://github.com/typedclojure/typedclojure.git"
   ;:typedclojure-scm-developerConnection "scm:git:git@github.com:typedclojure/typedclojure.git"
   :typedclojure-scm-developerConnection "scm:git:https://github.com/typedclojure/typedclojure.git"
   :typedclojure-git-https-url "https://github.com/typedclojure/typedclojure"
   :typedclojure-group-id "org.typedclojure"
   :selmer-mvn-version "1.12.62"
   :core-memoize-mvn-version "1.2.281"
   :nrepl-mvn-version "1.3.1"
   :reply-mvn-version "0.5.1"
   :parsley-mvn-version "0.9.3"
   :piggieback-mvn-version "0.6.0"
   :tools-namespace-mvn-version "1.5.1"
   :cider-nrepl-mvn-version "0.52.1"
   :asm-mvn-version "9.8"
   :tools-analyzer-jvm-mvn-version "1.3.4"
   :math-combinatorics-mvn-version "0.3.2"
   :tools-reader-mvn-version "1.6.0"
   :core-cache-mvn-version "1.1.234"
   :core-async-mvn-version "1.7.701"
   :test-check-mvn-version "1.1.3"
   :test-chuck-mvn-version "0.2.12"
   :cognitect-test-runner-coordinates "io.github.cognitect-labs/test-runner"
   :cognitect-test-runner-git-url "https://github.com/cognitect-labs/test-runner"
   :cognitect-test-runner-sha "dd6da11611eeb87f08780a30ac8ea6012d4c05ce"
   :codox-mvn-version "0.10.8"
   :kaocha-git-url "https://github.com/lambdaisland/kaocha.git"
   :kaocha-sha "8846f91c9bf4338c561ffb866b5a8890e22889cd"
   :typedclojure-homepage "https://typedclojure.org"
   :malli-mvn-version "0.20.1"
   :beholder-mvn-version "1.0.3"
   :process-mvn-version "0.5.22"
   :fs-mvn-version "0.5.33"
   :fully-satisfies-mvn-version "1.12.0"
   :fully-satisfies-bb-sha "37149ae490af63071a2d92e730f22324e7835f24"
   :clj-async-profiler-mvn-version "1.6.1"
   :javaparser-mvn-version "3.26.1"
   :graal-build-time-mvn-version "1.0.5"
   })

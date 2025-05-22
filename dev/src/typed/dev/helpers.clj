(ns typed.dev.helpers
  (:require [clojure.string :as str]))

(def ^String repo-root "..")

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

(def selmer-input-map
  {:clojure-mvn-version "1.12.0"
   :min-supported-jdk "1.8"
   :analyzer-clojure-mvn-version "1.9.0"
   :clojars-url "https://clojars.org/repo"
   :sonatype-oss-public-url "https://oss.sonatype.org/content/groups/public/"
   :clojurescript-mvn-version "1.11.132"
   :typedclojure-alpha-spec-mvn-version "0.2.177-typedclojure-2"
   :typedclojure-git-https-url "https://github.com/typedclojure/typedclojure"
   :typedclojure-alpha-spec-git-sha "9da58ec60f5a4a3bfc61fa19f54bf1d160b49dfc"
   :typedclojure-group-id "org.typedclojure"
   :selmer-mvn-version "1.12.62"
   :core-memoize-mvn-version "1.1.266"
   :nrepl-mvn-version "1.3.1"
   :reply-mvn-version "0.5.1"
   :parsley-mvn-version "0.9.3"
   :piggieback-mvn-version "0.6.0"
   :tools-namespace-mvn-version "1.5.0"
   :cider-nrepl-mvn-version "0.52.1"
   :asm-mvn-version "9.2"
   :tools-analyzer-mvn-version "1.2.0"
   :tools-analyzer-jvm-mvn-version "1.3.2"
   :math-combinatorics-mvn-version "0.2.0"
   :tools-reader-mvn-version "1.5.0"
   :core-cache-mvn-version "1.1.234"
   :core-async-mvn-version "1.7.701"
   :test-check-mvn-version "1.1.1"
   :test-chuck-mvn-version "0.2.12"
   :core-logic-mvn-version "1.1.0"
   :cognitect-test-runner-coordinates "io.github.cognitect-labs/test-runner"
   :cognitect-test-runner-git-url "https://github.com/cognitect-labs/test-runner"
   :cognitect-test-runner-sha "dd6da11611eeb87f08780a30ac8ea6012d4c05ce"
   :codox-mvn-version "0.10.8"
   :kaocha-git-url "https://github.com/lambdaisland/kaocha.git"
   :kaocha-sha "fcef91c365caa0f6fed6761b3deb80e800ccb617"
   :typedclojure-homepage "https://typedclojure.org"
   :malli-mvn-version "0.17.0"
   :beholder-mvn-version "1.0.2"
   :process-mvn-version "0.5.22"
   :fs-mvn-version "0.5.24"
   :fully-satisfies-mvn-version "1.12.0"
   :clj-async-profiler-mvn-version "1.6.1"
   })

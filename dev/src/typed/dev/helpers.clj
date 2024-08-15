(ns typed.dev.helpers
  (:require [clojure.string :as str]))

(def ^String repo-root "..")

(defn typedclojure-stable-version []
  (-> (str repo-root "/stable-version")
      slurp
      str/trim))

(defn typedclojure-current-version []
  (-> (str repo-root "/current-version")
      slurp
      str/trim))

(def selmer-input-map
  {:clojure-mvn-version "1.12.0-rc1"
   :min-supported-jdk "1.8"
   :analyzer-clojure-mvn-version "1.9.0"
   :clojars-url "https://clojars.org/repo"
   :sonatype-oss-public-url "https://oss.sonatype.org/content/groups/public/"
   :clojurescript-mvn-version "1.11.60"
   :typedclojure-alpha-spec-mvn-version "0.2.177-typedclojure-2"
   :typedclojure-git-https-url "https://github.com/typedclojure/typedclojure"
   :typedclojure-alpha-spec-git-sha "9da58ec60f5a4a3bfc61fa19f54bf1d160b49dfc"
   :typedclojure-group-id "org.typedclojure"
   :selmer-mvn-version "1.12.50"
   :core-memoize-mvn-version "1.0.257"
   :nrepl-mvn-version "1.1.1"
   :reply-mvn-version "0.5.1"
   :parsley-mvn-version "0.9.3"
   :piggieback-mvn-version "0.5.3"
   :tools-namespace-mvn-version "1.4.4"
   :cider-nrepl-mvn-version "0.47.0"
   :asm-mvn-version "9.2"
   :tools-analyzer-mvn-version "1.1.1"
   :tools-analyzer-jvm-mvn-version "1.2.3"
   :potemkin-mvn-version "0.4.6"
   :math-combinatorics-mvn-version "0.2.0"
   :tools-reader-mvn-version "1.4.1"
   :core-cache-mvn-version "1.0.225"
   :core-async-mvn-version "1.6.681"
   ;; FIXME clj.checker fails on higher
   :tools-nrepl-mvn-version "0.2.6"
   :test-check-mvn-version "1.1.1"
   :test-chuck-mvn-version "0.2.12"
   :core-logic-mvn-version "1.0.1"
   :cognitect-test-runner-coordinates "io.github.cognitect-labs/test-runner"
   :cognitect-test-runner-git-url "https://github.com/cognitect-labs/test-runner"
   :cognitect-test-runner-sha "dd6da11611eeb87f08780a30ac8ea6012d4c05ce"
   :codox-mvn-version "0.10.7"
   :kaocha-git-url "https://github.com/lambdaisland/kaocha.git"
   :kaocha-sha "24e2e02d0848160343a6f9ac629f0f8e27a682ea"
   :typedclojure-homepage "https://typedclojure.org"
   :malli-mvn-version "0.13.0"
   :beholder-mvn-version "1.0.2"
   :process-mvn-version "0.5.21"
   :fully-satisfies-mvn-version "1.12.0"
   :clj-async-profiler-mvn-version "1.2.2"
   })

(ns typed.dev.helpers
  (:require [clojure.string :as str]))

(def repo-root "..")

(defn typedclojure-stable-version []
  (-> (str repo-root "/stable-version")
      slurp
      str/trim))

(defn typedclojure-current-version []
  (-> (str repo-root "/current-version")
      slurp
      str/trim))

(def selmer-input-map
  {:clojure-mvn-version "1.10.3"
   :clojars-url "https://clojars.org/repo"
   :sonatype-oss-public-url "https://oss.sonatype.org/content/groups/public/"
   :clojurescript-mvn-version "1.10.866"
   :typedclojure-alpha-spec-mvn-version "0.2.177-typedclojure-2"
   :typedclojure-git-https-url "https://github.com/typedclojure/typedclojure"
   :typedclojure-alpha-spec-git-sha "9da58ec60f5a4a3bfc61fa19f54bf1d160b49dfc"
   :typedclojure-group-id "org.typedclojure"
   :core-memoize-mvn-version "1.0.236"
   :nrepl-mvn-version "0.8.3"
   :tools-namespace-mvn-version "1.1.0"
   :asm-mvn-version "5.2"
   :tools-analyzer-mvn-version "1.0.0"
   :tools-analyzer-jvm-mvn-version "1.1.0"
   :potemkin-mvn-version "0.4.5"
   :math-combinatorics-mvn-version "0.1.6"
   :tools-reader-mvn-version "1.3.6"
   :core-cache-mvn-version "1.0.207"
   :core-async-mvn-version "1.3.618"
   ;; FIXME clj.checker fails on higher
   :tools-nrepl-mvn-version "0.2.6"
   :test-check-mvn-version "1.1.0"
   :core-logic-mvn-version "1.0.0"
   :cognitect-test-runner-coordinates "io.github.cognitect-labs/test-runner"
   :cognitect-test-runner-git-url "https://github.com/cognitect-labs/test-runner"
   :cognitect-test-runner-sha "5557e92c932f024127762ec6007d849bec869c8f"
   :codox-mvn-version "0.10.7"
   :kaocha-git-url "https://github.com/lambdaisland/kaocha.git"
   ;; https://github.com/lambdaisland/kaocha/issues/208#issuecomment-822844384
   :kaocha-sha "3d834940d420d6490a2290f809526fff07ef7775"
   :typedclojure-homepage "https://typedclojure.org"
   })

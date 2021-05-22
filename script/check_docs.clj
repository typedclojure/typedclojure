#!/usr/bin/env bb

(ns check-docs
  (:require [babashka.curl :as curl]))

(defn check-api-typedclojure-org [{:keys [curl-get]}]
  (let [response (curl-get "https://api.typedclojure.org/latest/typed.clj.analyzer/index.html")]
    (assert (= 200 (:status response))
            (pr-str response)))
  :ok)

(defn -main [& args]
  (check-api-typedclojure-org {:curl-get curl/get}))

(when (= *file* (System/getProperty "babashka.file")) (-main))

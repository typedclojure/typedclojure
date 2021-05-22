#!/usr/bin/env bb

(ns check-docs
  (:require [babashka.curl :as curl]))

(let [response (curl/get "https://api.typedclojure.org/latest/typed.clj.analyzer/index.html")]
  (assert (= 200 (:status response))
          (pr-str response)))

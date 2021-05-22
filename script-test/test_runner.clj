#!/usr/bin/env bb

(ns test-runner
  (:require [clojure.test :as t]
            [clojure.set :as set])
  (:import [java.io File]))

(def test-namespaces
  '{"script-test/check_docs_test.clj" check-docs-test})

(let [fs (into #{}
               (keep (fn [^File f]
                       (when (.isFile f)
                         (.getPath f))))
               (file-seq (File. "script-test")))
      exclusions #{"script-test/test_runner.clj"}
      missing (set/difference fs (into exclusions (keys test-namespaces)))]
  (assert (empty? missing)
          (str "Don't forget to add these namespaces to script-test/test_runner.clj! "
               (pr-str missing))))

(apply require (vals test-namespaces))                  

(def test-results
  (apply t/run-tests (vals test-namespaces)))

(def failures-and-errors
  (let [{:keys [fail error]} test-results]
    (+ fail error)))

(System/exit (min 1 failures-and-errors))

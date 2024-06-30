(ns profile
  (:require [clj-async-profiler.core :as prof]
            [clojure.core.typed.test.self-check-tc :as self-check]))

(defn run-self-check-for [seconds]
  (let [deadline (+ (System/currentTimeMillis) (* 1000 seconds))]
    (while (< (System/currentTimeMillis) deadline)
      (with-out-str (self-check/check-tc)))))

(def transforms
  [;; Hide JIT compialtion from profile
   {:type :remove
    :what ";CompileBroker::compiler_thread_loop;"}
   ;; Collapse recursive check-expr
   {:type :replace
    :what #";(typed\.clj\.checker\.check/check-expr;).*\1"
    :replacement ";$1"}
   ;; Extract read+string into separate tree
   {:type :replace
    :what #".+(clojure\.tools\.reader/read\+string;)"
    :replacement ";$1"}
   ;; Extract run-pre-passes into separate tree
   {:type :replace
    :what #".+(typed\.cljc\.analyzer/run-pre-passes;)"
    :replacement ";$1"}
   ;; Extract run-post-passes into separate tree
   {:type :replace
    :what #".+(typed\.cljc\.analyzer/run-post-passes;)"
    :replacement ";$1"}
   ;; Extract analyze-outer into separate tree
   {:type :replace
    :what #".+(typed\.cljc\.analyzer/analyze-outer;)"
    :replacement ";$1"}
   ;; Collapse recursive subtypeA*
   {:type :replace
    :what #";(typed\.clj\.checker\.subtype/subtypeA\*;).*\1"
    :replacement ";$1"}
   ;; Extract subtypeA* into separate tree
   {:type :replace
    :what #".+(typed\.clj\.checker\.subtype/subtypeA\*;)"
    :replacement ";$1"}
   ;; Collapse recursive RClass-supers*
   {:type :replace
    :what #";(typed\.cljc\.checker\.type-ctors/RClass-supers\*;).*\1"
    :replacement ";$1"}
   ;; Extract force-type into separate tree
   {:type :replace
    :what #".+(typed\.cljc\.runtime\.env-utils/force-type;)"
    :replacement ";$1"}])

(defn -main [& [duration-in-secs event]]
  (let [secs (parse-long duration-in-secs)
        event (keyword event)]
    (case event
      (:cpu :alloc)
      (do (println "Warming up...")
          (run-self-check-for 10)
          (prof/profile {:event event, :predefined-transforms transforms}
            (run-self-check-for secs)))))
  (shutdown-agents))

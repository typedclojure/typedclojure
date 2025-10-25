#!/usr/bin/env bb
;; Git coordinator - long-running process that serializes git operations
;; to prevent race conditions in concurrent git clones

(require '[babashka.process :as p]
         '[clojure.java.io :as io])

(def lock-file "/tmp/git-coordinator.lock")
(def socket-file "/tmp/git-coordinator.sock")
(def real-git "/usr/bin/git")

(defn ensure-real-git-exists []
  (when-not (.exists (io/file real-git))
    (binding [*out* *err*]
      (println "ERROR: Real git not found at" real-git))
    (System/exit 1)))

(defn serialize-git-call [args]
  "Execute git with the given args, serialized via a lock file"
  (let [lock (io/file lock-file)]
    ;; Acquire lock by creating lock file atomically
    (loop [attempts 0]
      (if (.createNewFile lock)
        (try
          ;; Execute real git
          (let [result (p/shell {:out :string
                                  :err :string
                                  :continue true}
                                real-git args)]
            ;; Print output
            (print (:out result))
            (binding [*out* *err*]
              (print (:err result)))
            (flush)
            ;; Return exit code
            (:exit result))
          (finally
            ;; Release lock
            (.delete lock)))
        ;; Lock not acquired, wait and retry
        (do
          (Thread/sleep 100)
          (when (> attempts 100)
            (binding [*out* *err*]
              (println "ERROR: Timeout waiting for git lock"))
            (System/exit 1))
          (recur (inc attempts)))))))

(defn start-server []
  "Start server that listens for git commands and serializes them"
  (ensure-real-git-exists)
  (binding [*out* *err*]
    (println "[git-coordinator] Starting server..."))
  
  ;; Simple implementation: just keep running and let the wrapper handle locking
  ;; We use the lock file mechanism directly
  (loop []
    (Thread/sleep 1000)
    (recur)))

;; Main entry point
(when (= "server" (first *command-line-args*))
  (start-server))

;; If called as a git wrapper, serialize the call
(when-not (= "server" (first *command-line-args*))
  (System/exit (serialize-git-call (into-array String *command-line-args*))))

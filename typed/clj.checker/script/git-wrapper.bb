#!/usr/bin/env bb
;; Git wrapper that calls the coordinator to serialize git operations

(require '[babashka.process :as p]
         '[clojure.java.io :as io])

(def real-git "/usr/bin/git")
(def lock-file "/tmp/git-coordinator.lock")

(defn is-clone-command? [args]
  "Check if this is a git clone command"
  (and (seq args)
       (= "clone" (first args))))

(defn get-clone-target [args]
  "Extract target directory from git clone command"
  (when (is-clone-command? args)
    ;; Last argument is typically the target directory
    (last args)))

(defn serialize-git-call [args]
  "Execute git with the given args, serialized via a lock file"
  (let [lock (io/file lock-file)]
    ;; Acquire lock by creating lock file atomically
    (loop [attempts 0]
      (if (.createNewFile lock)
        (try
          ;; For clone operations, check if target already exists
          (if (and (is-clone-command? args)
                   (let [target (get-clone-target args)]
                     (and target (.exists (io/file target)))))
            ;; Target exists, another process already cloned it
            (do
              (binding [*out* *err*]
                (println "[git-wrapper] Target already exists, skipping clone"))
              0)
            ;; Execute real git
            (let [cmd (concat [real-git] args)
                  result (apply p/shell {:out :inherit
                                         :err :inherit
                                         :continue true}
                                cmd)]
              (:exit result)))
          (finally
            ;; Release lock
            (.delete lock)))
        ;; Lock not acquired, wait and retry
        (do
          (Thread/sleep 50)
          (when (> attempts 200)
            (binding [*out* *err*]
              (println "ERROR: Timeout waiting for git lock"))
            (System/exit 1))
          (recur (inc attempts)))))))

;; Execute git with serialization
(System/exit (serialize-git-call *command-line-args*))

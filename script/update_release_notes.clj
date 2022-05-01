#!/usr/bin/env bb
(ns update-release-notes
  (:require [clojure.string :as str]))

(defn -main []
  (let [new-changes (slurp "next-release-changes.md")
        current-changelog (slurp "CHANGELOG.md")
        release-version (str/trim (slurp "stable-version"))
        new-changelog (str "# " release-version
                           " (" (.format (java.text.SimpleDateFormat. "yyyy/MM/dd") (java.util.Date.)) ")"
                           "\n"
                           new-changes
                           (when (seq current-changelog)
                             (str "\n"
                                  current-changelog)))]
    (spit "CHANGELOG.md" new-changelog)
    (spit "next-release-changes.md" "")))

(-main)

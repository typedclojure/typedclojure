#!/usr/bin/env bb
(ns update-release-notes)

(defn -main []
  (let [new-changes (slurp "next-release-changes.md")
        current-changelog (slurp "CHANGELOG.md")
        release-version (slurp "stable-version")
        new-changelog (str "# " release-version
                           "\n"
                           new-changes
                           (when (seq current-changelog)
                             (str "\n"
                                  current-changelog)))]
    (spit "CHANGELOG.md" new-changelog)
    (spit "next-release-changes.md" "")))

(-main)

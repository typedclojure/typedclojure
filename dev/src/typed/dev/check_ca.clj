(ns typed.dev.check-ca
  (:require [hickory.core :as h]
            [hickory.select :as s]))

(defonce hickory-ca-page
  (h/as-hickory (h/parse (slurp "https://clojure.org/dev/contributors"))))

; TODO some entries are comma or slash separated
(defn github-users-signed []
  (into #{; redundant for Rich
          "richhickey"
          ; some users with no GitHub entries
          "stuarthalloway"
          "stuartsierra"
          "seancorfield"
          "dmiller"}
        (->> hickory-ca-page
             (s/select (s/tag :tbody))
             first
             :content
             (filter (comp #{:tr} :tag))
             (map #(filterv map? (:content %)))
             (map peek)
             (keep (comp :content first :content))
             (keep #(when (and (vector? %)
                               (#{1} (count %))
                               (string? (nth % 0)))
                      (nth % 0))))))

(defn -main [user]
  (let [all-signed (github-users-signed)]
    (if (contains? all-signed user)
      (println (str "GitHub user " user " has signed the Clojure CA"))
      (throw
        (ex-info (str "GitHub user " user " has not signed the Clojure CA. Please see CONTRIBUTING.md in this repository.")
                 {:signed all-signed
                  :user user})))))

(comment
  (-main "frenchy64")
  (-main "puredanger")
  (-main "seancorfield")
  (-main "richhickey")
  (-main "stuarthalloway")
  (-main "stuartsierra")
  (->> (hickory-ca-page)
       :content
       last
       )
  )

(ns typed.dev.selmer
  (:require [selmer.parser :as sp :refer [render render-file]]
            [selmer.util :as su]
            [typed.dev.helpers :as h]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(sp/cache-off!)

(defn missing-value-fn [tag context-map]
  (throw (Exception. (str "Missing tag " tag))))

(su/set-missing-value-formatter! missing-value-fn)

(defn do-not-edit-string [src]
  (assert src)
  (format "DO NOT EDIT! Instead, edit `%s` and run `./script/regen-selmer.sh`"
          src))

(sp/add-tag! :do-not-edit-xml-comment
             (fn [args {:keys [src] :as _context-map}]
               (format "<!-- %s -->"
                       (do-not-edit-string src))))

(sp/add-tag! :do-not-edit-edn-comment
             (fn [args {:keys [src] :as _context-map}]
               (assert src)
               (format ";; %s"
                       (do-not-edit-string src))))

(sp/add-tag! :license-md
             (fn [args context-map]
               (str/join
                 "\n"
                 ["## License"
                  ""
                  "Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors."
                  ""
                  "Licensed under the EPL (see the file epl-v10.html)."])))

#_
(defn get-input! [kw]
  (if-some [[_ v] (find (lib/get-input) kw)]
    v
    (throw (Exception. (str "Missing input: " kw)))))

#_
(defn part-of-typed-clojure []
  "<a href='http://typedclojure.org'><img src='/doc/images/part-of-typed-clojure-project.png'></a>")

#_
(defn module-header []
  (str/join
    "\n"
    ["<a href='http://typedclojure.org'><img src='/doc/images/part-of-typed-clojure-project.png'></a>"
     ""
     "<p>"
     "  <a href='https//www.patreon.com/ambrosebs'><img src='/doc/images/become_a_patron_button.png'></a>"
     "  <a href='https://opencollective.com/typedclojure'><img src='/doc/images/donate-to-our-collective.png'></a>"
     "</p>"]))

(defn transforms
  "Returns a map from template file to output location.
  Key is relative to dev/resources/root-templates, val is relative to repo root."
  []
  {:post [(seq %)]}
  (let [;; relative to dev directory, which is the directory this program is run from
        relative-to "resources/root-templates"]
    (into {}
          (map (fn [^java.io.File f]
                 (when-not (.isDirectory f)
                   (let [p (.getPath f)
                         _ (assert (.startsWith p (str relative-to "/")))
                         p (subs p (count (str relative-to "/")))]
                     (assert (seq p))
                     ;; repo root and dev/resources/root-templates have same structure
                     [p p]))))
          (file-seq (io/file relative-to)))))

(defn -main [& args]
  (doseq [[src dest] (transforms)]
    (spit (str "../" dest)
          (render-file
            (str "root-templates/" src)
            (into h/selmer-input-map
                  {;; root-relative
                   :src (str "dev/resources/root-templates/" src)
                   ;; root-relative
                   :dest dest
                   :typedclojure-stable-mvn-version (h/typedclojure-stable-version)
                   :typedclojure-current-mvn-version (h/typedclojure-current-version)})
            {;; {◊var◊} instead of {{var}}
             :filter-open \◊
             :filter-close \◊}))))

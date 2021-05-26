(ns typed.dev.selmer
  (:require [selmer.parser :as sp :refer [render render-file]]
            [selmer.util :as su]
            [typed.dev.helpers :as h]
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

#_
(defn version+doc-info [{:keys [:deps/root coordinate] :as m}]
  (eval-common/bind-env
    (-> (eval-common/get-env)
        (update :prose.alpha.document/input merge m))
    ;; TODO how to trim str/trim start of file?
    ;; overriding slurp/eval fns is too early.
    (lib/require-doc "templates/version-info.md.prose")))


(def transforms
  "Key is relative to dev/resources/root-templates, val is relative to repo root."
  (into {}
        (map (juxt identity identity))
        ;; TODO generate list of relative paths under dev/resources/root-templates/
        #{"README.md"
          "pom.xml"
          "typed/clj.analyzer/README.md"
          "typed/clj.analyzer/deps.edn"
          "typed/clj.analyzer/pom.xml"
          "typed/clj.annotator/README.md"
          "typed/clj.annotator/deps.edn"
          "typed/clj.annotator/pom.xml"
          "typed/clj.checker/README.md"
          "typed/clj.checker/deps.edn"
          "typed/clj.checker/pom.xml"
          "typed/clj.lang/README.md"
          "typed/clj.lang/deps.edn"
          "typed/clj.lang/pom.xml"
          "typed/clj.reader/README.md"
          "typed/clj.reader/deps.edn"
          "typed/clj.reader/pom.xml"
          "typed/clj.refactor/README.md"
          "typed/clj.refactor/deps.edn"
          "typed/clj.refactor/pom.xml"
          "typed/clj.runtime/README.md"
          "typed/clj.runtime/deps.edn"
          "typed/clj.runtime/pom.xml"
          "typed/clj.spec/README.md"
          "typed/clj.spec/deps.edn"
          "typed/clj.spec/pom.xml"
          "typed/cljc.analyzer/README.md"
          "typed/cljc.analyzer/deps.edn"
          "typed/cljc.analyzer/pom.xml"
          "typed/cljs.analyzer/README.md"
          "typed/cljs.analyzer/deps.edn"
          "typed/cljs.analyzer/pom.xml"
          "typed/cljs.checker/README.md"
          "typed/cljs.checker/deps.edn"
          "typed/cljs.checker/pom.xml"
          "typed/lib.clojure/README.md"
          "typed/lib.clojure/deps.edn"
          "typed/lib.clojure/pom.xml"
          "typed/lib.core.async/README.md"
          "typed/lib.core.async/deps.edn"
          "typed/lib.core.async/pom.xml"}))

(defn -main [& args]
  (doseq [[src dest] transforms]
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

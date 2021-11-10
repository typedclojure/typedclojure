(ns typed.dev.new-module
  (:require [selmer.parser :as sp :refer [render render-file]]
            [selmer.util :as su]
            [typed.dev.helpers :as h]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files]))

(sp/cache-off!)

(defn module-info [artifact-id]
  (let [[a1 :as splits] (str/split artifact-id #"[.]")
        _ (assert (every? seq splits) artifact-id)
        module-dir (str "typed/" (str/join "." (subvec splits 1)))
        _ (assert (#{"typed"} a1) (str "First part of artifactId must be `typed` :" artifact-id))
        module-src-dir (str module-dir "/src/" (namespace-munge (str/join "/" (pop splits))))
        module-test-dir (str module-dir "/test/typed_test/" (str/join "/" (map namespace-munge (-> splits (subvec 1) pop))))
        module-templates-dir (str "dev/resources/root-templates/typed/" (str/join "." (subvec splits 1)))
        module-script-dir (str module-dir "/script")
        module-root-src-ns (str/join "." splits)
        module-root-test-ns (str/join "." (cons "typed-test" (subvec splits 1)))]
    {:splits splits
     :module-artifact-id artifact-id
     :module-dir module-dir
     :module-src-dir module-src-dir
     :module-test-dir module-test-dir
     :module-script-dir module-script-dir
     :module-templates-dir module-templates-dir
     :module-root-src-ns module-root-src-ns
     :module-root-test-ns module-root-test-ns
     }))

(defn create-template-plan
  "Repo-root relative mapping from selmer template to destination path.
  Keys are classpath relative to dev/ project's classpath, vals are repo-relative."
  [artifact-id]
  (let [{:keys [splits
                module-dir
                module-src-dir
                module-test-dir
                module-templates-dir
                module-script-dir]} (module-info artifact-id)]
    (-> {"module-template/deps.edn" (str module-templates-dir "/deps.edn")
         "module-template/README.md" (str module-templates-dir "/README.md")
         "module-template/pom.xml" (str module-templates-dir "/pom.xml")
         "module-template/source.clj" (str module-src-dir "/" (peek splits) ".clj")
         "module-template/test.clj" (str module-test-dir "/" (peek splits) ".clj")}
        (into (map (fn [^java.io.File f]
                     (when (not (.isDirectory f))
                       [(str "module-template/script/" (.getName f))
                        (str module-script-dir "/" (.getName f))])))
              (file-seq (io/file "resources/module-template/script"))))))

(comment
  (-main "typed.clj.malli")
  (module-info "typed.clj.foo")
  (create-template-plan "typed.clj.foo")
  (module-info "typed.clj.foo-bar")
  (module-info "typed.clj.foo-bar.baz")
  )

(defn missing-value-fn [tag context-map]
  (throw (Exception. (str "Missing tag value for " tag))))

(defn -main [& args]
  (assert (= 1 (count args))
          (str "Must provide artifactId as only argument: " args))
  (su/set-missing-value-formatter! missing-value-fn)
  (let [artifact-id (first args)
        context-map (assoc (module-info artifact-id)
                           :do-not-edit-edn-comment-template "{% do-not-edit-edn-comment %}"
                           :do-not-edit-xml-comment-template "{% do-not-edit-xml-comment %}")]
    (doseq [[src dest] (create-template-plan artifact-id)]
      (io/make-parents (str "../" dest))
      (spit (str "../" dest)
            (render-file src
                         context-map
                         {;; {&var&} instead of {{var}}
                          :filter-open \&
                          :filter-close \&}))
      (when (Files/isExecutable (-> (io/resource src)
                                    io/file
                                    .toPath))
        (assert (.setExecutable (io/file (str "../" dest))
                                true
                                false)
                (str "Failed to set executable: " dest))))))

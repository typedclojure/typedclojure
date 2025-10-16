(ns build
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.process :as process]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [typed.dev.helpers :as h]))

(def group-id (:typedclojure-group-id h/selmer-input-map))

(defn clean [{:keys [dir] :as params}]
  (b/delete {:path (.getPath (io/file dir "target"))}))

(defn jar [{:keys [version lib basis dir class-dir] :as params}]
  {:pre [version lib basis dir]}
  (clean nil)
  (prn "saving pom to to" class-dir)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :packaging "jar"
                :pom-data [[:licenses
                            [:license
                             [:name "Eclipse Public License 1.0"]
                             [:url "https://opensource.org/licenses/epl-1-0"]
                             [:distribution "repo"]]]] 
                :scm {:url (:typedclojure-git-https-url h/selmer-input-map)
                      :connection (:typedclojure-scm-connection h/selmer-input-map)
                      :developerConnection (:typedclojure-scm-developerConnection h/selmer-input-map)
                      :tag (if (str/ends-with? version "-SNAPSHOT")
                             (b/git-process {:git-args "rev-parse HEAD"})
                             ;; we commit the checksum of the jar to the repo ahead of time to ensure it is reproducible.
                             ;; so it cannot contain the release SHA.
                             ;; TODO include command to reproduce jar
                             ;; TODO push sha to maven-metadata.xml
                             version)}
                ;;TODO copy from basis
                :src-dirs [(.getPath (io/file dir "src"))]})
  (b/copy-dir {;;TODO copy from basis
               :src-dirs [(.getPath (io/file dir "src"))]
               :target-dir class-dir})
  (let [jar-file (.getPath (io/file dir (format "target/%s-%s.jar" (name lib) version)))]
    (b/jar {:class-dir class-dir
            :jar-file jar-file})
    (assoc params :jar-file jar-file)))

(defn tag-release [{:keys [version] :as params}]
  {:pre [version]}
  (b/git-process {:git-args (format "tag %s" version)})
  params)

; (b/git-count-revs nil)
(defn infer-version [{:keys [release] :as params}]
  {:post [(if release
            true
            (str/ends-with? (:version %) "-SNAPSHOT"))]}
  (-> params
      (update :version #(or % (if release
                                (h/typedclojure-stable-version)
                                (h/typedclojure-current-version))))))

(def all-modules
  (into (sorted-map)
        (keep (fn [^java.io.File f]
                (when (.isDirectory f)
                  (when-not (str/starts-with? (.getName f) ".") ;; remove hidden dirs
                    {(symbol "org.typedclojure" (str "typed." (.getName f)))
                     {:path (.getPath f)
                      :depends (-> (io/file f "deps.edn")
                                   slurp
                                   edn/read-string
                                   :deps
                                   keys
                                   set)}}))))
        (.listFiles (io/file "typed"))))

;;TODO topological sort
(def sorted-modules
  '[org.typedclojure/typed.cljc.analyzer
    org.typedclojure/typed.clj.analyzer
    org.typedclojure/typed.lib.spec.alpha
    org.typedclojure/typed.lib.clojure
    org.typedclojure/typed.clj.runtime
    org.typedclojure/typed.clj.checker
    org.typedclojure/typed.lib.core.async
    org.typedclojure/typed.malli
    org.typedclojure/typed.clj.spec
    org.typedclojure/typed.cljs.analyzer
    org.typedclojure/typed.cljs.runtime
    org.typedclojure/typed.cljs.checker])

(assert (= (-> all-modules keys set)
           (set sorted-modules)))
;; TODO replace check with topological sort
(doseq [i (range (count sorted-modules))
        :let [dep (nth sorted-modules i)]
        prev (subvec sorted-modules 0 i)]
  (assert (not (contains? (get-in all-modules [prev :depends])
                          dep))
          (str dep " must go before " prev)))

(defn jars [params]
  (assoc params
         :deployments
         (mapv (fn [lib]
                 (println "Building" lib)
                 (let [dir (get-in all-modules [lib :path])
                       {:keys [jar-file] :as params} (jar (assoc params
                                                                 :lib lib
                                                                 :dir dir
                                                                 :class-dir (.getPath (io/file dir "target/classes"))
                                                                 :basis (b/create-basis {:dir dir
                                                                                         :extra {:deps (zipmap
                                                                                                         (filter
                                                                                                           #(= "org.typedclojure" (namespace %))
                                                                                                           (-> (io/file dir "deps.edn")
                                                                                                               slurp
                                                                                                               edn/read-string
                                                                                                               :deps
                                                                                                               keys))
                                                                                                         (repeat {:mvn/version (:version params)}))}})))]
                   (b/install params)
                   {:jar-file jar-file
                    :pom-file (.getPath (io/file dir "target/classes/META-INF/maven/org.typedclojure" (name lib) "pom.xml"))}))
               sorted-modules)))

(comment
  (b/create-basis {:dir "typed/clj.analyzer"})
  (b/create-basis {:dir "typed/clj.analyzer"
                   :extra {:deps {'org.typedclojure/typed.cljc.analyzer {:mvn/version "1.4.0"}}}})
  )

(defn build-snapshot [params]
  (-> params
      (assoc :release false)
      infer-version
      jars))

(comment
  (build-snapshot nil)
  )

(defn build-release [params]
  (-> params
      (assoc :release true)
      infer-version
      jars))

(comment
  (build-release nil)
  (build-release {:version "1.4.00"})
  )

(defn- deploy-maven [{:keys [jar-file pom-file]}]
  ;; Uses process/start with options map as first argument, then command and args.
  (prn "pom-file" (slurp pom-file))
  (let [proc (process/start {:out :inherit :err :inherit}
                            "mvn" "deploy:deploy-file"
                            (str "-Dfile=" jar-file)
                            (str "-DpomFile=" pom-file)
                            "-DrepositoryId=clojars"
                            "-Durl=https://repo.clojars.org"
                            "-B"
                            "-q")
        exit (.waitFor proc)]
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "Maven deploy failed with exit code" exit))
      (throw (ex-info "Maven deploy failed"
                      {:jar-file jar-file :pom-file pom-file :exit exit})))))

(defn deploy* [params]
  (run! deploy-maven (:deployments params))
  params)

(defn deploy-snapshot [params]
  (-> params
      build-snapshot
      deploy*))

(defn deploy-release [params]
  (-> params
      build-release
      deploy*))

(comment
  (deploy-snapshot nil))

(defn schedule-release [params]
  (let [params (-> params
                   build-release)]
    (b/git-process {:git-args "add ."})
    (b/git-process {:git-args (format "commit -m '[typedclojure-release] %s %s'"
                                      (h/typedclojure-stable-version)
                                      (h/typedclojure-current-version))})))

(defn perform-release [params]
  (tag-release params))

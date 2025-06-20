(ns build
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.java.shell :as sh]
            [clj-commons.digest :as digest]
            [typed.dev.helpers :as h]
            [deps-deploy.deps-deploy :as deploy])
  (:import (java.util.regex Pattern)))

(def group-id (:typedclojure-group-id h/selmer-input-map))

(defn clean [{:keys [dir] :as params}]
  (b/delete {:path (.getPath (io/file dir "target"))}))

(defn jar [{:keys [version lib basis dir class-dir] :as params}]
  {:pre [version lib basis dir]}
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
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

(defn- deploy* [params]
  (doseq [{:keys [jar-file pom-file]} (:deployments params)]
    (deploy/deploy
      {:installer :remote
       :sign-releases? false
       :artifact jar-file
       :pom-file pom-file}))
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

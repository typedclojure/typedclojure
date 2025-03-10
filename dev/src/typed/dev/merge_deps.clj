#!/usr/bin/env bb
;; usage: ./script/gen-dev-project

(ns typed.dev.merge-deps
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [typed.dev.helpers :as h])
  (:import java.io.File))

(set! *warn-on-reflection* true)

(def ^String subprojects-dir "typed")
(def ^String everything-root (str (File. h/repo-root subprojects-dir)))
(def ^String relative-projects-root ".")
(defn aliases []
  (sorted-map
    :perf
    (sorted-map
      :extra-paths ["/Applications/YourKit-Java-Profiler-2019.8.app/Contents/Resources/lib/yjp-controller-api-redist.jar"]
      :jvm-opts ["-agentpath:/Applications/YourKit-Java-Profiler-2019.8.app/Contents/Resources/bin/mac/libyjpagent.dylib"])
    :spec-skip-macros
    {:jvm-opts ["-Dclojure.spec.skip-macros=true"]}
    :kaocha
    {:extra-deps {'lambdaisland/kaocha (sorted-map
                                         :git/url (:kaocha-git-url h/selmer-input-map)
                                         :git/sha (:kaocha-sha h/selmer-input-map))}}
    :eastwood
    (sorted-map
      :main-opts ["-m" "eastwood.lint" {:exclude-linters (sorted-set
                                                           :def-in-def
                                                           :unlimited-use)}]
      :extra-deps (sorted-map 'jonase/eastwood
                              (sorted-map
                                :git/url "https://github.com/jonase/eastwood.git"
                                :git/tag "Release-0.9.9"
                                :git/sha "bbe8610")))
    :nREPL-runner
    {:main-opts ["-m" "nrepl.cmdline" "--interactive"
                 #_"
                 Note:
                   introducing other middleware makes vim-fireplace choose
                   fipp for pprint, which doesn't play well with the delicately
                   defined classes in type-rep."
                 "--middleware" "[cider.nrepl/wrap-complete,cider.nrepl/wrap-info]"
                 ]}
    :nREPL
    (sorted-map
      :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
      :extra-deps
      (sorted-map
        'reply/reply {:mvn/version (:reply-mvn-version h/selmer-input-map)}
        ;; override reply dep
        'net.cgrand/parsley {:mvn/version (:parsley-mvn-version h/selmer-input-map)}
        'nrepl/nrepl {:mvn/version (:nrepl-mvn-version h/selmer-input-map)}
        'cider/cider-nrepl {:mvn/version (:cider-nrepl-mvn-version h/selmer-input-map)}
        'cider/piggieback {:mvn/version (:piggieback-mvn-version h/selmer-input-map)}))))

(def subproject-dirs
  (->> (File. everything-root relative-projects-root)
       .listFiles
       (filter #(let [f (File. (str %) "deps.edn")]
                  (.exists f)))))

(def subproject-base-deps
  (into {}
        (map (fn [^File f]
               (let [project-name-suffix (.getName f)]
                 (assert project-name-suffix)
                 ;; FIXME grab project name from relative pom.xml
                 {(symbol "org.typedclojure" (str "typed." project-name-suffix))
                  (sorted-map
                    :local/root (str (File. "typed" project-name-suffix))
                    :deps/manifest :deps)})))
        subproject-dirs))

(def deps-maps
  (into {}
        (keep #(let [f (File. (str %) "deps.edn")]
                 (assert (.exists f))
                 [(.getName ^File %)
                  (-> f
                      str
                      slurp
                      read-string)]))
        subproject-dirs))

(defn src-paths []
  (vec (sort
         (mapcat
           (fn [[^String fname d]]
             (let [path->relative #(str (-> subprojects-dir
                                            (File. fname)
                                            str
                                            (File. ^String %)))
                   all-paths (concat (:paths d)
                                     (:extra-paths d))]
               (map path->relative all-paths)))
           deps-maps))))

(defn test-paths []
  (vec (sort
         (mapcat
           (fn [[^String fname d]]
             (let [path->relative #(str (-> subprojects-dir
                                            (File. fname)
                                            str
                                            (File. ^String %)))
                   d (-> d :aliases :test)
                   all-paths (concat (:paths d)
                                     (:extra-paths d))]
               (map path->relative all-paths)))
           deps-maps))))

(defn non-resource-test-paths []
  (filterv
    (fn [^String p]
      ;; TODO generalize pattern, perhaps via deps.edn alias convention
      ;; don't reload test resources
      (not= "test-resources" (.getName (File. p))))
    (test-paths)))

(defn kaocha-config []
  {:kaocha/bindings '{kaocha.stacktrace/*stacktrace-filters* []}
   :kaocha/tests                       [{:kaocha.testable/type :kaocha.type/clojure.test
                                         :kaocha.testable/id   :unit
                                         :kaocha/ns-patterns   [".*"]
                                         :kaocha/source-paths  (src-paths)
                                         :kaocha.filter/skip-meta [:typed/skip-from-repo-root]
                                         :kaocha/test-paths    (non-resource-test-paths)}
                                        {:kaocha.testable/type :kaocha.type/clojure.test
                                         :kaocha.testable/id   :cljs
                                         :kaocha/ns-patterns   [".*"]
                                         :kaocha/source-paths  (src-paths)
                                         ;:kaocha.filter/skip-meta [:typed/skip-from-repo-root]
                                         :kaocha/test-paths    (filterv
                                                                 (fn [^String p]
                                                                   ;; FIXME make less hacky
                                                                   (.startsWith p "typed/cljs."))
                                                                 (non-resource-test-paths))}
                                        {:kaocha.testable/type :kaocha.type/clojure.test
                                         :kaocha.testable/id   :checker
                                         :kaocha/ns-patterns   [".*"]
                                         :kaocha/source-paths  (src-paths)
                                         ;:kaocha.filter/skip-meta [:typed/skip-from-repo-root]
                                         :kaocha/test-paths    (filterv
                                                                 (fn [^String p]
                                                                   ;; FIXME make less hacky
                                                                   (.contains p "checker"))
                                                                 (non-resource-test-paths))}
                                        {:kaocha.testable/type :kaocha.type/clojure.test
                                         :kaocha.testable/id   :analyzer
                                         :kaocha/ns-patterns   [".*"]
                                         :kaocha/source-paths  (src-paths)
                                         ;:kaocha.filter/skip-meta [:typed/skip-from-repo-root]
                                         :kaocha/test-paths    (filterv
                                                                 (fn [^String p]
                                                                   ;; FIXME make less hacky
                                                                   (.contains p "analyzer"))
                                                                 (non-resource-test-paths))}]
   :kaocha/fail-fast?                  true
   :kaocha/color?                      false
   :kaocha/reporter                    ['kaocha.report/dots]
   ;:kaocha/reporter                    ['kaocha.report/documentation]
   :kaocha/plugins                     [:kaocha.plugin/randomize
                                        :kaocha.plugin/filter
                                        :kaocha.plugin/capture-output
                                        :kaocha.plugin/profiling]
   ;:kaocha.plugin.randomize/seed       950716166
   :kaocha.plugin.randomize/randomize? true
   :kaocha.plugin.profiling/count      3
   :kaocha.plugin.profiling/profiling? true})

(def test-maps-to-merge (->> (vals deps-maps)
                             (map (comp :test :aliases))
                             (mapcat (fn [d]
                                       {:pre [(map? d)]}
                                       (concat (some-> d :deps vector)
                                               (some-> d :extra-deps vector))))))

(def test-deps (into (sorted-map)
                     (map (fn [[k v]] [k (into (sorted-map) v)]))
                     (apply dissoc (apply merge-with
                                          (fn [v1 v2]
                                            (if (= v1 v2)
                                              v2
                                              (throw (ex-info (str "Version conflict: "
                                                                   v1 " " v2)
                                                              {:versions [v1 v2]
                                                               :maps-to-merge test-maps-to-merge}))))
                                          test-maps-to-merge)
                            (keys subproject-base-deps))))

(defn -main [& args]
  (assert (empty? args))
  (let [dev-deps {'selmer/selmer {:mvn/version (:selmer-mvn-version h/selmer-input-map)}
                  'org.clojure/clojure {:mvn/version (:clojure-mvn-version h/selmer-input-map)}}
        everything-deps (sorted-map
                          :deps (into subproject-base-deps
                                      dev-deps)
                          :paths ["dev/src"]
                          :aliases (assoc (aliases)
                                          :typed (sorted-map
                                                   :exec-fn 'typed.clojure.main/exec
                                                   :exec-args {:dirs (src-paths)
                                                               :watch-dirs (concat (src-paths) (test-paths))
                                                               :refresh-dirs (concat (src-paths)
                                                                                     (non-resource-test-paths))})
                                          :test (sorted-map
                                                  :jvm-opts ["-Djdk.attach.allowAttachSelf"
                                                             "-Dtyped.cljc.checker.utils.trace=true"
                                                             "-Dtyped.cljc.analyzer.passes/direct-link=false"
                                                             "-Dtyped.clojure.preserve-check-ns-after-opt-in=true"]
                                                  :extra-deps test-deps
                                                  :extra-paths (test-paths))))
        preamble (str ";; AUTOGENERATED FOR LOCAL DEV ONLY!!\n"
                      ";; edit via dev/src/typed/dev/merge_deps.clj")]
    (spit (str (File. h/repo-root "deps.edn"))
          (with-out-str
            (println preamble)
            (binding [*print-length* nil
                      *print-level* nil
                      *print-namespace-maps* nil]
              (pp/pprint everything-deps))))
    (spit (str (File. h/repo-root "tests.edn"))
          (with-out-str
            (println preamble)
            (binding [*print-length* nil
                      *print-level* nil
                      *print-namespace-maps* nil]
              (pp/pprint (kaocha-config)))))))

(when (= *file* (System/getProperty "babashka.file")) (-main))

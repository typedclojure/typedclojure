(ns typed.dev.merge-deps
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [typed.dev.helpers :as h])
  (:import java.io.File)
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^String everything-root (str h/repo-root "/typed"))
(def ^String relative-projects-root ".")
(defn aliases []
  `{:perf
    {:extra-paths ["/Applications/YourKit-Java-Profiler-2019.8.app/Contents/Resources/lib/yjp-controller-api-redist.jar"]
     :jvm-opts ["-agentpath:/Applications/YourKit-Java-Profiler-2019.8.app/Contents/Resources/bin/mac/libyjpagent.dylib"]}
    :spec-skip-macros
    {:jvm-opts ["-Dclojure.spec.skip-macros=true"]}
    :eastwood
    {:main-opts ["-m" "eastwood.lint" {}]
     :extra-deps {jonase/eastwood {:git/url "https://github.com/jonase/eastwood.git"
                                   :git/tag "Release-0.9.6"
                                   :git/sha "0130b5ec15a09458407ccf774ed76e22e08794d1"}}}
    :nREPL
    {:extra-deps
     {nrepl/nrepl {:mvn/version ~(:nrepl-mvn-version h/selmer-input-map)}
      cider/cider-nrepl {:mvn/version "0.25.3"}
      cider/piggieback {:mvn/version "0.5.2"}}
     :main-opts ["-m" "nrepl.cmdline" "--interactive"
                 #_"
                 Note:
                   introducing other middleware makes vim-fireplace choose
                   fipp for pprint, which doesn't play well with the delicately
                   defined classes in type-rep."
                 "--middleware" "[cider.nrepl/wrap-complete,cider.nrepl/wrap-info]"
                 ]}})

(defn -main [& args]
  (let [deps-maps (->> (File. everything-root relative-projects-root)
                       .listFiles 
                       (keep #(let [f (File. (str %) "deps.edn")]
                               (when (.exists f)
                                 [(.getName ^File %)
                                  (-> f
                                      str
                                      slurp
                                      read-string)])))
                       (into {}))
        expand-deps (juxt identity
                          (comp :test :aliases))
        maps-to-merge (->> (vals deps-maps)
                           (mapcat expand-deps)
                           (mapcat (fn [d]
                                     {:pre [(map? d)]}
                                     (concat (some-> d :deps vector)
                                             (some-> d :extra-deps vector)))))
        everything-deps {:deps (apply merge-with
                                      (fn [v1 v2]
                                        (if (= v1 v2)
                                          v2
                                          (throw (ex-info (str "Version conflict: "
                                                               v1 " " v2)
                                                          {:versions [v1 v2]
                                                           :maps-to-merge maps-to-merge}))))
                                      maps-to-merge)
                         :paths (vec (mapcat
                                       (fn [[^String fname d]]
                                         (let [path->relative #(str (-> relative-projects-root
                                                                        (File. fname)
                                                                        str
                                                                        (File. ^String %)))]
                                           (mapcat (fn [d]
                                                     (let [all-paths (concat (:paths d)
                                                                             (:extra-paths d))]
                                                       (map path->relative all-paths)))
                                                   (expand-deps d))))
                                       deps-maps))
                         :aliases (aliases)}]
    (spit (str (File. everything-root "deps.edn"))
          (with-out-str
            (binding [*print-length* nil
                      *print-level* nil
                      *print-namespace-maps* nil]
              (pp/pprint everything-deps))))))

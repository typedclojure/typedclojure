;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; mostly copied from clojure.core's data-reader discovery impl
(ns ^:no-doc clojure.core.typed.runtime.jvm.configs
  "Alpha - wip, subject to change"
  (:require [clojure.tools.reader :as rdr])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io InputStreamReader]
           [java.net URL]))

(defn- config-urls [features]
  (let [cl (.. Thread currentThread getContextClassLoader)]
    (concat
      (when (:clj features)
        (enumeration-seq (.getResources cl "typedclojure_config.clj")))
      (when (:cljs features)
        (enumeration-seq (.getResources cl "typedclojure_config.cljs")))
      (enumeration-seq (.getResources cl "typedclojure_config.cljc")))))

(defn- load-config-file [features ^URL url]
  (with-open [rdr (LineNumberingPushbackReader.
                    (InputStreamReader.
                      (.openStream url) "UTF-8"))]
    (binding [*file* (.getFile url)]
      (let [read-opts (cond-> {:eof nil}
                        (.endsWith (.getPath url) "cljc")
                        (assoc :read-cond :allow
                               :features features))
            new-config (try (rdr/read read-opts rdr)
                            (catch Throwable e
                              (println e)
                              (throw (ex-info (str "Error while reading " url)
                                              {:url url}))))]
        (when-not (map? new-config)
          (throw (ex-info (str "Not a valid Typed Clojure config map")
                          {:url url})))
        new-config))))

(defn- load-configs [features]
  (reduce (fn [configs url]
            (conj configs (load-config-file features url)))
          #{} (config-urls features)))

(def *clj-configs
  (delay (load-configs #{:clj})))

(def *cljs-configs
  (delay (load-configs #{:cljs})))

(defn- register-config-anns [configs require-fn]
  (run! (fn [{:keys [ann]}]
          (run! #(do (println (format "Registering annotations from %s..." %))
                     (require-fn %))
                ann))
        configs))

(defn- register-config-exts [configs require-fn]
  (run! (fn [{:keys [ext]}]
          (run! #(do (println (format "Registering extensions from %s..." %))
                     (require-fn %))
                ext))
        configs))

(defn- clj-require [nsym]
  (locking clojure.lang.RT/REQUIRE_LOCK
    (require nsym)))

(defn register-clj-config-anns [] (register-config-anns @*clj-configs clj-require))
(defn register-clj-config-exts [] (register-config-exts @*clj-configs clj-require))

(defn- cljs-require [nsym]
  ;; enough to macroexpand the file to force macros side effects
  ((requiring-resolve 'cljs.analyzer.api/analyze-file)
   ((requiring-resolve 'cljs.util/ns->source) nsym))
  #_
  ((requiring-resolve 'typed.cljs.checker.util/with-analyzer-bindings*)
   (fn []
     ((requiring-resolve 'typed.cljs.checker.util/with-cljs-typed-env*)
      #(do
         (requiring-resolve 'typed.cljs.checker.util/with-core-cljs*)
         ((requiring-resolve 'cljs.analyzer.api/analyze)
          ((requiring-resolve 'cljs.analyzer.api/empty-env))
          `(cljs.core/require '~nsym))))))
  nil)

(defn register-cljs-config-anns [] (register-config-anns @*cljs-configs cljs-require))
(defn register-cljs-config-exts [] (register-config-exts @*cljs-configs clj-require))

(defn- config-var-providers [configs]
  (println "Registering :var-type-providers from typedclojure_config's...")
  (mapcat :var-type-providers configs))
(defn clj-config-var-providers [] (config-var-providers @*clj-configs))
(defn cljs-config-var-providers [] (config-var-providers @*cljs-configs))

(defn register-malli-extensions [configs]
  (run! #(do (println (format "Registering Malli extensions from %s..." %))
             (clj-require %))
        (mapcat :malli-extensions configs)))
(defn register-clj-malli-extensions [] (register-malli-extensions @*clj-configs))
(defn register-cljs-malli-extensions [] (register-malli-extensions @*cljs-configs))

(defn register-spec1-extensions [configs]
  (run! #(do (println (format "Registering Spec1 extensions from %s..." %))
             (clj-require %))
        (mapcat :spec1-extensions configs)))
(defn register-clj-spec1-extensions [] (register-spec1-extensions @*clj-configs))
(defn register-cljs-spec1-extensions [] (register-spec1-extensions @*cljs-configs))

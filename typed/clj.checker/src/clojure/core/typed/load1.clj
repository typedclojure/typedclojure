;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.load1
  "Implementation of clojure.core.typed.load."
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [typed.clj.analyzer :as jana2]
            [typed.clj.checker.check-form :as chk-frm-clj]
            [typed.clj.lang :as lang]
            [typed.clj.runtime.env :as clj-env]
            [typed.cljc.analyzer.env :as env]
            [typed.clj.checker.utils :refer [->opts]]
            [typed.cljc.checker.check-form :as chk-frm]
            [typed.cljc.checker.ns-deps-utils :as ns-utils])
  (:import java.net.URL))

(defn base-resource-path->resource [base-resource-path]
  (some #(let [f (str base-resource-path %)]
           (when-some [r (io/resource f)]
             [r f]))
        [".clj" ".cljc"]))

;; based on clojure.tools.analyzer.jvm/analyze-ns
;; (IFn [String -> nil]
;;      [String ToolsAnalyzerEnv -> nil]
;;      [String ToolsAnalyzerEnv ToolsReaderOpts -> nil])
(defn load-typed-file
  "Loads a whole typed namespace, returns nil. Assumes the file is typed."
  ([filename] (load-typed-file filename (jana2/empty-env) {}))
  ([filename env] (load-typed-file filename env {}))
  ([filename env {:keys [ex-handler skip-check-form?] :as check-opts}]
   {:pre [(string? filename)]
    :post [(nil? %)]}
   ;(prn "load-typed-file" filename)
    (t/load-if-needed)
    (let [opts (->opts)]
      (env/ensure (jana2/global-env)
     (let [ex-handler (or ex-handler #(throw %))
           skip-check-form? (or skip-check-form? (fn [_] false))
           env (or env (jana2/empty-env))
           should-runtime-infer? vs/*prepare-infer-ns*
           instrument-infer-config vs/*instrument-infer-config*
           _ (when should-runtime-infer?
               (println "Refreshing runtime inference")
               (t/refresh-runtime-infer))
           orig-filename filename
           [file-url filename] (base-resource-path->resource filename)]
       (assert file-url (str "Cannot find file " orig-filename))
       (binding [*ns* *ns*
                 *file* filename
                 vs/*typed-load-atom* (atom {})
                 vs/*prepare-infer-ns* nil
                 vs/*instrument-infer-config* nil]
         (with-open [rdr (io/reader file-url)]
           (let [pbr (readers/indexing-push-back-reader
                       (java.io.PushbackReader. rdr) 1 filename)
                 eof (Object.)
                 read-opts (cond-> {:eof eof :features #{:clj :t.a.jvm}}
                             (.endsWith ^String filename "cljc") (assoc :read-cond :allow))
                 config (assoc (chk-frm-clj/config-map2)
                               :env env
                               :should-runtime-infer? should-runtime-infer?
                               :instrument-infer-config instrument-infer-config)]
             (loop []
               (let [form (reader/read read-opts pbr)]
                 (when-not (identical? form eof)
                   (if (skip-check-form? form)
                     (lang/default-eval form)
                     (let [{:keys [ex]} (chk-frm/check-form-info
                                          config form {:check-config (t/default-check-config)}
                                          opts)]
                       (some-> ex ex-handler)))
                   (recur))))))))))))

(defn typed-load1
  "For each path, checks if the given file is typed, and loads it with core.typed if so,
  otherwise with clojure.core/load"
  [& base-resource-paths]
  {:pre [(every? string? base-resource-paths)]
   :post [(nil? %)]}
  ;(prn "typed load" base-resource-paths)
  (let [opts (->opts)]
    (doseq [base-resource-path base-resource-paths]
      (cond
        (or (ns-utils/file-should-use-typed-load? (str base-resource-path ".clj") opts)
            (ns-utils/file-should-use-typed-load? (str base-resource-path ".cljc") opts))
        (do
          (when @#'clojure.core/*loading-verbosely*
            (printf "Loading typed file\n" base-resource-path))
          (load-typed-file base-resource-path))

        :else (clojure.lang.RT/load base-resource-path)))))

(defn typed-eval [form]
  (let [{:keys [ex result]} (t/check-form-info form)]
    (or (some-> ex throw)
        result)))

(defn install-typed-load
  "Extend the :lang dispatch table with the :core.typed language"
  []
  {:post [(nil? %)]}
  (alter-var-root #'lang/lang-dispatch
                  (fn [m]
                    (-> m 
                        (assoc-in [:core.typed :load] #'typed-load1)
                        (assoc-in [:core.typed :eval] #'typed-eval))))
  nil)

(defn monkey-patch-typed-load
  "Install the :core.typed :lang, and monkey patch `load`"
  []
  {:post [(nil? %)]}
  (install-typed-load)
  (lang/monkey-patch-extensible-load)
  nil)

(defn monkey-patch-typed-eval
  "Install the :core.typed :lang, and monkey patch `eval`"
  []
  {:post [(nil? %)]}
  (install-typed-load)
  (lang/monkey-patch-extensible-eval)
  nil)

(defn install 
  "Install the :core.typed :lang. Takes an optional set of features
  to install, defaults to #{:load :eval}.

  Features:
    - :load    Installs typed `load` over `clojure.core/load`
    - :eval    Installs typed `eval` over `clojure.core/eval`

  eg. (install)            ; installs `load` and `eval`
  eg. (install #{:eval})   ; installs `eval`
  eg. (install #{:load})   ; installs `load`"
  ([] (install :all))
  ([features]
   {:pre [((some-fn set? #{:all}) features)]
    :post [(nil? %)]}
   (lang/install features)
   (when (or (= features :all)
             (:load features))
     (monkey-patch-typed-load))
   (when (or (= features :all)
             (:eval features))
     (monkey-patch-typed-eval))
   nil))

(comment (find-resource "clojure/core/typed/test/load_file.clj")
         (typed-load "/clojure/core/typed/test/load_file.clj")
         (load "/clojure/core/typed/test/load_file")
         (require 'clojure.core.typed.test.load-file :reload :verbose)
         )

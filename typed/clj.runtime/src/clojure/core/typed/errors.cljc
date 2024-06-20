;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.errors
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  (:require [clojure.core.typed.util-vars :as uvs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.pprint :as pp]
            [clojure.core.typed.ast-utils :as ast-u]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

(def type-error-kw ::type-error)
(def int-error-kw ::internal-error)
(def nyi-error-kw ::nyi-error)

(def tc-error-parent ::tc-error-parent)

(defn derive-error [kw]
  (derive kw tc-error-parent))

(derive-error type-error-kw)
(derive-error int-error-kw)
(derive-error nyi-error-kw)

;(t/ann ^:no-check env-for-error [Any t/Any -> Any])
(defn env-for-error [env opts]
  (merge (select-keys env [:line :column])
         (impl/impl-case opts
           :clojure (let [f (:file env)]
           (when (string? f)
             {:file f}))
           ;FIXME filename?
           :cljs
           (let [n (get-in env [:ns :name])]
             (when (symbol? n)
               {:ns n})))))

(defn int-error
  ([estr opts] (int-error estr {} opts))
  ([estr {:keys [cause visible-cause use-current-env] :as opt} {::uvs/keys [current-expr] :as opts}]
   (assert (not (and cause visible-cause)))
   (let [{:keys [line column file] :as env} (::uvs/current-env opts)]
     (throw (ex-info (str "Internal Error "
                          "(" (or file 
                                  (impl/impl-case opts
                                    :clojure (or (:ns env) *file* (ns-name *ns*))
                                    :cljs (:name (:ns env))
                                    :unknown "?"))
                          ":" 
                          (or line "<NO LINE>")
                          (when column
                            (str ":" column))
                          ") "
                          estr)
                     (cond->
                       {:type-error int-error-kw
                        :env (or (when (and env
                                            (not use-current-env))
                                   (:env current-expr))
                                 (env-for-error env opts))}
                       ;; don't want this to unwrap in the REPL, so don't use 3rd arg of ex-info
                       cause (assoc :cause cause))
                     ;; for when we *do* want to see the cause
                     visible-cause)))))

;errors from check-ns or cf
(defn top-level-error? [{:keys [type-error] :as exdata}]
  (= :top-level-error type-error))

#?(:clj
(defmacro top-level-error-thrown? [& body]
  `(with-ex-info-handlers
     [top-level-error? (constantly true)]
     ~@body
     false)))

#?(:clj
(defmacro top-level-type-error-thrown?
  "Succeeds if a top-level error thrown by check-ns is due to a type error."
  [& body]
  `(with-ex-info-handlers
     [top-level-error? (fn [exd# _#]
                         (some->> (seq (:errors exd#))
                                  (every? #(some-> % ex-data type-error?))))]
     ~@body
     false)))

#?(:clj
(defmacro tc-error-thrown? [& body]
  `(with-ex-info-handlers
     [tc-error? (constantly true)]
     ~@body
     false)))

(defn tc-error? [exdata]
  (assert (not (instance? clojure.lang.ExceptionInfo exdata)))
  (isa? (:type-error exdata) tc-error-parent))

(defn type-error? [exdata]
  (assert (not (instance? clojure.lang.ExceptionInfo exdata)))
  (= (:type-error exdata) type-error-kw))

(defn any-tc-error? [exdata]
  (assert (not (instance? clojure.lang.ExceptionInfo exdata)))
  (keyword? (:type-error exdata)))

(defn msg-fn-opts []
  {:parse-type (requiring-resolve 'typed.clj.checker.parse-unparse/parse-type)})

(defn tc-delayed-error
  "Supports kw args or single optional map."
  ([msg opts] (tc-delayed-error msg {} opts))
  ([msg {:keys [return form expected] :as opt} {::uvs/keys [delayed-errors current-expr] :as opts}]
   (let [form (cond
                (contains? (:opts expected) :blame-form) (-> expected :opts :blame-form)
                (contains? opt :blame-form) (:blame-form opt)
                (contains? opt :form) form
                :else (or (some-> current-expr (ast-u/emit-form-fn opts))
                          '<NO-FORM>))
         msg (str (when-some [msg-fn (some-> (or (-> expected :opts :msg-fn)
                                                 (:msg-fn opt))
                                             eval)]
                    (str (msg-fn (merge (msg-fn-opts)
                                        (when-some [[_ actual] (find opt :actual)]
                                          {:actual ((requiring-resolve 'typed.clj.checker.parse-unparse/unparse-type) actual opts)})))
                         (when msg
                           (str
                             "\n\n"
                             "====================\n"
                             "  More information  \n"
                             "====================\n\n"))))
                  msg)
         e (ex-info msg {:type-error type-error-kw
                         :env (env-for-error
                                (merge (or (:env current-expr)
                                           (::uvs/current-env opts))
                                       (when (contains? (:opts expected) :blame-form)
                                         (meta (-> expected :opts :blame-form))))
                                opts)
                         :form form})]
     (cond
       ;can't delay here
       (not delayed-errors)
       (throw e)

       :else
       (do
         (swap! delayed-errors conj e)
         (or (when (contains? opt :return)
               return)
             @(requiring-resolve 'typed.cljc.checker.type-rep/-error)))))))

(defn tc-error
  [estr opts]
  (let [env (::uvs/current-env opts)]
    (throw (ex-info (str "Type Error "
                         "(" (:file env) ":" (or (:line env) "<NO LINE>")
                         (when-let [col (:column env)]
                           (str ":" col))
                         ") "
                         estr)
                    (merge
                      {:type-error type-error-kw}
                      {:env (env-for-error env opts)})))))

(defn warn [msg]
  (println (str "WARNING: " msg)))

(defn deprecated-warn
  [msg opts]
  (let [env (::uvs/current-env opts)
        file (:file env)]
    (println 
      (str
        "DEPRECATED SYNTAX "
        "(" 
        (cond
          (and file
               (not= "NO_SOURCE_PATH" file))
          (str (:file env)
               (when-let [line (:line env)]
                 (str ":" (:line env)
                      (when-let [col (:column env)]
                        (str ":" col)))))
          :else "NO_SOURCE_PATH")
        "): "
        msg))
    (flush))
  nil)

(defn nyi-error
  [estr opts]
  (let [env (::uvs/current-env opts)]
    (throw (ex-info (str "core.typed Not Yet Implemented Error:"
                           "(" (:file env) ":" (or (:line env) "<NO LINE>")
                           (when-let [col (:column env)]
                             (str ":"col))
                           ") "
                           estr)
                    {:type-error nyi-error-kw
                     :env (env-for-error env opts)}))))

#?(:cljs :ignore :default
(defmacro with-ex-info-handlers 
  "Handle an ExceptionInfo e thrown in body. The first handler whose left hand
  side returns true, then the right hand side is called passing (ex-info e) and e."
  [handlers & body]
  `(try
     (do ~@body)
     (catch clojure.lang.ExceptionInfo e#
       (let [found?# (atom false)
             result# (reduce (fn [_# [h?# hfn#]]
                               (when (h?# (ex-data e#))
                                 (reset! found?# true)
                                 (reduced (hfn# (ex-data e#) e#))))
                             nil
                             ~(mapv vec (partition 2 handlers)))]
         (if @found?#
           result#
           (throw e#)))))))

(defn var-for-impl [sym opts]
  {:pre [((some-fn string? symbol?) sym)]
   :post [(symbol? %)]}
  (symbol
    (impl/impl-case opts
      :clojure "clojure.core.typed"
      :cljs "cljs.core.typed")
    (str sym)))

(defn deprecated-plain-op
  ([old opts] (deprecated-plain-op old old opts))
  ([old new opts]
   {:pre [(symbol? old)
          ((some-fn symbol? nil?) new)]}
   (deprecated-warn (str old " syntax is deprecated, use " (var-for-impl (or new old) opts)) opts)))

(defn print-errors!
  "Internal use only"
  [errors opts]
  {:pre [(seq errors)
         (every? #(instance? clojure.lang.ExceptionInfo %) errors)]}
  (binding [*out* *err*]
    (doseq [^Exception e errors]
      (let [{{:keys [file line column] :as env} :env :as data} (ex-data e)]
        (print "Type Error ")
        (print (str "(" (or file 
                            (let [nsym (-> env :ns)]
                              (when (symbol? nsym)
                                nsym))
                            "NO_SOURCE_FILE")
                    (when line
                      (str ":" line
                           (when column
                             (str ":" column))))
                    ") "))
        (println)
        (print (#?(:cljr .Message :default .getMessage) e))
        (println)
        (flush)
        (let [[_ form :as has-form?] (find data :form)]
          (when has-form?
            (print "\n\nin:\n")
            (binding [*print-length* (when-not (::uvs/verbose-forms opts)
                                       10)
                      *print-level* (when-not (::uvs/verbose-forms opts)
                                      10)]
              (pp/pprint form)
              (println))
            (println)
            (println)
            (flush)))
        (flush))))
  (throw (ex-info (str "Type Checker: Found " (count errors) " error" (when (< 1 (count errors)) "s"))
                  {:type-error :top-level-error
                   :errors errors})))

(defn ^:no-doc
  -init-delayed-errors 
  "Internal use only"
  []
  (atom [] :validator #(and (vector? %)
                            (every? (fn [a] 
                                      (instance? clojure.lang.ExceptionInfo a))
                                    %))))

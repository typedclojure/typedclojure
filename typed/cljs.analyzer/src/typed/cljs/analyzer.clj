;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.analyzer
  "Analyzer for clojurescript code."
  (:require [cljs.env :as env]
            [cljs.analyzer :as ana-cljs']
            [cljs.analyzer.api :as ana-api]
            [typed.cljc.analyzer :as ana]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]))

(def instrumented-analyzer-ns (doto 'typed.cljs.analyzer.wrapped.cljs.analyzer
                                create-ns))
(alias 'ana-cljs instrumented-analyzer-ns)

;;https://clojure.atlassian.net/browse/CLJ-2568
(defn walk+meta
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (let [restore-meta #(if-let [fm (meta form)]
                        (with-meta %
                                   (merge fm (meta %)))
                        %)]
    (cond
      (list? form) (outer (restore-meta (apply list (map inner form))))
      (instance? clojure.lang.IMapEntry form)
      (outer (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
      (seq? form) (outer (restore-meta (doall (map inner form))))
      (instance? clojure.lang.IRecord form)
      (outer (restore-meta (reduce (fn [r x] (conj r (inner x))) form form)))
      (coll? form) (outer (restore-meta (into (empty form) (map inner form))))
      :else (outer form))))

;;https://clojure.atlassian.net/browse/CLJ-2568
(defn postwalk+meta
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  {:added "1.1"}
  [f form]
  (walk+meta (partial postwalk+meta f) f form))

;;https://clojure.atlassian.net/browse/CLJ-2568
(defn prewalk+meta
  "Like postwalk, but does pre-order traversal."
  {:added "1.1"}
  [f form]
  (walk+meta (partial prewalk+meta f) identity (f form)))

(defn instrument-cljs-analyzer-source []
  (let [filename "cljs/analyzer.cljc"]
    (with-open [rdr (io/reader (io/resource filename))]
      (let [pbr (readers/indexing-push-back-reader
                  (java.io.PushbackReader. rdr) 1 filename)
            eof (Object.)
            read-opts {:eof eof :features #{:clj}}
            read-opts (if (.endsWith filename "cljc")
                        (assoc read-opts :read-cond :allow)
                        read-opts)]
        (binding [*ns* *ns*
                  *file* filename]
          (loop [forms []]
            (let [form (reader/read read-opts pbr)]
              (if (identical? form eof)
                forms
                (let [form (postwalk+meta
                             (fn [form]
                               (cond
                                 ;; rename ::keywords to cljs.analyzer namespace
                                 (and (keyword? form)
                                      (= (str instrumented-analyzer-ns)
                                         (namespace form)))
                                 (keyword "cljs.analyzer" (name form))

                                 :else form))
                             form)
                      form (cond
                             (empty? forms) (do (assert (and (seq? form)
                                                             (= 'ns (first form))
                                                             (= 'cljs.analyzer (second form)))
                                                        (str "Incompatible cljs.analyzer version, must use exact version depended on by org.typedclojure/typed.cljs.analyzer: "
                                                             "first form was not an ns form " (pr-str form)))
                                                (with-meta
                                                  (apply list
                                                         (-> form
                                                             vec
                                                             (assoc 1 instrumented-analyzer-ns)))
                                                  (meta form)))

                             ;; make `analyze` and `parse` dynamic
                             (or (and (seq? form)
                                      (= 'declare (first form))
                                      (= 'analyze (second form)))
                                 (and (seq? form)
                                      (= 'defn (first form))
                                      (= 'analyze (second form)))
                                 (and (seq? form)
                                      (= 'defmulti (first form))
                                      (= 'parse (second form))))
                             (with-meta
                               (apply list
                                      (-> form
                                          vec
                                          (update 1 vary-meta assoc :dynamic true)))
                               (meta form))

                             ;; don't redefine existing dynamic vars
                             (or (and (seq? form)
                                      (= 'declare (first form))
                                      (= 'parse-ns (second form)))
                                 (and (seq? form)
                                      ('#{def defn} (first form))
                                      (-> (second form) meta :dynamic)))
                             `(require '~['cljs.analyzer :refer [(second form)]])
                             :else form)]
                  ;(binding [#_#_*print-meta* true] (prn "evaling" form))
                  (eval form)
                  (recur (conj forms form)))))))))))

(comment
  (do (instrument-cljs-analyzer-source)
      nil))

(defonce _create-instrumented-analyzer-ns
  (instrument-cljs-analyzer-source))

;; use analyzer's dynamic vars
(assert (= #'ana-cljs'/*cljs-ns* (ns-resolve instrumented-analyzer-ns '*cljs-ns*)))

(def inner-parse ana-cljs/parse)
(def inner-analyze ana-cljs/analyze)

(declare analyze-outer-root)

(defmulti parse (fn [op env form nme opts] op))

(defmethod parse 'def
  [op env form nme opts]
  (let [inner (inner-parse op env form nme opts)]
    (update inner :var
            (fn [expr]
              ;; completely analyze this:
              ;; :var (assoc
              ;;        (analyze
              ;;          (-> env (dissoc :locals)
              ;;            (assoc :context :expr)
              ;;            (assoc :def-var true))
              ;;          sym)
              ;;        :op :var)
              (assert (= :var (:op expr))
                      (pr-str (:op expr) (vec (keys expr))))
              (assert (symbol? (:form expr)))
              (-> expr
                  (assoc :op :unanalyzed)
                  analyze-outer-root)))))

(defmethod parse 'case*
  [op env [_ sym tests thens default :as form] name _]
  (assert (symbol? sym) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (assoc env :context :expr)
        v        (ana-cljs/disallowing-recur (ana-cljs/analyze expr-env sym))
        tests    (mapv #(mapv (fn [t] (analyze-outer-root (ana-cljs/analyze expr-env t))) %) tests)
        thens    (mapv #(ana-cljs/analyze env %) thens)
        nodes    (mapv (fn [tests then]
                         {:op :case-node
                          ;synthetic node, no :form
                          :env env
                          :tests (mapv (fn [test]
                                         {:op :case-test
                                          :form (:form test)
                                          :env expr-env
                                          :test test
                                          :children [:test]})
                                       tests)
                          :then {:op :case-then
                                 :form (:form then)
                                 :env env
                                 :then then
                                 :children [:then]}
                          :children [:tests :then]})
                       tests
                       thens)
        default  (ana-cljs/analyze env default)]
    (assert (every? (fn [t]
                      (or
                        (-> t :info :const)
                        (and (= :const (:op t))
                             ((some-fn number? string? char?) (:form t)))))
              (apply concat tests))
      "case* tests must be numbers, strings, or constants")
    {:env env :op :case :form form
     :test v :nodes nodes :default default
     :children [:test :nodes :default]}))

(defmethod parse :default
  [op env form nme opts]
  (inner-parse op env form nme opts))

(defn analyze
  ([env form] (analyze env form nil))
  ([env form name]
   (analyze env form name
            (when env/*compiler*
              (:options @env/*compiler*))))
  ([env form name opts]
   (inner-analyze env form name opts)))

(comment
  (env/with-compiler-env (ana-api/empty-state)
    (analyze (ana-api/empty-env)
             (list '+ 1 2)))
  (clojure.repl/pst)
  )

(defn unanalyzed-env-first
  ([env form] (unanalyzed-env-first env form nil))
  ([env form name] (unanalyzed-env-first
                     env form name (when env/*compiler*
                                     (:options @env/*compiler*))))
  ([env form name opts]
   {:pre [(map? env)]}
   {:op :unanalyzed
    :env env
    :form form
    :name name
    :opts opts
    :bindings (select-keys (get-thread-bindings)
                           [#'ana-cljs'/*recur-frames*
                            #'ana-cljs'/*loop-lets*
                            #'ana-cljs'/*allow-redef*
                            #'ana-cljs'/*allow-ns*
                            ])}))

(defn unanalyzed [form env]
  {:pre [(map? env)]}
  (unanalyzed-env-first env form))

(defn analyze-outer [ast]
  (case (:op ast)
    :unanalyzed (with-bindings (:bindings ast)
                    (cond-> (analyze (:env ast)
                                     (:form ast)
                                     (:name ast)
                                     (:opts ast))
                      (:body? ast) (assoc :body? true)))
    ast))

(defn analyze-outer-root [ast]
  (let [ast' (analyze-outer ast)]
    (if (identical? ast ast')
      ast'
      (recur ast'))))

(comment
  (select-keys
    (-> (env/with-compiler-env
          (ana-api/empty-state)
          (unanalyzed
            (ana-api/empty-env)
            (list '+ (list '- 1) 2)))
        analyze-outer)
    [:form :op])
  )

(defn default-thread-bindings [env]
  {#'ana-cljs/parse parse
   #'ana-cljs/analyze unanalyzed-env-first
   #'ana/scheduled-passes {:pre identity
                           :post identity
                           :init-ast identity}})

(defn resolve-op-sym
  [form env]
  (when (seq? form)
    (let [op (first form)]
      (when (and (symbol? op)
                 (not (ana-cljs'/specials op))
                 (not (get (:locals env) op)))
        (when-some [{:keys [op] :as expr} (ana-api/resolve env op)]
          (case op
            :var (:name expr)
            (throw (ex-info (str `resolve-op-sym " unsupported op: " op)))))))))

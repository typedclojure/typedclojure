(ns typed.clj.refactor
  "Alpha"
  (:refer-clojure :exclude [run!])
  (:require [typed.clj.analyzer :as ana]
            [typed.cljc.analyzer :as ana-cljc]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as ana-env]
            [typed.clj.reader :as rdr]
            [typed.clj.reader.reader-types :refer [indexing-push-back-reader]])
  (:import [java.io StringReader BufferedReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(declare ^:private refactor-form*)

(defmulti ^:private -refactor-form* (fn [expr rdr-ast opt] (::ana-cljc/op expr)))

(defmethod -refactor-form* ::ana-cljc/local
  [{:keys [form] sym :name :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:local {:local sym
                      :name sym
                      :form form}])))
  expr)

(defmethod -refactor-form* ::ana-cljc/var
  [{:keys [form var] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:var (symbol var)])))
  expr)

(defmethod -refactor-form* ::ana-cljc/binding
  [{:keys [form local env] sym :name :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:binding {:local local
                        :name sym
                        :sym sym}])))
  expr)

(defmethod -refactor-form* :default
  [expr rdr-ast opt]
  (-> expr
      (ast/update-children #(refactor-form* % rdr-ast opt))))

(defmulti -refactor-special (fn [op-sym expr rdr-ast opt] op-sym))

(defn ^:private run!
  "Runs the supplied procedure (via reduce), for purposes of side
  effects, on successive items in the collection. Returns nil"
  {:added "1.7"}
  [proc coll]
  (reduce #(do (proc %2) nil) nil coll)
  nil)

(defn record-destructuring [b file-map-atom]
  (cond
    (vector? b) (do
                  (when (seq (meta b))
                    (swap! file-map-atom assoc
                           (select-keys (meta b)
                                        [:line :column
                                         :end-line :end-column
                                         :file])
                           [:vector-destructure b]))
                  (run! #(record-destructuring % file-map-atom) b))
    (map? b) (do
               (when (seq (meta b))
                 (swap! file-map-atom assoc
                        (select-keys (meta b)
                                     [:line :column
                                      :end-line :end-column
                                      :file])
                        [:map-destructure b]))
               (->> b
                    (run! (fn [[bi v :as orig]]
                            (cond
                              (#{:as} bi) [orig]
                              (and (#{:keys} bi)
                                   (vector? v))
                              (run! (fn [bk]
                                      (when (seq (meta bk))
                                        (swap! file-map-atom assoc
                                               (select-keys (meta bk)
                                                            [:line :column
                                                             :end-line :end-column
                                                             :file])
                                               [:map-destructure-keys-enty bk])))
                                    v)
                              ;;TODO namespaced keywords, :syms :strs
                              (keyword? bi) nil
                              :else nil)))))))

(comment
  (let [a (atom {})]
    (record-destructuring
      '{:keys [a b]}
      a)
    @a)
  )

(defmethod -refactor-special 'clojure.core/let
  [_ {:keys [form] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [bindings (second form)]
    (when (and (vector? bindings)
               (even? (count bindings)))
      (let [_ (doseq [[b _] (partition 2 bindings)]
                (record-destructuring b file-map-atom))]
        (-> expr
            ana-cljc/analyze-outer
            (refactor-form* rdr-ast opt))))))

(defmethod -refactor-special :default [_ _ _ _] nil)

(defn- refactor-form* [expr rdr-ast {:keys [file-map-atom] :as opt}]
  (let [refactor-form* (fn [expr]
                         (case (::ana-cljc/op expr)
                           ::ana-cljc/unanalyzed (let [{:keys [form env]} expr
                                                       op-sym (ana/resolve-op-sym form env)]
                                                   (prn op-sym)
                                                   (when op-sym
                                                     (let [mta (meta (first form))]
                                                       (when ((every-pred :line :column) mta)
                                                         (swap! file-map-atom assoc
                                                                (select-keys mta [:line :column
                                                                                  :end-line :end-column
                                                                                  :file])
                                                                [:var op-sym]))))
                                                   (or (some-> op-sym (-refactor-special expr rdr-ast opt))
                                                       (recur (ana-cljc/analyze-outer expr))))
                           (-> expr
                               ana-cljc/run-pre-passes
                               (-refactor-form* rdr-ast opt)
                               ana-cljc/run-post-passes
                               ana-cljc/eval-top-level)))
        expr (refactor-form* expr)]
    expr))

(defn- file-map [form rdr-ast opt]
  (let [{:keys [file-map-atom] :as opt}
        (cond-> opt
          (not (:file-map-atom opt)) (assoc :file-map-atom (atom {})))
        env (ana/empty-env)]
    (with-bindings (ana/default-thread-bindings env)
      (ana-env/ensure (ana/global-env)
                      (-> form
                          (ana-cljc/unanalyzed-top-level env)
                          (refactor-form* rdr-ast opt))))
    @file-map-atom))

(defn- fq-rdr-ast [rdr-ast file-map opt]
  (let [fq-forms (fn fq-forms
                   ([forms] (fq-forms forms opt))
                   ([forms opt]
                    (mapv #(fq-rdr-ast % file-map opt) forms)))]
    (case (:op rdr-ast)
      (::rdr/whitespace ::rdr/number ::rdr/keyword) rdr-ast
      ::rdr/syntax-quote (update rdr-ast :forms fq-forms (assoc opt :syntax-quote true))
      (::rdr/unquote ::rdr/unquote-splicing) (update rdr-ast :forms fq-forms (dissoc opt :syntax-quote))
      ;; TODO lookup :vector-destructure and :map-destructure in file map
      (::rdr/list ::rdr/vector ::rdr/map) (update rdr-ast :forms fq-forms)
      ::rdr/symbol (let [{:keys [val]} rdr-ast]
                     (if-let [mapped (file-map (select-keys (meta val) [:line :column
                                                                        :end-line :end-column
                                                                        :file]))]
                       (case (first mapped)
                         :var (assoc rdr-ast :string (str (second mapped)))
                         (:local :binding) (assoc rdr-ast :string (-> mapped second :name str)))
                       rdr-ast)))))

(defn refactor-form-string [s opt]
  {:pre [(string? s)]}
  (with-open [rdr (-> s
                      StringReader.
                      LineNumberingPushbackReader.
                      (indexing-push-back-reader 1 "example.clj"))]
    (let [{form :val :as rdr-ast} (rdr/read+ast rdr)
          opt (cond-> opt
                (not (:file-map-atom opt)) (assoc :file-map-atom (atom {})))
          fm (file-map form rdr-ast opt)
          fq-string (rdr/ast->string (fq-rdr-ast rdr-ast fm {}))]
      fq-string)))

(comment
  (refactor-form-string "(map identity [1 2 3])" {})
  ;=> "(sequence (map identity) [1 2 3])"
  (refactor-form-string "(merge {:a 1 :b 2} {:c 3 :d 4})" {})
  ;=> "(into {:a 1 :b 2} {:c 3 :d 4})"
  (refactor-form-string "(merge {+ 1 :b 2} {:c 3 :d 4})" {})
  (refactor-form-string "(fn [])" {})
  (refactor-form-string "(fn [a] (a))" {})
  (refactor-form-string "(fn [a] (+ a 3))" {})

  (refactor-form-string "+" {})
  ;=> "clojure.core/+"
  (refactor-form-string "(defn foo [a] (+ a 3))" {})
  ;=> "(clojure.core/defn foo [a__#0] (clojure.core/+ a__#0 3))"
  (refactor-form-string "(defmacro foo [a] `(+ ~a 3))" {})
  ;=> "(clojure.core/defmacro foo [a__#0] `(+ ~a__#0 3))"
  (refactor-form-string "(defmacro foo [a] `(+ ~@a 3))" {})
  (refactor-form-string "(str (fn []))" {})
  (refactor-form-string "`fn" {})
  (let [a (atom {})]
    (refactor-form-string "(fn [{:keys [a]}] a)"
                          {:file-map-atom a})
    @a)
  )

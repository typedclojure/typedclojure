(ns typed.clj.refactor
  "Alpha"
  (:require [typed.clj.analyzer :as ana]
            [typed.cljc.analyzer :as ana-cljc]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as ana-env]
            [typed.clj.reader :as rdr]))

(declare ^:private refactor-form*)

(defmulti ^:private -refactor-form* (fn [expr rdr-ast opt] (::ana-cljc/op expr)))
(defmethod -refactor-form* :default
  [expr rdr-ast opt]
  (-> expr
      (ast/update-children #(refactor-form* % rdr-ast opt))))

(defn- refactor-form* [expr rdr-ast {:keys [macro-locations] :as opt}]
  (let [refactor-form* (fn [expr]
                         (case (::ana-cljc/op expr)
                           ::ana-cljc/unanalyzed (let [{:keys [form env]} expr
                                                       op-sym (ana/resolve-op-sym form env)]
                                                   (when op-sym
                                                     (when ((every-pred :line :column) (meta op-sym))
                                                       (swap! macro-locations assoc
                                                              (select-keys (meta op-sym) [:line :column
                                                                                          :end-line :end-column
                                                                                          :file])
                                                              op-sym)))
                                                   (recur (ana-cljc/analyze-outer expr)))
                           (-> expr
                               ana-cljc/run-pre-passes
                               (-refactor-form* rdr-ast opt)
                               ana-cljc/run-post-passes
                               ana-cljc/eval-top-level)))
        expr (refactor-form* expr)]
    expr))

(defn refactor-form-string [s opt]
  {:pre [(string? s)]}
  (let [{:keys [macro-locations] :as opt}
        (cond-> opt
          (not (:macro-locations opt)) (assoc :macro-locations (atom {})))
        {form :val :as rdr-ast} (rdr/read-string+ast s)
        env (ana/empty-env)]
    (with-bindings (ana/default-thread-bindings env)
      (ana-env/ensure (ana/global-env)
         (-> form
             (ana-cljc/unanalyzed-top-level env)
             (refactor-form* rdr-ast opt))))
    @macro-locations))

(comment
  (refactor-form-string "(map identity [1 2 3])" {})
  ;=> "(sequence (map identity) [1 2 3])"
  )

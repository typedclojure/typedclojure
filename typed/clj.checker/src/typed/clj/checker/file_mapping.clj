;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.file-mapping
  (:require [typed.clojure :as t]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.checker.check.utils :as cu]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

;(t/defalias InfoEntry '{:msg t/Str :fn-stack (t/Vec {:ftype Type :fn-expr Expr})})
;(t/defalias MappingKey '{:line Int :column Int :file Str})
;(t/defalias InfoMap (Map MappingKey (Vec InfoEntry)))

;[Expr -> InfoMap]
(defn ast->info-map
  [ast opts]
  (letfn [(mapping-key [{:keys [env] :as ast}]
            (let [ks [:line :column :file]]
              (when ((apply every-pred ks) env)
                (select-keys env ks))))]
    (case (:op ast)
      ; Functions can be checked any number of times. Each
      ; check is stored in the :clojure.core.typed/cmethods entry.
      :fn (let [method-mappings (for [method (:clojure.core.typed/cmethods ast)]
                                  (let [ftype (:clojure.core.typed/ftype method)
                                        _ (assert (r/Function? ftype))
                                        ;floc (mapping-key ast)
                                        ;_ (assert floc (select-keys (:env ast) [:line :file :column]))
                                        ]
                                    (ast->info-map method (update opts
                                                                  ; (Vec '{:ftype Type :fn-expr Expr})
                                                                  ::fn-stack (fnil conj [])
                                                                  {;:loc floc
                                                                   :name (cu/fn-self-name ast)
                                                                   :ftype ftype}))))
                v [{:expr ast
                    :fn-stack (::fn-stack opts)}]
                this-file (-> ast :env :file)
                _ (assert (string? this-file))]
            (merge
              (apply merge-with
                     (fn [a b] (vec (concat a b)))
                     method-mappings)
              (let [{:keys [line column end-line end-column]} (-> ast :form meta)]
                (if (and line column end-line end-column)
                  (merge {{:file this-file :line line :column column} v}
                         {{:file this-file :line end-line :column (dec end-column)} v})
                  (when-let [k (mapping-key ast)]
                    {k v})))))
      (apply merge 
             (concat (map #(ast->info-map % opts) (ast/children ast))
                     (let [{:keys [line column end-line end-column]} (-> ast :form meta)
                           this-file (-> ast :env :file)
                           _ (assert (string? this-file))
                           v [{:expr ast
                               :fn-stack (::fn-stack opts)}]]
                       (if (and line column end-line end-column)
                         (if (and (symbol? (:form ast))
                                  (= line end-line))
                           (for [c (range column end-column)]
                             {{:file this-file :line line :column c} v})
                           [{{:file this-file :line line :column column} v}
                            {{:file this-file 
                              :line end-line 
                              :column (dec end-column)}
                             v}])
                         (when-let [k (mapping-key ast)]
                           [{k v}]))))))))

;(defalias MsgMap (Map MappingKey Str))

;[InfoMap -> MsgMap]
(defn info-map->msg-map [info-map opts]
  (into {}
        (map
          (fn [[k v]]
            (when-some [msg (if (< 1 (count v))
                              (when-some [ms (seq
                                               (keep (fn [{:keys [expr fn-stack]}]
                                                       (let [r (u/expr-type expr)]
                                                         (when (r/TCResult? r)
                                                           (let [opts (prs/with-unparse-ns opts (cu/expr-ns expr opts))]
                                                             (str
                                                               "In context size " (count fn-stack) ":\n\t"
                                                               (pr-str (prs/unparse-type (r/ret-t r) opts))
                                                               "\n")))))
                                                     v))]
                                (apply str ms))
                              (let [{:keys [expr]} (first v)
                                    r (u/expr-type expr)]
                                (when (r/TCResult? r)
                                  (let [opts (prs/with-unparse-ns opts (cu/expr-ns expr opts))]
                                    (pr-str (prs/unparse-type (r/ret-t r) opts))))))]
              [k msg])))
        info-map))

(defn ast->file-mapping [ast opts]
  (-> ast (ast->info-map opts) info-map->msg-map))

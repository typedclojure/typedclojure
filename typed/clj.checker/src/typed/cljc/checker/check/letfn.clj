;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.letfn
  (:require [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check.utils :as cu]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.type-rep :as r]))

; annotations are in the first expression of the body (a :do)
(defn check-letfn [{:keys [bindings body] :as letfn-expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let [;; must pass over bindings first to uniquify
        bindings (mapv ana2/run-pre-passes bindings)
        inits-expected
        ;try and find annotations, and throw a delayed error if not found
        ;(this expression returns nil)
        (let [;throw away pass result so we can check body later
              body (update-in body [:statements 0] ana2/run-passes)]
          (when (and (#{:quote} (-> body :statements first :op))
                     (#{:const} (-> body :statements first :expr :op))
                     (vector? (-> body :statements first :expr :val)))
            (if-not (= (count (-> body :statements first :expr :val))
                       (count bindings))
              (do (err/tc-delayed-error "letfn requires each binding be annotated" opts)
                  nil)
              (into {}
                    (for [[nme type-syn] (map vector (map :name bindings) (-> body :statements first :expr :val))]
                      [nme (binding [prs/*parse-type-in-ns* (cu/expr-ns letfn-expr)]
                             (prs/parse-type type-syn opts))])))))]
    (if-not inits-expected
      (err/tc-delayed-error (str "letfn requires annotation, see: "
                                 (impl/impl-case :clojure 'clojure :cljs 'cljs) ".core.typed/letfn>")
                            {:return (assoc letfn-expr
                                            u/expr-type (cu/error-ret expected))}
                            opts)

      (let [cbindings
            (lex/with-locals inits-expected
              (mapv
                (fn [{:keys [name init] :as b}]
                  (let [expected-fn (inits-expected name)
                        _ (assert expected-fn (str "No expected type for " name
                                                   " " (vec (keys inits-expected))))
                        ; we already uniquified bindings above, so I don't think
                        ; we want to check the :binding node
                        cinit (check-expr init (r/ret expected-fn))]
                    (assoc b
                           :init cinit
                           u/expr-type (u/expr-type cinit))))
                bindings))

            cbody (lex/with-locals inits-expected
                    (check-expr body expected))
            unshadowed-ret (let/erase-objects (into #{} (map :name) cbindings) (u/expr-type cbody) opts)]
        (assoc letfn-expr
               :bindings cbindings
               :body cbody
               u/expr-type unshadowed-ret)))))

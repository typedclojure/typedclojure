;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.clj.ext.clojure.core__for
  "Typing rules clojure.core/for"
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.check :as check]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.ext.clojure.core__let :as ext-let]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.if :as if]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.runtime.env :as env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(defn emit-form [e opts]
  (impl/impl-case opts
    :clojure ((requiring-resolve 'typed.clj.analyzer.passes.emit-form/emit-form) e opts)
    :cljs ((requiring-resolve 'clojure.core.typed.emit-form-cljs/emit-form) e)))

;;==================
;; clojure.core/for

(defn ^:private -seqable-elem-query [opts]
  (prs/parse-type
    `(t/All [a#] [(t/Seqable a#) :-> a#])
    opts))

(defn check-list-comprehension-binder
  [{:keys [form args-syn ana-env prop-env]} {::check/keys [check-expr] :as opts}]
  {:pre [(seq? form)
         (seq form)
         (symbol? (first form))
         (map? ana-env)
         (lex/PropEnv? prop-env)]
   :post [((con/hmap-c? :prop-env lex/PropEnv? :ana-env map? :new-syms set?
                        :expanded-bindings vector? :reachable boolean?)
           %)]}
  (let [_ (assert (vector? args-syn) (str "List comprehension binder must be vector: " form))
        _ (assert (seq args-syn) (str "List comprehension binder must be non-empty: " form))
        _ (assert (even? (count args-syn)) (str "List comprehension binder must have even count: " form))
        kvs (partition 2 args-syn)
        {:keys [new-syms expanded-bindings prop-env ana-env reachable] :as res}
        (reduce (fn [{:keys [new-syms expanded-bindings prop-env ana-env reachable] :as context} [k v]]
                  {:pre [(vector? expanded-bindings)
                         (set? new-syms)
                         (some? prop-env)
                         (map? ana-env)
                         (boolean? reachable)]
                   :post [((con/maybe-reduced-c?
                             (con/hmap-c? :prop-env lex/PropEnv? :ana-env map? :new-syms set?
                                          :expanded-bindings vector? :reachable boolean?))
                           %)]}
                  ;; false should be handled in the previous iteration
                  (assert (true? reachable))
                  (case k
                    (:while :when) (let [cv (-> v
                                                (ana2/unanalyzed ana-env opts)
                                                (check-expr nil (var-env/with-lexical-env opts prop-env)))
                                         fs+ (-> cv u/expr-type r/ret-f :then)
                                         [env-thn reachable+] (if/update-lex+reachable prop-env fs+ opts)
                                         maybe-reduced (if reachable+
                                                         identity
                                                         reduced)]
                                     (-> context
                                         (update :expanded-bindings conj k (emit-form cv opts))
                                         (assoc :prop-env env-thn
                                                :reachable reachable+)
                                         maybe-reduced))
                    :let (let [bvec v
                               _ (assert (vector? bvec)
                                         (str "Expected binding vector as :let argument: " form))
                               _ (assert (even? (count bvec))
                                         (str "Uneven binding vector passed to :let: " form))
                               {:keys [new-syms prop-env ana-env expanded-bindings reachable] :as lb-res}
                               (ext-let/check-let-bindings
                                 (select-keys context #{:new-syms :prop-env :ana-env})
                                 bvec
                                 opts)
                               maybe-reduced (if reachable
                                               identity
                                               reduced)]
                           (-> context
                               (into (select-keys lb-res #{:new-syms :prop-env :ana-env :reachable}))
                               (update :expanded-bindings conj k expanded-bindings)
                               maybe-reduced))
                    (if (keyword? k)
                      (throw (Exception. (format "Invalid '%s' keyword: %s" (first form) k)))
                      (let [cv (-> v
                                   (ana2/unanalyzed ana-env opts)
                                   (check-expr nil (var-env/with-lexical-env opts prop-env)))
                            binding-ret (or (cgen/solve
                                              (u/expr-type cv)
                                              (-seqable-elem-query opts)
                                              opts)
                                            (r/ret
                                              (err/tc-delayed-error
                                                (format "Right hand side of '%s' clause must be seqable: %s"
                                                        (first form)
                                                        (-> cv u/expr-type :t (prs/unparse-type opts)))
                                                {:form v}
                                                opts)))
                            is-reachable (volatile! reachable)
                            updated-context (ext-let/update-destructure-env prop-env ana-env k nil binding-ret is-reachable opts)
                            ;; must go after update-destructure-env
                            reachable @is-reachable
                            maybe-reduced (if reachable identity reduced)]
                        (-> updated-context
                            (assoc :expanded-bindings (conj expanded-bindings k (emit-form cv opts))
                                   :reachable reachable)
                            (update :new-syms #(into new-syms %))
                            maybe-reduced)))))
                {:expanded-bindings []
                 :new-syms #{}
                 :prop-env prop-env
                 :ana-env ana-env
                 :reachable true}
                kvs)]
    (-> res
        (update :expanded-bindings ext-let/pad-vector args-syn))))

(defuspecial defuspecial__for
  "defuspecial implementation for clojure.core/for"
  [{ana-env :env :keys [form] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [_ (assert (= 3 (count form)) form)
        [args-syn body-syn] (next form)
        {:keys [new-syms expanded-bindings prop-env ana-env reachable]}
        (check-list-comprehension-binder 
          {:form form
           :args-syn args-syn
           :ana-env ana-env
           :prop-env (lex/lexical-env opts)}
          opts)
        expr (-> expr
                 (update :form
                         (fn [form]
                           (-> (map-indexed
                                 (fn [i args-syn]
                                   ;; add back short-circuited args
                                   (if (= 1 i)
                                     expanded-bindings
                                     args-syn))
                                 form)
                               (with-meta (meta form))))))]
        (if-not reachable
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/-name `t/ASeq r/-nothing)
                                      (fo/-true-proposition))
                               expected
                               opts))
          (let [body-expected (some-> expected
                                      (cgen/solve (-seqable-elem-query opts) opts))
                cbody (-> body-syn
                          (ana2/unanalyzed ana-env opts)
                          (check-expr body-expected (var-env/with-lexical-env opts prop-env)))
                unshadowed-ret (let/erase-objects new-syms (u/expr-type cbody) opts)
                expr (-> expr
                         (update :form
                                 (fn [form]
                                   (-> form
                                       vec
                                       (assoc 2 (emit-form cbody opts))
                                       list*
                                       (with-meta (meta form))))))]
            (assoc expr
                   u/expr-type (below/maybe-check-below
                                 (r/ret (c/-name `t/ASeq (r/ret-t unshadowed-ret))
                                        (fo/-true-proposition))
                                 expected
                                 opts))))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core__for
  "Typing rules clojure.core/for"
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.check :refer [check-expr]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.clj.analyzer.utils :as ana-utils]
            [typed.clj.ext.clojure.core__let :as ext-let]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.if :as if]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.utils :as u]))

;;==================
;; clojure.core/for

(defn ^:private -seqable-elem-query []
  (prs/parse-clj
    `(t/All [a#] [(t/U nil (t/Seqable a#)) :-> a#])))

;; see also clojure.core.typed.expand
(defn defuspecial__for
  "defuspecial implementation for clojure.core/for"
  [{ana-env :env :keys [form] :as expr} expected]
  (let [_ (assert (= 3 (count form)) form)
        [args-syn body-syn] (next form)
        _ (assert (vector? args-syn) args-syn)
        _ (assert (seq args-syn))
        _ (assert (even? (count args-syn)) args-syn)
        kvs (partition 2 args-syn)
        {:keys [new-syms expanded-bindings prop-env ana-env reachable]}
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
                    (:while :when) (let [cv (var-env/with-lexical-env prop-env
                                              (-> v
                                                  (ana2/unanalyzed ana-env)
                                                  check-expr))
                                         fs+ (-> cv u/expr-type r/ret-f :then)
                                         [env-thn reachable+] (if/update-lex+reachable prop-env fs+)
                                         maybe-reduced (if reachable+
                                                         identity
                                                         reduced)]
                                     (-> context
                                         (update :expanded-bindings conj k (emit-form/emit-form cv))
                                         (assoc :prop-env env-thn
                                                :reachable reachable+)
                                         maybe-reduced))
                    :let (let [bvec v
                               _ (assert (vector? bvec)
                                         (str "Expected binding vector as :let argument of clojure.core/for:" (pr-str bvec)))
                               _ (assert (even? (count bvec))
                                         (str "Uneven binding vector passed to :let in clojure.core/for: " bvec))
                               {:keys [new-syms prop-env ana-env expanded-bindings reachable] :as lb-res}
                               (ext-let/check-let-bindings
                                 (select-keys context #{:new-syms :prop-env :ana-env})
                                 bvec)
                               maybe-reduced (if reachable
                                               identity
                                               reduced)]
                           (-> context
                               (into (select-keys lb-res #{:new-syms :prop-env :ana-env :reachable}))
                               (update :expanded-bindings conj k expanded-bindings)
                               maybe-reduced))
                    (if (keyword? k)
                      (throw (Exception. (str "Invalid 'for' keyword: " k)))
                      (let [cv (var-env/with-lexical-env prop-env
                                 (-> v
                                     (ana2/unanalyzed ana-env)
                                     check-expr))
                            binding-ret (or (cgen/solve
                                              (u/expr-type cv)
                                              (-seqable-elem-query))
                                            (r/ret
                                              (err/tc-delayed-error
                                                (str "Right hand side of 'for' clause must be seqable: "
                                                     (-> cv u/expr-type :t prs/unparse-type))
                                                :form v)))
                            is-reachable (atom reachable)
                            updated-context (ext-let/update-destructure-env prop-env ana-env k nil binding-ret is-reachable)
                            ;; must go after update-destructure-env
                            reachable @is-reachable
                            maybe-reduced (if reachable identity reduced)]
                        (-> updated-context
                            (assoc :expanded-bindings (conj expanded-bindings k (emit-form/emit-form cv))
                                   :reachable reachable)
                            (update :new-syms #(into new-syms %))
                            maybe-reduced)))))
                {:expanded-bindings []
                 :new-syms #{}
                 :prop-env (lex/lexical-env)
                 :ana-env ana-env
                 :reachable true}
                kvs)
        expr (-> expr
                 (update :form
                         (fn [form]
                           (-> (map-indexed
                                 (fn [i args-syn]
                                   ;; add back short-circuited args
                                   (case i
                                     1 (ext-let/pad-vector expanded-bindings args-syn)
                                     args-syn))
                                 form)
                               (with-meta (meta form))))))]
        (if-not reachable
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/-name `t/ASeq r/-nothing))
                               expected))
          (let [body-expected (some-> expected
                                      (cgen/solve (-seqable-elem-query)))
                cbody (var-env/with-lexical-env prop-env
                        (-> body-syn
                            (ana2/unanalyzed ana-env)
                            (check-expr body-expected)))
                unshadowed-ret (let/erase-objects new-syms (u/expr-type cbody))
                expr (-> expr
                         (update :form
                                 (fn [form]
                                   (-> form
                                       vec
                                       (assoc 2 (emit-form/emit-form cbody))
                                       list*
                                       (with-meta (meta form))))))]
            (assoc expr
                   u/expr-type (below/maybe-check-below
                                 (r/ret (c/-name `t/ASeq (r/ret-t unshadowed-ret))
                                        (fo/-true-filter))
                                 expected))))))



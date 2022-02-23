;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core__let
  "Typing rules for clojure.core/let"
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.check.nth :as nth]
            [typed.clj.checker.check :refer [check-expr]]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.clj.analyzer.utils :as ana-utils]
            [typed.cljc.analyzer.passes.uniquify :as uniquify]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

;;==================
;; clojure.core/let

(def combined-env? (con/hmap-c? :prop-env lex/PropEnv?-workaround :ana-env map? :new-syms set?))

(defn bad-vector-destructure-error-msg
  "Error message when destructuring a non-sequential type.
  
  t is the string representation of the type being destructured
  and dform is the string representation of the destructuring syntax."
  [t dform]
  {:pre [(string? t)
         (string? dform)]
   :post [(string? %)]}
  (format (str "The type `%s` cannot be destructured via syntax `%s` "
               "because the type cannot be passed as the first argument of `nth`.`")
          t
          dform))

(defn update-destructure-env [prop-env ana-env lhs maybe-rhs-expr rhs-ret is-reachable]
  {:pre [(lex/PropEnv?-workaround prop-env)
         (map? ana-env)
         ((some-fn nil? map?) maybe-rhs-expr)
         (r/TCResult? rhs-ret)
         (instance? clojure.lang.IAtom2 is-reachable)]
   :post [(combined-env? %)]}
  (letfn [(upd-prop-env [prop-env uniquified-lhs rhs-ret]
            {:pre [(lex/PropEnv?-workaround prop-env)
                   (map? ana-env)
                   (simple-symbol? uniquified-lhs)
                   (r/TCResult? rhs-ret)]
             :post [(lex/PropEnv?-workaround %)]}
            (-> prop-env
                (let/update-env uniquified-lhs rhs-ret is-reachable)))
          (upd-ana-env [ana-env lhs]
            {:pre [(map? ana-env)
                   (simple-symbol? lhs)]
             :post [(map? %)]}
            (let [_uniquified-lhs (uniquify/uniquify! lhs ana-env)
                  tag (-> lhs meta :tag)]
              (-> ana-env
                  (assoc-in [:locals lhs]
                            ;; no :env in :locals
                            (cond-> {:op :binding
                                     ::ana2/op ::ana2/binding
                                     ;; TODO simulate :uniquify/uniquify-env analyzer pass when enabled
                                     :name lhs
                                     ;:init cexpr
                                     :form lhs
                                     ;:children [:init]
                                     :local :arg}
                              ;; TODO :o-tag ?
                              tag (assoc :tag ((some-fn ana-utils/maybe-class identity) tag)))))))
        ;; this is similar to analyzing a :fn :binding expr.
        ;; must uniquify! and normalize 
        (upd-combined-env [combined-env lhs rhs-ret]
          {:pre [(combined-env? combined-env)
                 (simple-symbol? lhs)
                 (r/TCResult? rhs-ret)]
           :post [(combined-env? %)]}
          (let [ana-env (upd-ana-env (:ana-env combined-env) lhs)
                uniquified-lhs (uniquify/normalize lhs ana-env)]
            (-> combined-env
                (assoc :ana-env ana-env)
                (update :prop-env upd-prop-env uniquified-lhs rhs-ret)
                (update :new-syms conj uniquified-lhs))))
        (chk-form
          ([combined-env form] (chk-form combined-env form nil))
          ([{:keys [ana-env prop-env] :as combined-env}
            form
            expected]
           {:pre [(combined-env? combined-env)
                  ((some-fn nil? r/TCResult?) expected)]
            :post [(map? %)]}
           (var-env/with-lexical-env prop-env
             (-> form
                 (ana2/unanalyzed ana-env)
                 (check-expr expected)))))
        (upd-combined-env-from-init-form [combined-env lhs init-form]
          {:pre [(combined-env? combined-env)
                 (simple-symbol? lhs) ]
           :post [(combined-env? %)]}
          (upd-combined-env combined-env
                            lhs
                            (u/expr-type
                              (chk-form combined-env
                                        init-form))))
        (upd-destructure-env-from-init-form
          [combined-env lhs init-form]
          {:pre [(combined-env? combined-env)]
           :post [(combined-env? %)]}
          (upd-destructure-env
            combined-env
            lhs
            (u/expr-type (chk-form combined-env
                                   init-form))))
        (upd-destructure-env 
          [combined-env lhs rhs-ret]
          {:pre [(combined-env? combined-env)
                 (r/TCResult? rhs-ret)]
           :post [(combined-env? %)]}
          (cond
            (simple-symbol? lhs) (upd-combined-env
                                   combined-env
                                   lhs
                                   rhs-ret)
            (vector? lhs) (let [gvec (gensym "vec__")
                                gseq (gensym "seq__")
                                gfirst (gensym "first__")
                                has-rest (some #{'&} lhs)]
                            (loop [combined-env (let [combined-env (upd-combined-env
                                                                     combined-env
                                                                     gvec
                                                                     rhs-ret)]
                                                  (if has-rest
                                                    (let [;; TODO wrap with destructuring related error handling
                                                          cseq (chk-form combined-env
                                                                         (list `seq gvec))]
                                                      (upd-combined-env
                                                        combined-env
                                                        gseq
                                                        (u/expr-type cseq)))
                                                    combined-env))
                                   n 0
                                   bs lhs
                                   seen-rest? false]
                              (if (seq bs)
                                (let [firstb (first bs)]
                                  (cond
                                    (= firstb '&) (recur (upd-destructure-env-from-init-form
                                                           combined-env
                                                           (second bs)
                                                           gseq)
                                                         n
                                                         (nnext bs)
                                                         true)
                                    (= firstb :as) (upd-destructure-env-from-init-form
                                                     combined-env
                                                     (second bs)
                                                     gvec)
                                    :else (if seen-rest?
                                            (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                            (let [combined-env
                                                  (cond-> combined-env
                                                    has-rest (-> (upd-combined-env-from-init-form
                                                                   gfirst
                                                                   `(first ~gseq))
                                                                 (upd-combined-env-from-init-form
                                                                   gseq
                                                                   `(next ~gseq))))
                                                  gvec-type (-> (chk-form combined-env
                                                                          gvec)
                                                                u/expr-type
                                                                :t)
                                                  firstb-res (if has-rest
                                                               (u/expr-type
                                                                 (chk-form combined-env
                                                                           gfirst))
                                                               (if (nth/valid-first-arg-for-3-arity-nth?
                                                                     gvec-type)
                                                                 (u/expr-type
                                                                   (chk-form combined-env
                                                                             (list `nth gvec n nil)))
                                                                 (r/ret
                                                                   (err/tc-delayed-error
                                                                     (bad-vector-destructure-error-msg
                                                                       (pr-str gvec-type)
                                                                       (pr-str lhs))
                                                                     (cond-> {}
                                                                       (contains? (meta lhs) ::internal/destructure-blame-form)
                                                                       (assoc :blame-form (-> lhs meta ::internal/destructure-blame-form)))))))]
                                              (recur (upd-destructure-env
                                                       combined-env
                                                       firstb
                                                       firstb-res)
                                                     (inc n)
                                                     (next bs)
                                                     seen-rest?)))))
                                combined-env)))
            (map? lhs)
            (let [gmap (gensym "map__")
                  gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
                  defaults (:or lhs)]
              (loop [combined-env (-> combined-env
                                      (upd-combined-env gmap rhs-ret)
                                      (upd-combined-env-from-init-form
                                        gmap
                                        ;; TODO clojure 1.11 expansion
                                        `(if (seq? ~gmap)
                                           (clojure.lang.PersistentHashMap/create (seq ~gmapseq))
                                           ~gmap))
                                      (cond->
                                        ;; TODO unit test
                                        (:as lhs) (upd-combined-env-from-init-form
                                                    (:as lhs)
                                                    gmap)))
                     bes (let [transforms
                               (reduce
                                 (fn [transforms mk]
                                   (if (keyword? mk)
                                     (let [mkns (namespace mk)
                                           mkn (name mk)]
                                       (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
                                             (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
                                             (= mkn "strs") (assoc transforms mk str)
                                             :else transforms))
                                     transforms))
                                 {}
                                 (keys lhs))]
                           (reduce
                             (fn [bes entry]
                               (reduce #(assoc %1 %2 ((val entry) %2))
                                       (dissoc bes (key entry))
                                       ((key entry) bes)))
                             (dissoc lhs :as :or)
                             transforms))]
                (if (seq bes)
                  (let [bb (key (first bes))
                        bk (val (first bes))
                        local (if (instance? clojure.lang.Named bb)
                                (with-meta (symbol nil (name bb)) (meta bb))
                                bb)
                        rhs (if (contains? defaults local)
                              (list `get gmap bk (defaults local))
                              (list `get gmap bk))]
                    (recur (if (ident? bb)
                             (upd-combined-env-from-init-form
                               combined-env
                               local
                               rhs)
                             (upd-destructure-env-from-init-form
                               combined-env
                               bb
                               rhs))
                           (next bes)))
                  combined-env)))
            :else (throw (new Exception (str "Unsupported binding form: " (pr-str lhs))))))]
    (upd-destructure-env
      {:prop-env prop-env
       :ana-env (uniquify/push-new-locals-frame ana-env)
       :new-syms #{}}
      lhs
      rhs-ret)))

(defn pad-vector
  [v p]
  {:pre [(vector? v)
         (vector? p)
         (<= (count v)
             (count p))]
   :post [(vector? %)
          (= (count %)
             (count p))]}
  (cond-> v
    (< (count v)
       (count p))
    (into (subvec p (count v)))))

(defn check-let-bindings [combined-env bvec]
  {:pre [
         (vector? bvec)
         (even? (count bvec))]
   :post [((con/hmap-c? :prop-env lex/PropEnv?-workaround :ana-env map? :new-syms set?
                        :expanded-bindings vector? :reachable boolean?)
           %)]}
  (assert (combined-env? combined-env)
          [(pr-str (mapv (fn [[k v]] [k (class v)]) combined-env))
           (pr-str combined-env)])
  (let [res (reduce
              (fn [{:keys [new-syms prop-env ana-env expanded-bindings reachable]} [lhs rhs]]
                {:pre [(boolean? reachable)
                       (set? new-syms)
                       (lex/PropEnv?-workaround prop-env)
                       (map? ana-env)
                       (vector? expanded-bindings)]
                 :post [((con/maybe-reduced-c?
                           (con/hmap-c? :prop-env lex/PropEnv?-workaround :ana-env map? :new-syms set?
                                        :expanded-bindings vector? :reachable boolean?))
                         %)]}
                (assert (true? reachable))
                (let [; check rhs
                      cexpr (var-env/with-lexical-env prop-env
                              (-> rhs
                                  (ana2/unanalyzed ana-env)
                                  check-expr))
                      inferred-tag (let [tag (:tag cexpr)]
                                     (cond-> tag
                                       (class? tag) coerce/Class->symbol))
                      ; propagate inferred tag when useful
                      tagged-lhs (cond-> lhs
                                   (and (simple-symbol? lhs)
                                        inferred-tag)
                                   (vary-meta update :tag #(or % inferred-tag)))
                      is-reachable (atom reachable)
                      updated-context (update-destructure-env prop-env ana-env tagged-lhs cexpr (u/expr-type cexpr) is-reachable)
                      ;; must go after update-destructure-env
                      reachable @is-reachable
                      maybe-reduced (if reachable identity reduced)]
                  (-> updated-context
                      (assoc :expanded-bindings (conj expanded-bindings
                                                      ;; preserve original lhs
                                                      lhs (emit-form/emit-form cexpr))
                             :reachable reachable)
                      (update :new-syms #(into new-syms %))
                      maybe-reduced)))
              (assoc combined-env
                     :reachable true
                     :expanded-bindings [])
              (partition 2 bvec))]
    (update res :expanded-bindings
            (fn [expanded-bindings]
              {:pre [(vector? expanded-bindings)]
               :post [(vector? %)
                      (= (count bvec)
                         (count %))]}
              (-> (pad-vector expanded-bindings bvec)
                  (with-meta (meta bvec)))))))

(defuspecial defuspecial__let
  "defuspecial implementation for clojure.core/let"
  [{ana-env :env :keys [form] :as expr} expected]
  (prn `defuspecial__let form)
  (let [_ (assert (next form)
                  (str "Expected 1 or more arguments to clojure.core/let: " form))
        [bvec & body-syns] (next form)
        _ (assert (vector? bvec)
                  (str "Expected binding vector as first argument of clojure.core/let:" (pr-str bvec)))
        _ (assert (even? (count bvec))
                  (str "Uneven binding vector passed to clojure.core/let: " bvec))
        {:keys [prop-env ana-env expanded-bindings new-syms reachable]}
        (check-let-bindings
          {:new-syms #{}
           :prop-env (lex/lexical-env)
           :ana-env ana-env}
          bvec)]
    (cond
      (not reachable) (assoc expr 
                             :form (-> (list* (first form)
                                              expanded-bindings
                                              body-syns)
                                       (with-meta (meta form)))
                             u/expr-type (or expected (r/ret (c/Un))))
      :else (let [cbody (var-env/with-lexical-env prop-env
                          (let [body (-> `(do ~@body-syns)
                                         (ana2/unanalyzed ana-env))]
                            (binding [vs/*current-expr* body]
                              (-> body
                                  (check-expr expected)))))
                  unshadowed-ret (let/erase-objects new-syms (u/expr-type cbody))]
              (assoc expr
                     :form (-> (list (first form)
                                     expanded-bindings
                                     (emit-form/emit-form cbody))
                               (with-meta (meta form)))
                     u/expr-type unshadowed-ret)))))

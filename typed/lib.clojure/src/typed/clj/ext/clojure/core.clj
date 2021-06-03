;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core
  "Typing rules for base Clojure distribution."
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.check :refer [check-expr defuspecial -unanalyzed-special]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.utils :as u]))

;;==================
;; clojure.core/ns


;; # slow version
;; fully expands and traverses using ana2, and then sends to Compiler.java to
;; do it all again.
#_
(defuspecial 'clojure.core/ns
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-nil
                              (fo/-FS fl/-bot fl/-top))
                       expected)))

;; # fast version
;; Delegates (possible) expansion and evaluation to Compiler.java.
;; Always returns nil.

;; TODO could improve error messages. old c.c.t.expand approach used:
;; (check-expected
;;   nil
;;   {:msg-fn (fn [_#]
;;              "This 'ns' expression returns nil, which does not agree with the expected type.")
;;    :blame-form ~&form})
(defmethod -unanalyzed-special 'clojure.core/ns
  [expr expected]
  (-> expr
      ana2/eval-top-level
      (assoc
        u/expr-type (below/maybe-check-below
                      (r/ret r/-nil
                             (fo/-FS fl/-bot fl/-top))
                      expected)
        :tag nil)))

(comment
        (binding [*ns* *ns*]
          (t/cf (ns foo)))
        (binding [*ns* *ns*]
          (t/check-form-info '(ns foo)
                             :expected 't/Str
                             :type-provided? true))
        )

;;==================
;; clojure.core/defmacro

(defuspecial 'clojure.core/defmacro
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-any)
                       expected)))

;;==================
;; clojure.core/let

(def ^:private combined-env? (con/hmap-c? :prop-env lex/PropEnv? :ana-env map? :new-syms set?))

(defn ^:private update-destructure-env [prop-env ana-env lhs rhs-ret is-reachable]
  {:pre [(lex/PropEnv? prop-env)
         (map? ana-env)
         (r/TCResult? rhs-ret)
         (instance? clojure.lang.IAtom2 is-reachable)]
   :post [(combined-env? %)]}
  (let [upd-prop-env (fn [prop-env lhs rhs-ret]
                       {:pre [(lex/PropEnv? prop-env)
                              (simple-symbol? lhs)
                              (r/TCResult? rhs-ret)]
                        :post [(lex/PropEnv? %)]}
                       (-> prop-env
                           (let/update-env lhs rhs-ret is-reachable)))
        upd-ana-env (fn [ana-env lhs]
                      {:pre [(map? ana-env)
                             (simple-symbol? lhs)]
                       :post [(map? %)]}
                      (-> ana-env
                          (assoc-in [:locals lhs]
                                    ;; no :env in :locals
                                    {:op :binding
                                     ::ana2/op ::ana2/binding
                                     :name lhs
                                     ;:init cexpr
                                     :form lhs
                                     ;:children [:init]
                                     :local :fn})))
        upd-combined-env (fn [combined-env lhs rhs-ret]
                           {:pre [(combined-env? combined-env)
                                  (simple-symbol? lhs)
                                  (r/TCResult? rhs-ret)]
                            :post [(combined-env? %)]}
                           (-> combined-env
                               (update :prop-env upd-prop-env lhs rhs-ret)
                               (update :ana-env upd-ana-env lhs)
                               (update :new-syms conj lhs)))
        chk-form (fn [{:keys [ana-env prop-env] :as combined-env}
                      form
                      expected]
                   {:pre [(combined-env? combined-env)
                          ((some-fn nil? r/TCResult?) expected)]
                    :post [(map? %)]}
                   (var-env/with-lexical-env prop-env
                     (-> form
                         (ana2/unanalyzed ana-env)
                         (check-expr expected))))
        upd-destructure-env (fn upd-destructure-env 
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
                                                                                             (list `seq gvec)
                                                                                             nil)]
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
                                                        (= firstb '&) (recur (let [cseq (chk-form combined-env
                                                                                                  gseq
                                                                                                  nil)]
                                                                               (upd-destructure-env
                                                                                 combined-env
                                                                                 (second bs)
                                                                                 (u/expr-type cseq)))
                                                                             n
                                                                             (nnext bs)
                                                                             true)
                                                        (= firstb :as) (let [cvec (chk-form combined-env
                                                                                            gvec
                                                                                            nil)]
                                                                         (upd-destructure-env
                                                                           combined-env
                                                                           (second bs)
                                                                           (u/expr-type cvec)))
                                                        :else (if seen-rest?
                                                                (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                                                (let [combined-env
                                                                      (if has-rest
                                                                        (let [cfirst (chk-form combined-env
                                                                                               `(first ~gseq)
                                                                                               nil)
                                                                              cnext (chk-form combined-env
                                                                                              `(next ~gseq)
                                                                                              nil)]
                                                                          (-> combined-env
                                                                              (upd-combined-env
                                                                                gfirst
                                                                                (u/expr-type cfirst))
                                                                              (upd-combined-env
                                                                                gseq
                                                                                (u/expr-type cnext))))
                                                                        combined-env)]
                                                                  (recur (upd-destructure-env
                                                                           combined-env
                                                                           firstb
                                                                           (if has-rest
                                                                             (-> (chk-form combined-env
                                                                                           gfirst
                                                                                           nil)
                                                                                 u/expr-type)
                                                                             ;; FIXME error handling
                                                                             (-> (chk-form combined-env
                                                                                           (list `nth gvec n nil)
                                                                                           nil)
                                                                                 u/expr-type)))
                                                                         (inc n)
                                                                         (next bs)
                                                                         seen-rest?)))))
                                                    combined-env)))
                                (map? lhs)
                                (let [gmap (gensym "map__")
                                      gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
                                      defaults (:or lhs)]
                                  (loop [combined-env (let [combined-env (upd-combined-env
                                                                           combined-env
                                                                           gmap
                                                                           rhs-ret)
                                                            ccreate (chk-form combined-env
                                                                              ;; TODO clojure 1.11 expansion
                                                                              `(if (seq? ~gmap)
                                                                                 (clojure.lang.PersistentHashMap/create (seq ~gmapseq))
                                                                                 ~gmap)
                                                                              nil)
                                                            combined-env (upd-combined-env
                                                                           combined-env
                                                                           gmap
                                                                           (u/expr-type ccreate))]
                                                        (cond-> combined-env
                                                          (:as lhs) (upd-combined-env
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
                                            cbv (chk-form combined-env
                                                          (if (contains? defaults local)
                                                            (list `get gmap bk (defaults local))
                                                            (list `get gmap bk))
                                                          nil)]
                                        (recur (if (ident? bb)
                                                 (upd-combined-env
                                                   combined-env
                                                   local
                                                   (u/expr-type cbv))
                                                 (upd-destructure-env
                                                   combined-env
                                                   bb
                                                   (u/expr-type cbv)))
                                               (next bes)))
                                      combined-env)))
                                :else (throw (new Exception (str "Unsupported binding form: " lhs)))))]
    (upd-destructure-env
      {:prop-env prop-env
       :ana-env ana-env
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

;; TODO
#_
(defuspecial 'clojure.core/let
  [{ana-env :env :keys [form] :as expr} expected]
  (let [_ (assert (< 1 (count form)) form)
        [bvec & body-syns] (next form)
        _ (assert (vector? bvec) bvec)
        _ (assert (even? (count bvec)) bvec)
        is-reachable (atom true)
        [env ana-env cbindings syms]
        (reduce
          (fn [[env ana-env cbindings syms] [lhs rhs]]
            {:pre [@is-reachable
                   (lex/PropEnv? env)
                   (map? ana-env)]
             :post [((con/maybe-reduced-c? (con/hvector-c? lex/PropEnv? map? vector?)) %)]}
            (let [; check rhs
                  cexpr (var-env/with-lexical-env env
                          (-> rhs
                              (ana2/unanalyzed ana-env)
                              check-expr))
                  {:keys [prop-env ana-env new-syms]} (update-destructure-env env ana-env lhs (u/expr-type cexpr) is-reachable)
                  maybe-reduced (if @is-reachable identity reduced)]
              (maybe-reduced
                [prop-env
                 ana-env
                 (conj cbindings [lhs cexpr])
                 (into syms new-syms)])))
          [(lex/lexical-env) ana-env [] #{}]
          (partition 2 bvec))
        bvec-out-form (-> (pad-vector (into []
                                            (mapcat (fn [[lhs expr]]
                                                      [lhs (emit-form/emit-form expr)]))
                                            cbindings)
                                      bvec)
                          (with-meta (meta bvec)))
        _ (assert (= (count bvec)
                     (count bvec-out-form)))]
    (cond
      (not @is-reachable) (assoc expr 
                                 :form (-> (list* (first form)
                                                  bvec-out-form
                                                  body-syns)
                                           (with-meta (meta form)))
                                 u/expr-type (or expected (r/ret (c/Un))))
      :else (let [cbody (var-env/with-lexical-env env
                          (let [body (-> `(do ~@body-syns)
                                         (ana2/unanalyzed ana-env))]
                            (binding [vs/*current-expr* body]
                              (-> body
                                  (check-expr expected)))))
                  unshadowed-ret (let/erase-objects syms (u/expr-type cbody))]
              (assoc expr
                     :form (-> (list (first form)
                                     bvec-out-form
                                     (emit-form/emit-form cbody))
                               (with-meta (meta form)))
                     u/expr-type unshadowed-ret)))))

;;==================
;; clojure.core/for

(def ^:private -seqable-elem-query
  (prs/parse-clj
    `(t/All [a#] [(t/U nil (t/Seqable a#)) :-> a#])))

;; see also clojure.core.typed.expand
(defuspecial 'clojure.core/for
  [{ana-env :env :keys [form] :as expr} expected]
  (let [_ (assert (= 3 (count form)) form)
        [args-syn body-syn] (next form)
        _ (assert (vector? args-syn) args-syn)
        _ (assert (seq args-syn))
        _ (assert (even? (count args-syn)) args-syn)
        kvs (partition 2 args-syn)
        is-reachable (atom true)
        {:keys [new-syms expanded-bindings prop-env ana-env]}
        (reduce (fn [{:keys [new-syms expanded-bindings prop-env ana-env]} [k v]]
                  {:pre [(vector? expanded-bindings)
                         (set? new-syms)
                         (some? prop-env)
                         (map? ana-env)]
                   :post [((con/maybe-reduced-c?
                             (con/hmap-c? :prop-env lex/PropEnv? :ana-env map? :new-syms set?
                                          :expanded-bindings vector?))
                           %)]}
                  (assert @is-reachable)
                  (case k
                    (:let :while :when) (assert nil (str "TODO: " k))
                    (if (keyword? k)
                      (throw (Exception. (str "Invalid 'for' keyword: " k)))
                      (let [cv (var-env/with-lexical-env prop-env
                                 (-> v
                                     (ana2/unanalyzed ana-env)
                                     check-expr))
                            binding-ret (or (cgen/solve
                                              (u/expr-type cv)
                                              -seqable-elem-query)
                                            (r/ret
                                              (err/tc-delayed-error
                                                (str "Right hand side of 'for' clause must be seqable: "
                                                     (-> cv u/expr-type :t prs/unparse-type))
                                                :form v)))
                            updated-context (update-destructure-env prop-env ana-env k binding-ret is-reachable)
                            ;; must go after update-destructure-env
                            maybe-reduced (if @is-reachable identity reduced)]
                        (-> updated-context
                            (assoc :expanded-bindings (conj expanded-bindings k (emit-form/emit-form cv)))
                            (update :new-syms #(into new-syms %))
                            maybe-reduced)))))
                {:expanded-bindings []
                 :new-syms #{}
                 :prop-env (lex/lexical-env)
                 :ana-env ana-env}
                kvs)
        expr (-> expr
                 (update :form
                         (fn [form]
                           (-> form
                               vec
                               (update 1
                                       (fn [args-syn]
                                         ;; add back short-circuited args
                                         (pad-vector expanded-bindings args-syn)))
                               list*
                               (with-meta (meta form))))))]
        (if-not @is-reachable
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (r/ret (c/-name `t/ASeq r/-nothing))
                               expected))
          (let [body-expected (some-> expected
                                      (cgen/solve -seqable-elem-query))
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

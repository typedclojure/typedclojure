;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.check-cljs
  (:require [cljs.analyzer.api :as ana-api]
            [cljs.analyzer :as cljs-ana]
            [clojure.java.io :as io]
            [clojure.core.typed :as t]
            [clojure.core.typed.analyze-cljs :as ana]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.check.dot-cljs :as dot]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.core.typed.util-cljs :as uc]
            [clojure.core.typed.util-vars :as vs]
            [clojure.tools.reader :as reader]
            [clojure.string :as c-str]
            [typed.clj.checker.constant-type :as constant-type]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.clj.checker.tc-equiv :as equiv]
            [typed.cljc.checker.ns-deps-utils :as ns-depsu]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.binding :as binding]
            [typed.cljc.checker.check.const :as const]
            [typed.cljc.checker.check.def :as def]
            [typed.cljc.checker.check.do :as do]
            [typed.cljc.checker.check.fn :as fn]
            [typed.cljc.checker.check.fn-method-utils :as fn-method-u]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.if :as if]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check.letfn :as letfn]
            [typed.cljc.checker.check.local :as local]
            [typed.cljc.checker.check.loop :as loop]
            [typed.cljc.checker.check.map :as map]
            [typed.cljc.checker.check.print-env :as pr-env]
            [typed.cljc.checker.check.quote :as quote]
            [typed.cljc.checker.check.recur :as recur]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.check.set :as set]
            [typed.cljc.checker.check.set-bang :as set!]
            [typed.cljc.checker.check.special.fn :as special-fn]
            [typed.cljc.checker.check.special.loop :as special-loop]
            [typed.cljc.checker.check.throw :as throw]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.check.vector :as vec]
            [typed.cljc.checker.check.with-meta :as with-meta]
            [typed.cljc.checker.filter-ops :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as f]
            [typed.cljc.checker.inst :as inst]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.local-result :as local-result]
            [typed.cljc.checker.object-rep :a obj]
            [typed.cljc.checker.object-rep :as o]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r :refer [ret ret-t ret-o]]
            [typed.cljc.checker.utils :as u :refer [expr-type]]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.analyzer :as ana2]
            [typed.cljs.analyzer :as tana2]))

(defmulti -check (fn [expr expected]
                   (:op expr)))

(defn check-expr
  ([expr] (check-expr expr nil))
  ([{:keys [env] :as expr} expected]
   (loop [expr expr
          fuel 1000]
     (when (neg? fuel) (prn `check-expr "infinite loop"))
     (if (= :unanalyzed (:op expr))
       (recur (tana2/analyze-outer expr) (dec fuel))
       (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
                 vs/*current-expr* expr]
         (-check expr expected))))))

(defn unanalyzed-top-level [form env]
  (tana2/unanalyzed env form))

(defn check-top-level
  "Type check a top-level form at an expected type, returning a
  fully analyzed core.typed.analyzer AST node (ie., containing no :unanalyzed nodes)
  with a u/expr-type entry giving its TCResult type, and a :result entry
  holding its evaluation result."
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env] :as opts}]
   ;(prn "check-top-level" form)
   ;(prn "*ns*" *ns*)
   ;; TODO any bindings needed to be pinned here?
   (binding [ana2/scheduled-passes {:pre identity
                                    :post identity
                                    :init-ast identity}]
     (uc/with-cljs-typed-env
       (-> form
           (unanalyzed-top-level (or env (ana-api/empty-env)))
           (check-expr expected))))))

(defn check-asts [asts]
  (mapv check-expr asts))

#_
(defn check-ns [nsym]
  {:pre [(symbol? nsym)]
   :post [(nil? %)]}
  (cu/check-ns-and-deps*
    nsym
    {:ast-for-ns ana/ast-for-ns
     :check-asts check-asts
     :check-ns check-ns}))

(t/ann ^:no-check checked-ns! [t/Sym -> nil])
(defn- checked-ns! [nsym]
  (t/when-let-fail [a vs/*already-checked*]
    (swap! a conj nsym))
  nil)

(t/ann already-checked? [t/Sym -> Boolean])
(defn already-checked? [nsym]
  (t/when-let-fail [a vs/*already-checked*]
    (boolean (@a nsym))))

(declare check-ns-and-deps)

(defn check-deps
  [nsym]
  (when (= :recheck (some-> vs/*check-config* deref :check-ns-dep))
    (checked-ns! nsym)
    ;check normal dependencies
    (doseq [dep (ns-depsu/deps-for-ns nsym)]
      ;; ensure namespace actually exists
      (when (ns-depsu/should-check-ns? nsym)
        (check-ns-and-deps dep)))))

(defn check-ns1
  "Type checks an entire namespace."
  ([ns] (check-ns1 ns (ana-api/empty-env)))
  ([ns env]
   (uc/with-cljs-typed-env
     (let [res (coerce/ns->URL ns)]
       (assert res (str "Can't find " ns " in classpath"))
       (let [filename (str res)
             path     (.getPath res)]
         (binding [cljs-ana/*file-defs*        (atom #{})
                   cljs-ana/*unchecked-if*     false
                   cljs-ana/*unchecked-arrays* false
                   cljs-ana/*cljs-warnings*    cljs-ana/*cljs-warnings*
                   cljs-ana/*cljs-ns* 'cljs.user
                   cljs-ana/*cljs-file* path
                   reader/*alias-map* (or reader/*alias-map* {})]
           (with-open [rdr (io/reader res)]
             (let [pbr (readers/indexing-push-back-reader
                         (java.io.PushbackReader. rdr) 1 filename)
                   eof (Object.)
                   read-opts (cond-> {:eof eof :features #{:cljs}}
                               (.endsWith filename "cljc") (assoc :read-cond :allow))]
               (loop []
                 (let [form (reader/read read-opts pbr)]
                   (when-not (identical? form eof)
                     (check-top-level form nil {:env (assoc env :ns cljs-ana/*cljs-ns*)})
                     (recur))))))))))))

(defn check-ns-and-deps
  "Type check a namespace and its dependencies."
  [nsym]
  {:pre [(symbol? nsym)]
   :post [(nil? %)]}
  (cond 
    (already-checked? nsym) (do
                              ;(println (str "Already checked " nsym ", skipping"))
                              ;(flush)
                              nil)
    :else
    ; check deps
    (let [_ (check-deps nsym)
          ; ignore ns declaration
          ns-form (ns-depsu/ns-form-for-ns nsym)
          check? (some-> ns-form ns-depsu/should-check-ns-form?)]
      (if-not check?
        (when-not ('#{clojure.core.typed cljs.core.typed clojure.core cljs.core} nsym)
          (println (str "Not checking " nsym 
                        (cond
                          (not ns-form) " (ns form missing)"
                          (ns-depsu/collect-only-ns? ns-form) " (tagged :collect-only in ns metadata)"
                          (not (ns-depsu/requires-tc? ns-form)) " (does not depend on clojure.core.typed)")))
          (flush))
        (let [start (. System (nanoTime))
              _ (println "Start checking" nsym)
              _ (flush)
              _ (check-ns1 nsym)
              _ (println "Checked" nsym "in" (/ (double (- (. System (nanoTime)) start)) 1000000.0) "msecs")
              _ (flush)]
          nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check CLJS AST

(defmethod -check :no-op
  [expr expected]
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected)))

(defmethod -check :const
 [{:keys [val] :as expr} expected]
 ;; FIXME probably want a custom `constant-type` function
 (const/check-const constant-type/constant-type false expr expected))

(defmethod -check :vector
  [expr expected]
  (vec/check-vector check-expr expr expected))

(defmethod -check :set
  [expr expected]
  (set/check-set check-expr expr expected))

(defmethod -check :map
  [expr expected]
  (map/check-map check-expr expr expected))

(defmethod -check :def
  [{:keys [init] :as expr} expected]
  (if init
    (def/check-normal-def check-expr expr expected)
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (ret r/-any)
                         expected))))

(defmethod -check :js
  [{:keys [js-op args env] :as expr} expected]
  (cond
    js-op (let [res (expr-type (check-expr {:op :invoke
                                            :from-js-op expr
                                            :env env
                                            :children [:fn :args]
                                            :fn {:op :var
                                                 :env env
                                                 :name js-op}
                                            :args args}
                                           expected))]
            (assoc expr
                   u/expr-type res))
    :else (do (u/tc-warning (str "js-op missing, inferring Unchecked"))
              (assoc expr
                     u/expr-type (below/maybe-check-below
                                   (r/ret (r/-unchecked))
                                   expected)))))

(defmulti invoke-special (fn [{{:keys [op] :as fexpr} :fn :as expr} & expected]
                           (when (= :var op)
                             (:name fexpr))))

(defmethod invoke-special :default [& _] ::not-special)

(defmethod invoke-special 'cljs.core.typed/print-env
  [{[{debug-string :form :as texpr} :as args] :args :as expr} & [expected]]
  (assert (= 1 (count args)))
  (assert (string? debug-string))
  ;DO NOT REMOVE
  (pr-env/print-env*)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (ret r/-any)))

(defmethod invoke-special 'cljs.core.typed/inst-poly
  [{[pexpr targs-expr :as args] :args :as expr} & [expected]]
  (assert (#{2} (count args)) "Wrong arguments to inst")
  (let [ptype (let [t (-> (check-expr pexpr) expr-type ret-t)]
                (c/fully-resolve-type t))
        _ (assert ((some-fn r/Poly? r/PolyDots?) ptype))
        targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                (mapv prs/parse-type (-> targs-expr :expr :form)))]
    (assoc expr
           expr-type (ret (inst/manual-inst ptype targs)))))

(defmethod invoke-special 'cljs.core.typed/loop>-ann
  [{[expr {{expected-bnds-syn :expr} :form}] :args :as dummy-expr} & [expected]]
  (let [expected-bnds (binding [prs/*parse-type-in-ns* (cu/expr-ns dummy-expr)]
                        (mapv prs/parse-type expected-bnds-syn))]
    ;loop may be nested, type the first loop found
    (binding [recur-u/*loop-bnd-anns* expected-bnds]
      (check-expr expr expected))))

; args are backwards if from inlining
(defmethod invoke-special 'cljs.core/instance?
  [{:keys [args] :as expr} & [expected]]
  (assert (= 2 (count args)) "Wrong arguments to instance?")
  ; are arguments the correct way round?
  (assert (:from-js-op expr) "instance? without inlining NYI")
  (binding [vs/*current-env* (:env expr)
            vs/*current-expr* expr]
    (let [target-expr (first args)
          inst-of-expr (second args)
          varsym (when (#{:var} (:op inst-of-expr))
                   (-> inst-of-expr :name))
          _ (when-not varsym
              (err/int-error (str "First argument to instance? must be a datatype var "
                                (:op inst-of-expr))))
          inst-of (c/DataType-with-unknown-params varsym)
          cexpr (check-expr target-expr)
          expr-tr (expr-type cexpr)
          final-ret (ret (r/JSBoolean-maker)
                         (fo/-FS (fo/-filter-at inst-of (ret-o expr-tr))
                                 (fo/-not-filter-at inst-of (ret-o expr-tr))))]
      (assoc expr
             expr-type final-ret))))

;=
(defmethod invoke-special 'cljs.core/= 
  [{:keys [args] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update :fn check-expr)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected)))))

(defmethod -check :invoke
  [{fexpr :fn :keys [args] :as expr} expected]
  (let [e (invoke-special expr)]
    (cond
      (not= e ::not-special) e
      :else
      (let [cfexpr (check-expr fexpr)
            cargs (mapv check-expr args)
            ftype (expr-type cfexpr)
            argtys (map expr-type cargs)
            actual (funapp/check-funapp cfexpr cargs ftype argtys expected)]
        (assoc expr
               expr-type actual)))))

;only local bindings are immutable, vars/js do not partipate in occurrence typing
(defn js-var-result [expr vname expected]
  {:pre [((every-pred symbol? namespace) vname)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (let [t (var-env/type-of vname)]
      (below/maybe-check-below
        (ret t)
        expected))))

(defmethod -check :var
  [{vname :name :as expr} expected]
  (assoc expr expr-type
         (js-var-result expr vname expected)))

;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form ::t/tc-ignore
  [expr expected]
  ;; TODO migrate to extensible type rules
  (err/nyi-error "cljs tc-ignore")
  #_(tc-ignore/check-tc-ignore check-expr expr expected))

(defn check-fn-rest [remain-dom & {:keys [rest drest kws prest]}]
  {:pre [(or (r/Type? rest)
             (r/Type? prest)
             (r/DottedPretype? drest)
             (r/KwArgs? kws))
         (#{1} (count (filter identity [rest drest kws prest])))]
   :post [(r/Type? %)]}
  ;(prn "rest" rest)
  ;(prn "drest" drest)
  ;(prn "kws" kws)
  (cond
    (or rest drest)
    (c/Un r/-nil 
          ; only difference to Clojure impl
          (r/TApp-maker (r/Name-maker 'cljs.core.typed/NonEmptySeq)
                        [(or rest (:pre-type drest))]))

    prest (err/nyi-error "NYI handle prest in CLJS")
    :else (c/KwArgs->Type kws)))

(defmacro prepare-check-fn [& body]
  `(binding [fn-method-u/*check-fn-method1-checkfn* check-expr
             fn-method-u/*check-fn-method1-rest-type* check-fn-rest]
     ~@body))

(defmethod internal-special-form ::t/fn
  [{[_ _ {{fn-anns :ann} :val} :as statements] :statements fexpr :ret :as expr} expected]
  (prepare-check-fn
    (special-fn/check-special-fn check-expr expr expected)))

(defmethod internal-special-form ::t/ann-form
  [{[_ _ {{tsyn :type} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  ;; TODO migrate to extensible type rules
  (err/nyi-error "cljs ann-form")
  #_(ann-form/check-ann-form check-expr expr expected))

(defmethod internal-special-form ::t/loop
  [{[_ _ {{tsyns :ann} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (special-loop/check-special-loop check-expr expr expected))

(defmethod internal-special-form :default
  [expr expected]
  (err/int-error (str "No such internal form: " (ast-u/emit-form-fn expr))))

(defmethod -check :do
  [expr expected]
  (do/check-do check-expr internal-special-form expr expected))

(defmethod -check :fn
  [{:keys [methods] :as expr} expected]
  (prepare-check-fn
    (if expected
      (fn/check-fn expr expected)
      (special-fn/check-core-fn-no-expected check-expr expr))))

(defmethod -check :set!
  [{:keys [target val] :as expr} expected]
  (set!/check-set! check-expr expr expected))

(defmethod -check :if
  [{:keys [test then else] :as expr} expected]
  (if/check-if check-expr expr expected))

(defmethod -check :let
  [expr expected]
  (let/check-let check-expr expr expected))

(defmethod -check :letfn
  [{:keys [bindings body env] :as expr} expected]
  (letfn/check-letfn bindings body expr expected check-expr))

(defmethod -check :recur
  [{:keys [exprs env] :as recur-expr} expected]
  (recur/check-recur exprs env recur-expr expected check-expr))

(defmethod -check :loop
  [loop-expr expected]
  (loop/check-loop check-expr loop-expr expected))

(defmethod -check :ns
  [expr expected]
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected)))

(defmethod -check :ns*
  [expr expected]
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret r/-any)
                       expected)))

(defmethod -check :binding
  [expr expected]
  (binding/check-binding check-expr expr expected))

(defmethod -check :quote
  [expr expected]
  (quote/check-quote check-expr constant-type/constant-type expr expected))

;; adding a bunch of missing methods: 

(defn fail-empty [expr]
  (throw (Exception. (str "Not implemented, yet: " (:op expr)))))

(defmethod -check :new
  [{:keys [ctor args] :as expr} expected]
  (let [;; TODO check ctor
        cargs (mapv check-expr args)]
    (u/tc-warning (str "`new` special form is Unchecked"))
    (assoc :args cargs
           u/expr-type (below/maybe-check-below
                         ;; TODO actual checks
                         (r/ret (r/-unchecked))
                         expected))))

;; TODO does this actually work?
#_
(defmethod -check :case
  [{:keys [test nodes default :as expr]} expected]
  (chk/check ;;TODO what `check` is this?
   {:op :case
    :test test
    :default default
    :tests (mapcat :tests nodes)
    :thens (map :then nodes)}
   expected))

(defmethod -check :case-node
  [expr expected]
  (fail-empty expr))

(defmethod -check :case-test
  [expr expected]
  (fail-empty expr))

(defmethod -check :case-then
  [expr expected]
  (fail-empty expr))

;TODO
(defmethod -check :defrecord
  [expr expected]
  (u/tc-warning (str "`defrecord` special form is Unchecked"))
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

(defmethod -check :deftype
  [expr expected]
  (u/tc-warning (str "`deftype` special form is Unchecked"))
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

(defmethod -check :fn-method
  [expr expected]
  (fail-empty expr))

; see clojure.core.typed.check.dot-cljs
;; TODO check
(defmethod -check :host-call
  [{:keys [method target args] :as expr} expected]
  (let [ctarget (check-expr target)
        cargs (mapv check-expr args)]
    (u/tc-warning (str "`.` special form is Unchecked"))
    (assoc expr 
           :target ctarget
           :args cargs
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected))))

; see clojure.core.typed.check.dot-cljs
;; TODO check
(defmethod -check :host-field
  [{:keys [target] :as expr} expected]
  (let [ctarget (check-expr target)]
    (u/tc-warning (str "`.` special form is Unchecked"))
    (assoc expr 
           :target ctarget
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected))))

;; TODO check
(defmethod -check :js-array
  [{:keys [items] :as expr} expected]
  (let [citems (mapv check-expr items)]
    (u/tc-warning (str "`#js []` special form is Unchecked"))
    (assoc expr 
           :items citems
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected))))

(defmethod -check :js-object
  [{:keys [keys vals] :as expr} expected]
  (let [cvals (mapv check-expr vals)]
    (assoc expr
           :vals cvals
           u/expr-type (below/maybe-check-below
                         (r/ret (r/JSObj-maker (zipmap (map keyword keys)
                                                       (map (comp r/ret-t u/expr-type) cvals)))
                                (fo/-true-filter))
                         expected))))

; TODO check
(defmethod -check :js-var
  [{:keys [name] :as expr} expected]
  (u/tc-warning (str "Assuming JS variable is unchecked " name))
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

(defmethod -check :local
  [expr expected]
  (local/check-local expr expected))


; TODO check
(defmethod -check :the-var
  [expr expected]
  (u/tc-warning (str "`var` special form is Unchecked"))
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

(defmethod -check :throw
  [expr expected]
  (throw/check-throw check-expr expr expected nil))

; TODO check
(defmethod -check :try
  [expr expected]
  (fail-empty expr))

(defmethod -check :with-meta
  [expr expected]
  (with-meta/check-with-meta check-expr expr expected))

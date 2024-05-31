;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.check
  (:refer-clojure :exclude [delay])
  (:require [cljs.analyzer :as cljs-ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.tagged-literals :as tags]
            [cljs.util :as cljs-util]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.check.dot-cljs :as dot]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io]
            [clojure.string :as c-str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.clj.checker.tc-equiv :as equiv]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-impl :refer [-check]]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.def :as def]
            [typed.cljc.checker.check.print-env :as pr-env]
            [typed.cljc.checker.check.special.loop :as special-loop]
            [typed.cljc.checker.check.unanalyzed :as unanalyzed]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as f]
            [typed.cljc.checker.inst :as inst]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.local-result :as local-result]
            [typed.cljc.checker.ns-deps-utils :as ns-depsu]
            [typed.cljc.checker.object-rep :as o]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r :refer [ret ret-t ret-o]]
            [typed.cljc.checker.utils :as u :refer [expr-type]]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljs.analyzer :as tana2]
            [typed.cljs.checker.jsnominal-env :as jsnom]
            [typed.cljs.checker.util :as uc]
            [typed.clojure :as t]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]))

(def ^:private *register-exts (delay
                                (configs/register-cljs-config-anns)
                                (configs/register-cljs-config-exts)))

(declare check-expr)

(defn maybe-check-unanalyzed [{:keys [form env] :as expr} expected]
  (binding [vs/*current-expr* expr]
    (or (unanalyzed/-unanalyzed-special expr expected)
        ;; don't expand macros that inline raw js
        (when-some [rsym (when (seq? form) (ana2/resolve-sym (first form) env))]
          (when-some [cljsvar-ann (var-env/type-of-nofail rsym)]
            ;(prn "cljsvar-ann" rsym cljsvar-ann)
            (let [macro-var (find-var rsym)]
              (when (and (var? macro-var)
                         (-> macro-var meta :macro))
                (check-expr (assoc expr :form (-> form
                                                  vec
                                                  (update 0 #(with-meta (list 'do %) {::fake-do true}))
                                                  list*
                                                  (with-meta (meta form))))
                            expected))))))))

(defn check-expr
  ([expr] (check-expr expr nil))
  ([{:keys [env] :as expr} expected]
   (loop [expr expr
          fuel 1000]
     (when (neg? fuel) (prn `check-expr "infinite loop"))
     #_
     (prn `check-expr "op" (:op expr) (:form expr)
          cljs-ana/*cljs-ns*
          )
     ;; should really be a map. worth asserting this at some point.
     (assert (not (symbol? (-> expr :env :ns)))
             (str "MALFORMED :env :ns " (pr-str (-> expr :env :ns))))
     (if (= :unanalyzed (:op expr))
       (do @*register-exts
           (or (maybe-check-unanalyzed expr expected)
               (recur (tana2/analyze-outer expr) (max -1 (dec fuel)))))
       (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
                 vs/*current-expr* expr]
         (-check expr expected))))))

(defn unanalyzed-top-level [form env]
  (tana2/unanalyzed form env))

(defn flush-analysis-side-effects [cexpr opts]
  (reduce (fn [ast pass] (pass (:env ast) ast opts))
          (ast/walk cexpr
                    identity
                    ana2/analyze-outer-root)
          cljs-ana/default-passes))

(defn check-top-level
  "Type check a top-level form at an expected type, returning a
  fully analyzed core.typed.analyzer AST node (ie., containing no :unanalyzed nodes)
  with a u/expr-type entry giving its TCResult type, and a :result entry
  holding its evaluation result."
  ([form] (check-top-level form nil))
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env] :as opts}]
   ;(prn "check-top-level" form)
   ;(prn "*ns*" *ns*)
   ;(prn "*cljs-ns*" cljs-ana/*cljs-ns*)
   ;; TODO any bindings needed to be pinned here?
   (binding [ana2/scheduled-passes {:pre identity
                                    :post identity
                                    :init-ast identity}
             check/check-expr check-expr]
     (let [cexpr (uc/with-cljs-typed-env
                   (-> form
                       (unanalyzed-top-level (or env (ana-api/empty-env)))
                       (check-expr expected)))]
       (flush-analysis-side-effects cexpr opts)
       cexpr))))

(defn check-asts [asts]
  (mapv check-expr asts))

(defn check-ns1
  "Type checks an entire namespace."
  ([ns] (check-ns1 ns (ana-api/empty-env)))
  ([ns env]
   (uc/with-cljs-typed-env
     (let [res (coerce/ns->URL ns)]
       (assert res (str "Can't find " ns " in classpath"))
       (let [filename (str res)
             path     (.getPath res)]
         (uc/with-analyzer-bindings*
           path
           (fn []
             (with-open [rdr (io/reader res)]
               (let [pbr (readers/indexing-push-back-reader
                           (java.io.PushbackReader. rdr) 1 filename)
                     data-readers (merge tags/*cljs-data-readers*
                                         (cljs-ana/load-data-readers))
                     eof (Object.)
                     read-opts (cond-> {:eof eof :features #{:cljs}}
                                 (.endsWith filename "cljc") (assoc :read-cond :allow))]
                 (loop []
                   (let [form (binding [*ns* (do (when (:check-form-eval vs/*check-config*)
                                                   ;; see clj implementation
                                                   (err/nyi-error ":check-form-eval in CLJS"))
                                                 (create-ns cljs-ana/*cljs-ns*))
                                        reader/*data-readers* data-readers
                                        reader/*alias-map* (uc/get-aliases)]
                                (reader/read read-opts pbr))]
                     (when-not (identical? form eof)
                       (check-top-level form)
                       (recur)))))))))))))

(defn check-ns-and-deps [nsym] (cu/check-ns-and-deps nsym check-ns1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check CLJS AST

(defmethod -check ::tana2/no-op
  [expr expected]
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected)))

(defn check-def
  [{:keys [init] :as expr} expected]
  (if init
    (def/check-normal-def expr expected)
    (assoc expr
           u/expr-type (below/maybe-check-below
                         (ret r/-any)
                         expected))))

(defmethod -check ::tana2/js
  [{:keys [js-op args env] :as expr} expected]
  (cond
    js-op (let [res (expr-type (check-expr {:op :invoke
                                            ::ana2/op ::ana2/invoke
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
    :else (do (u/tc-warning (str "js-op missing, inferring Any"))
              (assoc expr
                     u/expr-type (below/maybe-check-below
                                   (r/ret r/-any)
                                   expected)))))

(defmulti invoke-special (fn [{{:keys [op] :as fexpr} :fn :keys [env] :as expr} _expected]
                           (case op
                             :var (:name fexpr)
                             :unanalyzed (let [{:keys [form]} fexpr]
                                           (when (symbol? form)
                                             (ana2/resolve-sym form env)))
                             nil)))

(defmethod invoke-special :default [expr expected])

(defmethod invoke-special 'cljs.core.typed/print-env
  [{[{debug-string :form :as texpr} :as args] :args :as expr} expected]
  (assert (= 1 (count args)))
  (assert (string? debug-string))
  ;DO NOT REMOVE
  (pr-env/print-env*)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected)))

; args are backwards if from inlining
(defmethod invoke-special 'cljs.core/instance?
  [{:keys [args] :as expr} expected]
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
  [{:keys [args] :as expr} expected]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check-expr args)]
    (-> expr
        (update :fn check-expr)
        (assoc :args cargs
               u/expr-type (equiv/tc-equiv := (map u/expr-type cargs) expected)))))


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

(defn check-var
  [{vname :name :as expr} expected]
  (impl/assert-cljs)
  (assoc expr expr-type
         (js-var-result expr vname expected)))

;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form :clojure.core.typed/loop
  [{[_ _ {{tsyns :ann} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (special-loop/check-special-loop check-expr expr expected))

(defmethod internal-special-form :default
  [expr expected]
  (err/int-error (str "No such internal form: " (ast-u/emit-form-fn expr))))



(defmethod -check ::tana2/ns
  [expr expected]
  (assoc expr
         expr-type (below/maybe-check-below
                     (ret r/-any)
                     expected)))

(defmethod -check ::tana2/ns*
  [expr expected]
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret r/-any)
                       expected)))


;; adding a bunch of missing methods: 

(defn fail-empty [expr]
  (throw (Exception. (str "Not implemented, yet: " (:op expr)))))

(defn check-new
  [{ctor :class :keys [args] :as expr} expected]
  (impl/assert-cljs)
  (let [;; TODO check ctor
        cargs (mapv check-expr args)]
    (u/tc-warning (str "`new` special form is Unchecked"))
    (-> expr
        (assoc :args cargs
               u/expr-type (below/maybe-check-below
                             ;; TODO actual checks
                             (r/ret (r/-unchecked 'new))
                             expected)))))

;; TODO does this actually work?
#_
(defmethod -check ::tana2/case
  [{:keys [test nodes default :as expr]} expected]
  (chk/check ;;TODO what `check` is this?
   {:op :case
    :test test
    :default default
    :tests (mapcat :tests nodes)
    :thens (map :then nodes)}
   expected))

(defmethod -check ::tana2/case-node
  [expr expected]
  (fail-empty expr))

(defmethod -check ::tana2/case-test
  [expr expected]
  (fail-empty expr))

(defmethod -check ::tana2/case-then
  [expr expected]
  (fail-empty expr))

;TODO
(defmethod -check ::tana2/defrecord
  [expr expected]
  (u/tc-warning (str "`defrecord` special form is Unchecked"))
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

(defmethod -check ::tana2/deftype
  [expr expected]
  (u/tc-warning (str "`deftype` special form is Unchecked"))
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

; see clojure.core.typed.check.dot-cljs
;; TODO check
(defmethod -check ::tana2/host-call
  [{:keys [method target args] :as expr} expected]
  (let [ctarget (check-expr target)
        cargs (mapv check-expr args)]
    #_(dot/check-dot ...)
    (u/tc-warning (str "`.` special form is Unchecked"))
    (assoc expr 
           :target ctarget
           :args cargs
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected))))

; see clojure.core.typed.check.dot-cljs
;; TODO check
(defmethod -check ::tana2/host-field
  [{:keys [target] :as expr} expected]
  (let [ctarget (check-expr target)]
    #_(dot/check-dot ...)
    (u/tc-warning (str "`.` special form is Unchecked"))
    (assoc expr 
           :target ctarget
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected))))

;; TODO check
(defmethod -check ::tana2/js-array
  [{:keys [items] :as expr} expected]
  (let [citems (mapv check-expr items)]
    #_(dot/check-dot ...)
    (u/tc-warning (str "`#js []` special form is Unchecked"))
    (assoc expr 
           :items citems
           u/expr-type (below/maybe-check-below
                         (r/ret (r/-unchecked))
                         expected))))

(defmethod -check ::tana2/js-object
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
(defmethod -check ::tana2/js-var
  [{:keys [name] :as expr} expected]
  (u/tc-warning (str "Assuming JS variable is unchecked " name))
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))


; TODO check
(defn check-the-var
  [expr expected]
  (impl/assert-cljs)
  (u/tc-warning (str "`var` special form is Unchecked"))
  (assoc expr 
         u/expr-type (below/maybe-check-below
                       (r/ret (r/-unchecked))
                       expected)))

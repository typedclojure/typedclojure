;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.cache
  (:require [typed.cljc.checker.check :as check]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.var-env :as var-env]
            [clojure.pprint :as pp]
            [typed.cljc.runtime.env :as env]
            [clojure.walk :as walk]
            [clojure.core.typed.util-vars :as uvs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.analyzer :as ana2]))

(defn monitored-checker-map [m callback path]
  (reify
    clojure.lang.IPersistentMap
    (containsKey [_ k] (let [res (.containsKey ^clojure.lang.Associative m k)]
                         (when res (callback (conj path k) nil))
                         res))
    clojure.lang.IFn
    (invoke [this k] (.valAt this k))
    (iterator [_] (let [res (.iterator ^java.util.Iterator m)]
                    ;;TODO
                    (callback (conj path ::ALL) nil)
                    res))
    clojure.lang.ILookup
    (entryAt [_ k] (let [r (find m k)
                         path (conj path k)]
                     (if r
                       (if (map? (nth r 1))
                         (update r 1 monitored-checker-map callback path)
                         (do (callback path (val r))
                             r))
                       (do (callback path nil)
                           r))))
    (valAt [_ k] (let [r (get m k)
                       path (conj path k)]
                   (if (map? r)
                     (monitored-checker-map r callback path)
                     (do (when (some? r) (callback path r))
                         r))))
    (valAt [_ k not-found] (let [unique (Object.)
                                 r (get m k unique)]
                             (if (identical? r unique)
                               not-found
                               (let [path (conj path k)]
                                 (if (map? r)
                                   (monitored-checker-map r callback path)
                                   (do (when (some? r) (callback path r))
                                       r))))))))

(defn with-recorded-deps [expr expected {::uvs/keys [delayed-errors]
                                         ::env/keys [checker]
                                         ::check/keys [check-expr] :as opts}]
  (assert check-expr)
  (assert checker (vec (keys opts)))
  (let [errors (volatile! nil)
        types (atom {})
        vars (atom {})
        interop (atom {})
        type-syms (atom {})
        ->serialize (fn [t]
                      (let [t (force-type t opts)]
                        (or #_(some-> t meta :pretty (get t) :no-simpl-verbose-syntax deref)
                            (if (r/Type? t)
                              (prs/unparse-type t (assoc opts ::uvs/verbose-types true))
                              t))))
        instrumented-checker (reify
                               clojure.lang.IAtom2
                               (swapVals [_ f] (.swapVals checker f))
                               (swapVals [_ f arg] (.swapVals checker f arg))
                               (swapVals [_ f arg1 arg2] (.swapVals checker f arg1 arg2))
                               (swapVals [_ f x y args] (.swapVals checker f x y args))
                               (swap [_ f] (.swap checker f))
                               (swap [_ f arg] (.swap checker f arg))
                               (swap [_ f arg1 arg2] (.swap checker f arg1 arg2))
                               (swap [_ f x y args] (.swap checker f x y args))
                               (compareAndSet [_ old new] (.compareAndSet checker old new))
                               (reset [_ new] (.reset checker new))
                               clojure.lang.IDeref
                               (deref [_] (monitored-checker-map @checker #(swap! types update-in %1 (fn [prev] (or prev (->serialize %2) {}))) [])))
        check-expr (comp (fn [{:keys [op] :as cexpr}]
                           (when (impl/checking-clojure? opts)
                             ;;TODO resolve actual method signatures
                             (case op
                               (:instance-call :static-call) (swap! interop update op (fnil conj #{}) (cu/MethodExpr->qualsym cexpr opts))
                               (:instance-field :static-field) (swap! interop update op (fnil conj #{}) (cu/FieldExpr->qualsym cexpr opts))
                               :new (swap! interop update op (fnil conj #{}) (cu/NewExpr->qualsym cexpr))
                               nil))
                           cexpr)
                         check-expr)
        opts (-> opts
                 (assoc ::check/check-expr check-expr)
                 (assoc ::env/checker instrumented-checker)
                ;; preserve the namespace resolutions that occur while parsing during type checking, like t/ann-form
                 (assoc ::prs/resolve-type-clj->sym
                        (let [resolve-type-clj->sym prs/-resolve-type-clj->sym]
                          (fn __resolve-type-clj->sym [sym opts]
                            (let [res (resolve-type-clj->sym sym opts)]
                              (when (not= res sym)
                                (swap! type-syms assoc-in [(prs/parse-in-ns opts) sym] res))
                              res))))
                 (assoc ::prs/resolve-type-clj
                        (let [resolve-type-clj prs/-resolve-type-clj]
                          (fn __resolve-type-clj [sym opts]
                            (let [res (resolve-type-clj sym opts)]
                              (when res
                                (let [res (cond
                                            (var? res) (symbol res)
                                            (class? res) (-> ^Class res .getName symbol)
                                            :else (assert nil (str "WIP prs/resolve-type-clj to sym: " sym " " res)))]
                                  (assert (symbol? res))
                                  (when (not= res sym)
                                    (swap! type-syms assoc-in [(prs/parse-in-ns opts) sym] res))))
                              res))))
                 (assoc ::prs/parse-type-symbol-default
                        (let [parse-type-symbol-default prs/-parse-type-symbol-default]
                          (fn [sym opts]
                            (let [res (parse-type-symbol-default sym opts)]
                              (let [rep (->serialize res)]
                                (when (not= rep sym)
                                  (swap! type-syms assoc-in [(prs/parse-in-ns opts) sym] rep)))
                              res)))))]
    (binding [ana2/resolve-sym (let [resolve-sym ana2/resolve-sym]
                                 (fn [sym env]
                                   (let [r (resolve-sym sym env)]
                                     (when r
                                       (let [v (if (ana2/var? r opts)
                                                 (ana2/var->sym r opts)
                                                 (if (class? r)
                                                   (coerce/Class->symbol r)
                                                   r))]
                                         (when (not= v sym)
                                           (swap! vars assoc sym v))))
                                     r)))]
      (let [result (check-expr expr expected opts)]
        (assoc result ::cache-info {::types (dissoc @types :clojure.core.typed.current-impl/current-nocheck-var?)
                                    ::vars @vars ::errors (pos? (count @delayed-errors)) ::interop @interop
                                    ::type-syms @type-syms})))))

(defn ns-check-cached? [checker nsym slurped]
  {:pre [(simple-symbol? nsym)
         (string? slurped)]
   :post [(boolean? %)]}
  (if-some [{::keys [types] :as cache-info} (get-in (env/deref-checker checker) [::check-ns-cache nsym])]
    (do ;(println "top level info" types)
        (and (= slurped (:slurped cache-info))
             ;;TODO include the source of the namespaces that declare the relevant types and rules
             ;; so we just need to compare the files at this level
             ;(= (:clojure.core.typed.current-impl/unanalyzed-special ))
             ;;DISABLE comment this out until the cache works!!
             false)
        )
    false))

(defn cache-info-id [env {::ana2/keys [current-ns-name]
                          :keys [top-level-form-string ns-form-string] :as opts}]
  [::check-form-cache (current-ns-name env opts) ns-form-string top-level-form-string])

(defn retrieve-form-cache-info [{:keys [env] :as expr}
                                expected
                                {:keys [top-level-form-string ns-form-string] :as opts}]
  (get-in (env/deref-checker (env/checker opts)) (cache-info-id env opts)))

(defn need-to-check-top-level-expr? [expr expected {:keys [top-level-form-string ns-form-string] :as opt} opts]
  (or (-> *ns* meta :typed.clojure :experimental :cache not)
      (some? expected)
      (not ns-form-string)
      (not top-level-form-string)
      (if-some [cache-info (retrieve-form-cache-info expr expected opts)]
        (do
          (println "need-to-check-top-level-expr?: found cache info")
          ;;TODO compare cache-info to current environment
          true)
        (do
          (println "need-to-check-top-level-expr?: did not find cache info")
          true))))

(defn- record-cache! [{::keys [cache-info env] :as cexpr}
                      {:keys [form env] :as original}
                      {:keys [top-level-form-string ns-form-string] :as opt}
                      opts]
  (when (-> *ns* meta :typed.clojure :experimental :cache)
    (if (::errors cache-info)
      (println "cache: Not caching form due to type error")
      (println "cache: Caching form with cache info"))
    (binding [*print-namespace-maps* false
              *print-level* 10
              *print-length* 10]
      (println (str "ns form:\n>>>>\n" ns-form-string "\n<<<<"))
      (println (str "cache: on disk:\n>>>>\n" (if (and (seq? form)
                                                       (= #'comment (-> (first form) (ana2/resolve-sym env))))
                                                (str (subs top-level-form-string 0 (min 10 (count top-level-form-string))) "...")
                                                top-level-form-string)
                    "\n<<<<")))
    (binding [*print-namespace-maps* false
              *print-level* nil
              *print-length* nil]
      (pp/pprint cache-info)))
  ;; communicate with need-to-check-top-level-expr?
  (env/swap-checker! (env/checker opts) assoc-in (cache-info-id env opts) cache-info))

(defn remove-stale-cache-entries [nsym ns-form-str sforms slurped opts]
  {:pre [(simple-symbol? nsym)]}
  (when ns-form-str
    (let [{{{forms-cache ns-form-str} nsym} ::check-form-cache}
          (env/swap-checker! (env/checker opts) update-in [::check-form-cache nsym]
                             (fn [m]
                               (some-> m
                                       (select-keys [ns-form-str])
                                       not-empty
                                       (update ns-form-str select-keys sforms))))]
      (env/swap-checker! (env/checker opts) assoc-in [::check-ns-cache nsym]
                         (when (not-any? ::errors (vals forms-cache))
                           (-> (apply merge-with merge (map #(dissoc % :clojure.core.typed.current-impl/current-used-vars :clojure.core.typed.current-impl/current-impl
                                                                     ;;TODO
                                                                     :typed.cljc.checker.check.cache/errors)
                                                            (vals forms-cache)))
                               (assoc :slurped slurped)))))))

(defn check-top-level-expr [expr expected opt opts]
  (if (need-to-check-top-level-expr? expr expected opt opts)
    (let [cexpr (with-recorded-deps expr expected opts)]
      (when (not expected) (record-cache! cexpr expr opt opts))
      cexpr)
    expr))

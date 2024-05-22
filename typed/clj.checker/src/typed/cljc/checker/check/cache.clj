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

(defn cache-id
  [expr expected opts]
  (str (random-uuid)
       "#:;-;:#"
       (-> expr :env :ns)
       "#:;-;:#"
       (when (some? expected)
         (random-uuid))
       "#:;-;:#"
       (when (-> expr :env :locals seq)
         (random-uuid))))

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

(defn with-recorded-deps [expr expected]
  (let [errors (volatile! nil)
        types (atom {})
        vars (atom {})
        interop (atom {})
        type-syms (atom {})
        ->serialize (fn [t]
                      (binding [uvs/*verbose-types* true]
                        (let [t (force-type t)]
                          (cond-> t (r/Type? t) prs/unparse-type))))
        instrumented-checker (when-some [^clojure.lang.IAtom2 checker env/*checker*]
                               (reify
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
                                 (deref [_] (monitored-checker-map @checker #(swap! types update-in %1 (fn [prev] (or prev (->serialize %2) {}))) []))))]
    (binding [env/*checker* instrumented-checker
              ana2/resolve-sym (let [resolve-sym ana2/resolve-sym]
                                 (fn [sym env]
                                   (let [r (resolve-sym sym env)]
                                     (when r
                                       (swap! vars assoc sym (if (ana2/var? r)
                                                               (ana2/var->sym r)
                                                               (if (class? r)
                                                                 (coerce/Class->symbol r)
                                                                 r))))
                                     r)))
              check/check-expr (let [check-expr check/check-expr]
                                 (fn ce
                                   ([expr] (ce expr nil))
                                   ([expr expected] (let [{:keys [op] :as cexpr} (check-expr expr expected)]
                                                      (when (impl/checking-clojure?)
                                                        ;;TODO resolve actual method signatures
                                                        (case op
                                                          (:instance-call :static-call) (swap! interop update op (fnil conj #{}) (cu/MethodExpr->qualsym cexpr))
                                                          (:instance-field :static-field) (swap! interop update op (fnil conj #{}) (cu/FieldExpr->qualsym cexpr))
                                                          :new (swap! interop update op (fnil conj #{}) (cu/NewExpr->qualsym cexpr))
                                                          nil))
                                                      cexpr))))
              ;; preserve the namespace resolutions that occur while parsing during type checking, like t/ann-form
              prs/resolve-type-clj->sym (let [resolve-type-clj->sym prs/resolve-type-clj->sym]
                                          (fn [sym]
                                            (let [res (resolve-type-clj->sym sym)]
                                              (swap! type-syms assoc sym res)
                                              res)))
              prs/resolve-type-clj (let [resolve-type-clj prs/resolve-type-clj]
                                     (fn [sym]
                                       (let [res (resolve-type-clj sym)]
                                         (when res (swap! type-syms assoc sym res))
                                         res)))
              prs/parse-type-symbol-default (let [parse-type-symbol-default prs/parse-type-symbol-default]
                                              (fn [sym]
                                                (let [res (parse-type-symbol-default sym)]
                                                  (swap! type-syms assoc sym (->serialize res))
                                                  res)))]
      (let [result (check/check-expr expr expected)]
        (assoc result ::cache-info {::types (dissoc @types :clojure.core.typed.current-impl/current-nocheck-var?)
                                    ::vars @vars ::errors (pos? (count @uvs/*delayed-errors*)) ::interop @interop
                                    ::type-syms @type-syms})))))

(defn retrieve-cache-info [{:keys [env] :as expr}
                           expected
                           {:keys [top-level-form-string ns-form-string] :as opts}]
  (get-in (env/deref-checker) [::check-cache (ana2/current-ns-name env) ns-form-string top-level-form-string]))

(defn need-to-check-top-level-expr? [expr expected {:keys [top-level-form-string ns-form-string] :as opts}]
  (or (-> *ns* meta :typed.clojure :experimental :cache not)
      (some? expected)
      (not ns-form-string)
      (not top-level-form-string)
      (if-some [cache-info (retrieve-cache-info expr expected opts)]
        (do
          (println "need-to-check-top-level-expr?: found cache info")
          ;;TODO compare cache-info to current environment
          true)
        (do
          (println "need-to-check-top-level-expr?: did not find cache info")
          true))))

(defn- record-cache! [{::keys [cache-info env] :as cexpr}
                      {:keys [form env] :as original}
                      {:keys [top-level-form-string ns-form-string] :as opts}]
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
  (env/swap-checker! assoc-in [::check-cache (ana2/current-ns-name env) ns-form-string top-level-form-string]
                     cache-info))

(defn remove-stale-cache-entries [nsym ns-form-str sforms]
  {:pre [(simple-symbol? nsym)]}
  (when ns-form-str
    (env/swap-checker! update-in [::check-cache nsym]
                       (fn [m]
                         (some-> m
                                 (select-keys [ns-form-str])
                                 not-empty
                                 (update ns-form-str select-keys sforms))))))

(defn check-top-level-expr [expr expected opts]
  (if (need-to-check-top-level-expr? expr expected opts)
    (let [cexpr (with-recorded-deps expr expected)]
      (when (not expected) (record-cache! cexpr expr opts))
      cexpr)
    expr))

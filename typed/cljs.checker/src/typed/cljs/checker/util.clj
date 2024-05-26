;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljs.checker.util
  (:refer-clojure :exclude [requiring-resolve])
  (:require [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.emit-form-cljs :as emit-form]
            [clojure.tools.reader :as reader]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(defonce default-env (env/default-compiler-env))

(comment
  (-> @default-env keys)
  (-> @default-env ::ana/namespaces
      (get 'cljs.core.typed.test.ann))
  )

(defmacro with-cljs-typed-env [& body]
  `(env/with-compiler-env (or env/*compiler* default-env)
     (do ~@body)))

(defn with-cljs-typed-env* [f]
  (with-cljs-typed-env (f)))

(defn with-core-cljs* []
  (comp/with-core-cljs))

(defn var-exists? [env prefix suffix]
  (let [compiler env/*compiler*
        _ (assert compiler)]
    (contains? (get-in @compiler [::ana/namespaces prefix :defs])
               suffix)))

(defn resolve-var [nsym sym]
  {:post [((some-fn symbol? nil?) %)]}
  (with-cljs-typed-env
    (comp/with-core-cljs)
    (binding [ana/*cljs-ns* nsym
              ana/*private-var-access-nowarn* true]
      (let [unresolved? (atom false)
            r (binding [ana/*cljs-warning-handlers* []]
                (ana/resolve-var (ana/empty-env) sym
                                 (fn [env ns sym]
                                   (when-not (var-exists? env ns sym)
                                     (reset! unresolved? true)))))
            sym* (or (when-not @unresolved?
                       (:name r)))
            _ (when sym*
                (assert (symbol? sym*) sym*)
                (assert (namespace sym*) sym*))]
      ;(prn sym sym*)
      sym*))))

(comment
  (resolve-var 'cljs.core '+)
  (resolve-var 'cljs.core.typed.test.ann 't/Num)
  (resolve-var 'cljs.core.typed.test.ann 'foo)
  (with-cljs-typed-env
    (binding [ana/*cljs-ns* 'cljs.core.typed.test.ann]
      (cljs.core.typed/check-ns* 'cljs.core.typed.test.ann)
      (ana/empty-env)))
  )

(defn resolve-ns-alias [env alias-sym]
  {:pre [(simple-symbol? alias-sym)]
   :post [(simple-symbol? %)]}
  (or (ana/resolve-macro-ns-alias env alias-sym nil)
      (ana/resolve-ns-alias env alias-sym nil)
      alias-sym))

(defn cljs-ns []
  ana/*cljs-ns*)

(defn emit-form [ast]
  (emit-form/emit-form ast))

(defmacro with-core-cljs-typed [& body]
  `(comp/with-core-cljs
     nil
     #(do (when-not (get-in @env/*compiler* [::ana/namespaces 'cljs.core.typed :defs])
            (ana/analyze-file "cljs/core/typed.cljs"))
          ~@body)))

(defn get-aliases
  ([] (get-aliases ana/*cljs-ns*))
  ([cljs-nsym]
   (with-cljs-typed-env
     (comp/with-core-cljs)
     (if-some [get-aliases (requiring-resolve 'cljs.analyzer/get-aliases)]
       (get-aliases cljs-nsym)
       (apply merge
              ((juxt :requires :require-macros :as-aliases)
               (ana/get-namespace cljs-nsym)))))))

(defn with-analyzer-bindings*
  ([f] (with-analyzer-bindings* "NO_FILE" f))
  ([file f] (with-analyzer-bindings* 'cljs.user file f))
  ([nsym file f]
   (binding [ana/*file-defs*        (atom #{})
             ana/*unchecked-if*     false
             ana/*unchecked-arrays* false
             ana/*cljs-warnings*    ana/*cljs-warnings*
             ana/*cljs-ns* nsym
             ana/*cljs-file* file
             reader/*alias-map* (or reader/*alias-map* {})]
     (f))))

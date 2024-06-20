(ns typed.cljs.checker.test-utils
  (:require [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.core.typed :as cljs-t]
            [cljs.core.typed :as t]
            [typed.cljs.runtime.env :as cljs-env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set]
            [clojure.test :as test]
            [cljs.core.server]
            [cljs.repl.node]
            [clojure.core.server :as server]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.test-utils :as common-test]
            [typed.cljs.checker.util :as ucljs])
  (:import [java.net Socket ServerSocket SocketTimeoutException]))

(defmacro cljs [& body]
  `(ucljs/with-cljs-typed-env
     ~@body))

(defmacro is-cljs [& body]
  `(cljs (test/is ~@body)))

(defmacro is-cf [& body]
  `(is-cljs (t/cf ~@body) true))

(defn check-opt [opt]
  #_(assert (empty? (set/difference (set (keys opt))
                                  #{:expected :ret}))))

(defn tc-common* [frm {{:keys [syn provided?]} :expected-syntax :keys [expected-ret] :as opt}]
  (let [nsym (gensym 'clojure.core.typed.test.temp)]
    (check-opt opt)
    `(ucljs/with-analyzer-bindings*
       (fn []
         (cljs-t/load-if-needed)
         (ucljs/with-cljs-typed-env
           (let [expected-ret# (cljs ~expected-ret)
                 ; first element of this list must be the symbol ns
                 ns-form# '(~'ns ~nsym
                             ~'(:refer-clojure :exclude [fn])
                             ~'(:require-macros [cljs.core.typed :as t :refer [ann-form fn]]
                                                [cljs.core :as cc]))
                 ;; use cljs.analyzer just for side effects
                 _# (ana-api/analyze (ana-api/empty-env) ns-form#)
                 res# (t/check-form-info 
                        '~frm
                        :expected-ret expected-ret#
                        :expected '~syn
                        :type-provided? ~provided?
                        :skip-cljs-analyzer-bindings true)]
             (ana-api/remove-ns '~nsym)
             res#))))))

(defmacro tc-e 
  "Type check an an expression in namespace that :refer's
  all of clojure.core.typed (aliased to t) and aliases clojure.core
  to core.

  Takes one form and then options, and returns true if the form checks
  with the expected input/output types according to the provided options.
  
  The first form in the options can be a static type syntax scoped
  in the new namespace. This is disambiguated with a call to keyword?
  (literal keywords aren't valid type syntax).
  
  eg. (tc-e (+ 1 1) Num)
      ;=> Num

  Keyword Options:

    :expected-ret An expected ret, evaluated in the current namespace (not the new
                  one that refers c.c.t). Cannot be provided in combination with the implicit
                  first option as a type, as above.
    :ret          Check the return TCResult of this expression against this ret. Evaluated
                  in the current namespace."
  [frm & opts]
  (apply common-test/tc-e tc-common* frm opts))

(defmacro is-tc-e [& body]
  `(test/is (do (tc-e ~@body)
                true)))

(defmacro is-tc-err [& body]
  `(test/is (tc-err ~@body)))

(defmacro tc-err [frm & opts]
  (apply common-test/tc-err tc-common* frm opts))

(defmacro subtype? [s t]
  `(sub/subtype? ~s ~t cljs-opts))

(defmacro sub? [s t]
  `(subtype? (prs/parse-cljs '~s)
             (prs/parse-cljs '~t)))

(defn cljs-eval
  ([exprs] (cljs-eval 5 5000 exprs))
  ([fuel timeout-ms exprs]
   (let [^ServerSocket server (server/start-server {:accept 'cljs.core.server/io-prepl
                                                    :address "127.0.0.1"
                                                    :port 0
                                                    :name (str `cljs-prepl-server)
                                                    :args [:repl-env (cljs.repl.node/repl-env)]})
         port (.getLocalPort server)]
     (with-open [socket (doto (Socket. "127.0.0.1" port)
                          (.setSoTimeout timeout-ms))
                 reader (io/reader socket)
                 writer (io/writer socket)]
       (into []
             (mapcat (fn [expr]
                       (let [stdout *out*
                             dbg #(binding [*out* stdout]
                                    (apply prn %&))]
                         (binding [*out* writer
                                   *in* reader]
                           (println (pr-str expr))
                           (loop [acc []
                                  fuel fuel]
                             ;(dbg "fuel" fuel)
                             (if-not (pos? fuel)
                               (conj acc {:tag ::fuel-depleted
                                          :form expr})
                               (if-some [r (try (edn/read-string (read-line))
                                                (catch SocketTimeoutException _))]
                                 (cond-> (conj acc r)
                                   (and (not= :ret (:tag r))
                                        (not (:exception r))) (recur (dec fuel)))
                                 (conj acc {:tag ::timeout
                                            :form expr}))))))))
             exprs)))))

(def cljs-opts (cljs-env/cljs-opts))

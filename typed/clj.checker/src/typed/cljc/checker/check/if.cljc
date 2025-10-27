;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.check.if
  (:require [clojure.string :as str]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.proposition-rep :as fl]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.update :as update]
            [typed.cljc.checker.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.var-env :as var-env]))

(defn update-lex+reachable [lex-env fs opts]
  {:pre [(lex/PropEnv? lex-env)]}
  (let [reachable (volatile! true)
        env (update/env+ lex-env [fs] reachable opts)]
    [env @reachable]))

(defn combine-rets [{fs+ :then fs- :else :as tst-ret}
                    {ts :t fs2 :fl os2 :o :as then-ret}
                    env-thn
                    {us :t fs3 :fl os3 :o :as else-ret}
                    env-els
                    opts]
  (let [then-reachable? (not (r/Bottom? ts))
        else-reachable? (not (r/Bottom? us))
        type (c/Un [ts us] opts)
        filter (let [{f2+ :then f2- :else} fs2
                     {f3+ :then f3- :else} fs3
                     new-thn-props (:props env-thn)
                     new-els-props (:props env-els)
                     ; +ve test, +ve then
                     +t+t (if then-reachable?
                            (fo/-and (list* fs+ f2+ new-thn-props) opts)
                            fl/-bot)
                     ; +ve test, -ve then
                     +t-t (if then-reachable?
                            (fo/-and (list* fs+ f2- new-thn-props) opts)
                            fl/-bot)
                     ; -ve test, +ve else
                     -t+e (if else-reachable?
                            (fo/-and (list* fs- f3+ new-els-props) opts)
                            fl/-bot)
                     ; -ve test, -ve else
                     -t-e (if else-reachable?
                            (fo/-and (list* fs- f3- new-els-props) opts)
                            fl/-bot)

                     final-thn-prop (fo/-or [+t+t -t+e] opts)
                     final-els-prop (fo/-or [+t-t -t-e] opts)
                     fs (fo/-FS final-thn-prop final-els-prop)]
                 fs)
        object (cond
                 (not then-reachable?) os3
                 (not else-reachable?) os2
                 :else (if (= os2 os3) os2 obj/-empty))]
    (r/ret type filter object)))

(defn unreachable-ret []
  (r/ret r/-nothing
         (fo/-unreachable-proposition)
         obj/-empty))

(defn check-if-reachable [expr lex-env reachable? expected {::check/keys [check-expr] :as opts}]
  {:pre [(lex/PropEnv? lex-env)
         (boolean? reachable?)]}
  (if (not reachable?)
    (assoc expr 
           u/expr-type (unreachable-ret))
    (check-expr expr expected
                (-> opts
                    (assoc ::vs/current-expr expr)
                    (var-env/with-lexical-env lex-env)))))

(defn check-if [{:keys [test then else] :as expr} expected
                {::vs/keys [^java.util.concurrent.ExecutorService check-threadpool]
                 ::check/keys [check-expr] :as opts}]
  {:pre [((some-fn r/TCResult? nil?) expected)]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [ctest (check-expr test nil opts)
        tst (u/expr-type ctest)
        {fs+ :then fs- :else :as tst-f} (r/ret-f tst)
        lex-env (lex/lexical-env opts)
        chk-thn #(let [[env-thn reachable+] (update-lex+reachable lex-env fs+ opts)]
                   {:env-thn env-thn
                    :cthen (check-if-reachable then env-thn reachable+ expected opts)})
        chk-els #(let [[env-els reachable-] (update-lex+reachable lex-env fs- opts)]
                   {:env-els env-els
                    :celse (check-if-reachable else env-els reachable- expected opts)})
        [{:keys [cthen env-thn]} {:keys [celse env-els]}] (if check-threadpool
                                                            (let [^java.util.concurrent.Callable f (bound-fn []
                                                                                                     (let [res (volatile! nil)
                                                                                                           ex (volatile! nil)
                                                                                                           out (with-out-str
                                                                                                                 (try (vreset! res (chk-els))
                                                                                                                      (catch Throwable e (vreset! ex e))))]
                                                                                                       {:out out
                                                                                                        :ex @ex
                                                                                                        :els @res}))
                                                                  fut (.submit check-threadpool f)
                                                                  thn (chk-thn)
                                                                  {:keys [ex out els]} (try (.get fut)
                                                                                            (catch java.util.concurrent.ExecutionException e
                                                                                              (throw (or (.getCause e) e))))]
                                                              (some-> out str/trim not-empty println)
                                                              (some-> ex throw)
                                                              [thn els])
                                                            [(chk-thn) (chk-els)])
        then-ret (u/expr-type cthen)
        else-ret (u/expr-type celse)]
    (let [if-ret (combine-rets tst-f then-ret env-thn else-ret env-els opts)]
      (assoc expr
             :test ctest
             :then cthen
             :else celse
             ;; already called `check-below` down each branch
             u/expr-type if-ret))))

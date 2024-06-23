;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.base-env-helper-cljs
  (:refer-clojure :exclude [type])
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljs.runtime.env :refer [cljs-opts]]
            [typed.cljc.checker.declared-kind-env :as decl-env]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljs.checker.jsnominal-env :as jsnom]
            [typed.cljc.checker.datatype-env :as dtenv]
            ;[typed.cljs.checker.util :as ucljs]
            ;[cljs.analyzer :as ana]
            ;[cljs.compiler :as comp]
            [clojure.pprint :as pprint]))

;;TODO use delay-type idiom + when-bindable-defining-ns

(defmacro var-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (doall
             (for [[s# t#] ts#]
               (do
                 (assert (and (symbol? s#)
                              (namespace s#))
                         "Need fully qualified symbol")
                 [s# (prs/parse-type t# (cljs-opts))]))))))

(defmacro js-var-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (doall
             (for [[s# t#] ts#]
               [s# (prs/parse-type t# (cljs-opts))])))))

(defn declared-kind-for-protocol [binder opts]
  (let [fs (map first binder)
        _ (assert (every? symbol? fs) fs)
        vs (map (fn [[v & {:keys [variance]}]] variance) binder)]
    (c/TypeFn* fs vs (repeat (count vs) r/no-bounds) r/-any opts)))

(defmacro protocol-mappings [& args]
  `(let [checker# (impl/cljs-checker)
         ts# (partition 2 '~args)
         opts# (cljs-opts)]
     (into {}
           (doall
             (for [[n# [fields# & {:as popts#}]] ts#]
               (let [vs# (seq
                           (for [[_# & {variance# :variance}] fields#]
                             variance#))
                     decl-kind# (declared-kind-for-protocol fields# opts#)
                     ;FIXME this is harder than it has to be
                     ; add a Name so the methods can be parsed
                     _# (nme-env/declare-protocol* checker# n#)
                     _# (when (r/TypeFn? decl-kind#)
                          (decl-env/add-declared-kind checker# n# decl-kind#))
                     names# (when (seq fields#)
                              (map first fields#))
                     ; FIXME specify bounds
                     bnds# (when (seq fields#)
                             (repeat (count fields#) r/no-bounds))
                     frees# (map r/make-F names#)
                     methods# (let [opts# (free-ops/with-bounded-frees opts# (zipmap frees# bnds#))]
                                (into {}
                                      (for [[mname# mtype#] (:methods popts#)]
                                        [mname# (prs/parse-type mtype# opts#)])))
                     the-var# n#
                     on-class# (c/Protocol-var->on-class the-var#)]
                 (decl-env/remove-declared-kind checker n#)
                 [n# (c/Protocol* names# vs# frees# the-var#
                                  on-class# methods# bnds# opts#)]))))))

(defn jsnominal-entry [[n [binder & {:as jopts}]] opts]
  (let [names (when (seq binder)
                    (map first binder))
        {vs :variances
         names :names
         bnds :bnds} 
        (when (seq binder)
          ; don't bound frees because mutually dependent bounds are problematic
          ; FIXME ... Or is this just laziness? 
          (let [opts (-> opts (free-ops/with-free-symbols names))
                b (mapv #(prs/parse-tfn-binder % opts) binder)]
            {:variances (map :variance b)
             :names (map :nme b)
             :bnds (map :bound b)}))
        frees (map r/make-F names)
        methods (let [opts (free-ops/with-bounded-frees opts (zipmap frees bnds))]
                   (into {}
                         (for [[mname mtype] (:methods jopts)]
                           [mname (c/abstract-many names (prs/parse-type mtype opts) opts)])))
        fields (let [opts (free-ops/with-bounded-frees opts (zipmap frees bnds))]
                  (into {}
                        (for [[mname mtype] (:fields jopts)]
                          [mname (c/abstract-many names (prs/parse-type mtype opts) opts)])))
        ctor (when-let [ctor (:ctor jopts)]
                (let [opts (free-ops/with-bounded-frees opts (zipmap frees bnds))]
                  (c/abstract-many names (prs/parse-type ctor opts) opts)))
        ancestors (let [opts (free-ops/with-bounded-frees opts (zipmap frees bnds))]
                    (into #{}
                          (for [mtype (:ancestors jopts)]
                            (c/abstract-many names (prs/parse-type mtype opts) opts))))]
    (decl-env/remove-declared-kind (impl/cljs-checker) n)
    [n {:jsnominal (c/JSNominal* names vs frees n bnds opts)
         :fields fields
         :methods methods
         :ctor ctor
         :ancestors ancestors}]))


(defmacro jsnominal-mappings [& args]
  `(let [ts# (partition 2 '~args)
         opts# (cljs-opts)]
     (into {}
           (doall
              (for [t# ts#]
                (jsnominal-entry t# opts#))))))

(defmacro datatype-mappings [& args]
  `(let [ts# (partition 2 '~args)
         opts# (cljs-opts)]
     (into {}
           (doall
             (for [[n# [binder# & {record?# :record? :as dopts#}]] ts#]
               (let [names# (when (seq binder#)
                              (map first binder#))
                     {vs# :variances
                      names# :names
                      bnds# :bnds} 
                     (when (seq binder#)
                       ; don't bound frees because mutually dependent bounds are problematic
                       ; FIXME ... Or is this just laziness? 
                       (let [opts# (-> opts# (free-ops/with-free-symbols names#))
                             b# (mapv #(prs/parse-tfn-binder % opts#) binder#)]
                         {:variances (seq (map :variance b#))
                          :names (seq (map :nme b#))
                          :bnds (seq (map :bound b#))}))
                     frees# (seq (map r/make-F names#))
                     fields# (let [opts# (free-ops/with-bounded-frees opts# (zipmap frees# bnds#))]
                               (into {}
                                     (for [[mname# mtype#] (:fields dopts#)]
                                       [mname# (prs/parse-type mtype# opts#)])))]
                 (decl-env/remove-declared-kind (impl/cljs-checker) n#)
                 [n# (c/DataType* names# vs# frees# n# bnds# fields# (boolean record?#) opts#)]))))))

(defmacro jsenv-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (doall
             (for [[s# t#] ts#]
               [s# (prs/parse-type t# (cljs-opts))])))))

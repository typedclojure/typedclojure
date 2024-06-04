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
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# (prs/parse-type t#)])))))))

(defmacro js-var-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 [s# (prs/parse-type t#)]))))))

(defn declared-kind-for-protocol [binder]
  (let [fs (map first binder)
        _ (assert (every? symbol? fs) fs)
        vs (map (fn [[v & {:keys [variance]}]] variance) binder)]
    (c/TypeFn* fs vs (repeat (count vs) r/no-bounds) r/-any)))

(defmacro protocol-mappings [& args]
  `(impl/with-cljs-impl
     (let [checker# (impl/cljs-checker)
           ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[n# [fields# & {:as opts#}]] ts#]
                 (let [vs# (seq
                             (for [[_# & {variance# :variance}] fields#]
                               variance#))
                       decl-kind# (declared-kind-for-protocol fields#)
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
                       methods# (free-ops/with-bounded-frees (zipmap frees# bnds#)
                                  (into {}
                                        (for [[mname# mtype#] (:methods opts#)]
                                          [mname# (prs/parse-type mtype#)])))
                       the-var# n#
                       on-class# (c/Protocol-var->on-class the-var#)]
                   (decl-env/remove-declared-kind checker n#)
                   [n# (c/Protocol* names# vs# frees# the-var#
                                    on-class# methods# bnds#)])))))))

(defn jsnominal-entry [[n [binder & {:as opts}]]]
  (let [names (when (seq binder)
                    (map first binder))
        {vs :variances
         names :names
         bnds :bnds} 
        (when (seq binder)
          ; don't bound frees because mutually dependent bounds are problematic
          ; FIXME ... Or is this just laziness? 
          (let [b (free-ops/with-free-symbols names
                     (mapv prs/parse-tfn-binder binder))]
            {:variances (map :variance b)
             :names (map :nme b)
             :bnds (map :bound b)}))
        frees (map r/make-F names)
        methods (free-ops/with-bounded-frees (zipmap frees bnds)
                   (into {}
                         (for [[mname mtype] (:methods opts)]
                           [mname (c/abstract-many names (prs/parse-type mtype))])))
        fields (free-ops/with-bounded-frees (zipmap frees bnds)
                  (into {}
                        (for [[mname mtype] (:fields opts)]
                          [mname (c/abstract-many names (prs/parse-type mtype))])))
        ctor (when-let [ctor (:ctor opts)]
                (free-ops/with-bounded-frees (zipmap frees bnds)
                  (c/abstract-many names (prs/parse-type ctor))))
        ancestors (free-ops/with-bounded-frees (zipmap frees bnds)
                     (into #{}
                           (for [mtype (:ancestors opts)]
                             (c/abstract-many names (prs/parse-type mtype)))))]
    (decl-env/remove-declared-kind (impl/cljs-checker) n)
    [n {:jsnominal (c/JSNominal* names vs frees n bnds)
         :fields fields
         :methods methods
         :ctor ctor
         :ancestors ancestors}]))


(defmacro jsnominal-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
                (for [t# ts#]
                 (jsnominal-entry t#)))))))

(defmacro datatype-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[n# [binder# & {record?# :record? :as opts#}]] ts#]
                 (let [names# (when (seq binder#)
                                (map first binder#))
                       {vs# :variances
                        names# :names
                        bnds# :bnds} 
                       (when (seq binder#)
                         ; don't bound frees because mutually dependent bounds are problematic
                         ; FIXME ... Or is this just laziness? 
                         (let [b# (free-ops/with-free-symbols names#
                                    (mapv prs/parse-tfn-binder binder#))]
                           {:variances (seq (map :variance b#))
                            :names (seq (map :nme b#))
                            :bnds (seq (map :bound b#))}))
                       frees# (seq (map r/make-F names#))
                       fields# (free-ops/with-bounded-frees (zipmap frees# bnds#)
                                 (into {}
                                       (for [[mname# mtype#] (:fields opts#)]
                                         [mname# (prs/parse-type mtype#)])))]
                   (decl-env/remove-declared-kind (impl/cljs-checker) n#)
                   [n# (c/DataType* names# vs# frees# n# bnds# fields# (boolean record?#))])))))))

(defmacro jsenv-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 [s# (prs/parse-type t#)]))))))

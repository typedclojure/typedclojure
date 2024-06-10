;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check-impl
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.core.typed.current-impl :as impl]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.binding :as binding]
            [typed.cljc.checker.check.catch :as catch]
            [typed.cljc.checker.check.const :as const]
            [typed.cljc.checker.check.do :as do]
            [typed.cljc.checker.check.fn :as fn]
            [typed.cljc.checker.check.if :as if]
            [typed.cljc.checker.check.invoke :as invoke]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check.letfn :as letfn]
            [typed.cljc.checker.check.local :as local]
            [typed.cljc.checker.check.loop :as loop]
            [typed.cljc.checker.check.map :as map]
            [typed.cljc.checker.check.quote :as quote]
            [typed.cljc.checker.check.recur :as recur]
            [typed.cljc.checker.check.set :as set]
            [typed.cljc.checker.check.set-bang :as set!]
            [typed.cljc.checker.check.throw :as throw]
            [typed.cljc.checker.check.try :as try]
            [typed.cljc.checker.check.vector :as vec]
            [typed.cljc.checker.check.with-meta :as with-meta]))

(defmulti -check 
  "Type checks the given expression at an optional expected TCResult.
  Assumes expression has been passed to ana2/run-pre-passes.
  Dispatches on the operator of expr."
  (fn [expr expected opts]
    (::ana2/op expr)))

(defmethod -check ::ana2/binding   [expr expected opts] (binding/check-binding     expr expected opts))
(defmethod -check ::ana2/catch     [expr expected opts] (catch/check-catch         expr expected opts))
(defmethod -check ::ana2/const     [expr expected opts] (const/check-const         expr expected false opts))
(defmethod -check ::ana2/do        [expr expected opts] (do/check-do               expr expected opts))
(defmethod -check ::ana2/fn        [expr expected opts] (fn/check-fn               expr expected opts))
(defmethod -check ::ana2/if        [expr expected opts] (if/check-if               expr expected opts))
(defmethod -check ::ana2/invoke    [expr expected opts] (invoke/check-invoke       expr expected opts))
(defmethod -check ::ana2/let       [expr expected opts] (let/check-let             expr expected {} opts))
(defmethod -check ::ana2/letfn     [expr expected opts] (letfn/check-letfn         expr expected opts))
(defmethod -check ::ana2/local     [expr expected opts] (local/check-local         expr expected opts))
(defmethod -check ::ana2/loop      [expr expected opts] (loop/check-loop           expr expected opts))
(defmethod -check ::ana2/map       [expr expected opts] (map/check-map             expr expected opts))
(defmethod -check ::ana2/quote     [expr expected opts] (quote/check-quote         expr expected opts))
(defmethod -check ::ana2/recur     [expr expected opts] (recur/check-recur         expr expected opts))
(defmethod -check ::ana2/set       [expr expected opts] (set/check-set             expr expected opts))
(defmethod -check ::ana2/set!      [expr expected opts] (set!/check-set!           expr expected opts))
(defmethod -check ::ana2/throw     [expr expected opts] (throw/check-throw         expr expected opts))
(defmethod -check ::ana2/try       [expr expected opts] (try/check-try             expr expected opts))
(defmethod -check ::ana2/vector    [expr expected opts] (vec/check-vector          expr expected opts))
(defmethod -check ::ana2/with-meta [expr expected opts] (with-meta/check-with-meta expr expected opts))

;; TODO

(defmethod -check ::ana2/var
  [expr expected opts]
  ((impl/impl-case opts
    :clojure (requiring-resolve 'typed.clj.checker.check/check-var)
    :cljs (requiring-resolve 'typed.cljs.checker.check/check-var))
   expr expected opts))

(defmethod -check ::ana2/the-var
  [expr expected opts]
  ((impl/impl-case opts
    :clojure (requiring-resolve 'typed.clj.checker.check/check-the-var)
    :cljs (requiring-resolve 'typed.cljs.checker.check/check-the-var))
   expr expected opts))

(defmethod -check ::ana2/def
  [expr expected opts]
  ((impl/impl-case opts
    :clojure (requiring-resolve 'typed.clj.checker.check/check-def)
    :cljs (requiring-resolve 'typed.cljs.checker.check/check-def))
   expr expected opts))

(defmethod -check ::ana2/new
  [expr expected opts]
  ((impl/impl-case opts
    :clojure (requiring-resolve 'typed.clj.checker.check/check-new)
    :cljs (requiring-resolve 'typed.cljs.checker.check/check-new))
   expr expected opts))

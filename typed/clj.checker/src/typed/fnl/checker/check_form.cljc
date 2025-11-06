;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.fnl.checker.check-form
  (:refer-clojure :exclude [#?(:clj requiring-resolve)])
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.current-impl :as impl]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])
            [typed.fnl.checker.env :as fnl-env]
            [typed.fnl.analyzer :as ana-fnl]
            [typed.fnl.checker.check :as chk-fnl]
            [typed.cljc.checker.check-form :as chk-form]
            [typed.cljc.checker.runtime-check :as rt-chk]))

(defn config-map []
  {:impl impl/fnl
   :check-top-level chk-fnl/check-top-level
   :unparse-ns (ns-name *ns*)
   :runtime-check-expr rt-chk/runtime-check-expr
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] ast)) ; No evaluation for Fennel
   :emit-form ast-u/emit-form-fn
   :check-form-info chk-form/check-form-info
   :check-form* chk-form/check-form*})

(defn check-form-info
  "Check a Fennel form and return info map.
  
  The form should be a Clojure data structure as returned by typed.fnl.reader.
  The caller is responsible for parsing Fennel source strings before calling this function."
  [form opt opts]
  (assert (map? opt))
  (let [config (config-map)]
    (chk-form/check-form-info-with-config
      config form opt opts)))

(defn check-form*
  [form expected type-provided? opt opts]
  {:pre [(map? opt)]}
  (let [config (config-map)]
    (chk-form/check-form*-with-config
      config form expected type-provided? opt opts)))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.check-form-clj
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.analyze-clj :as ana-clj]
            [typed.clj.checker.check :as chk-clj]
            [typed.cljc.checker.check-form-common2 :as chk-form2]
            [typed.cljc.checker.runtime-check :as rt-chk]))

(defn config-map2 []
  {:impl impl/clojure
   :check-top-level chk-clj/check-top-level
   :unparse-ns (ns-name *ns*)
   :runtime-check-expr rt-chk/runtime-check-expr
   :runtime-infer-expr (fn [& args]
                         (apply (requiring-resolve 'typed.clj.annotator/runtime-infer-expr) args))
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] (ana-clj/eval-ast ast opts)))
   :custom-expansions? (-> *ns*
                           meta
                           :core.typed
                           :experimental
                           (contains? :custom-expansions))
   :emit-form ast-u/emit-form-fn
   :check-form-info chk-form2/check-form-info
   :check-form* chk-form2/check-form*
   })

(defn check-form-info
  [form & opt]
  (let [config (config-map2)]
    (chk-form2/check-form-info-with-config
      config form opt)))

(defn check-form*
  [form expected type-provided? opt]
  {:pre [(map? opt)]}
  (let [config (config-map2)]
    (chk-form2/check-form*-with-config
      config form expected type-provided? opt)))

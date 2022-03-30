;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.check-form
  (:require [cljs.compiler :as comp]
            [cljs.env :as env]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.check-form :as chk-form2]
            [typed.cljc.checker.runtime-check :as rt-chk] ;;TODO untested
            [typed.cljs.analyzer :as ana]
            [typed.cljs.checker.check :as chk-cljs]
            [typed.cljs.checker.util :as ucljs]))

(defn config-map2 []
  {:impl impl/clojurescript
   :check-top-level chk-cljs/check-top-level
   :unparse-ns (ucljs/cljs-ns)
   ;:runtime-check-expr rt-chk/runtime-check-expr
   ;:runtime-infer-expr (fn [& args]
   ;                      (apply @runtime-infer-expr args))
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] (assert nil "TODO eval cljs") nil #_(ana-clj/eval-ast ast opts)))
   :custom-expansions? true
   :emit-form ast-u/emit-form-fn
   :check-form-info chk-form2/check-form-info
   :check-form* chk-form2/check-form*
   })

(defn maybe-with-analyzer-bindings [{:keys [skip-cljs-analyzer-bindings] :as _opt} f]
  (if skip-cljs-analyzer-bindings
    (f)
    (ucljs/with-analyzer-bindings* (ucljs/cljs-ns) "NO_FILE" f)))

(defn check-form-info
  [form & {:as opt}]
  (with-bindings (ana/default-thread-bindings)
    (maybe-with-analyzer-bindings opt
      (fn []
        (chk-form2/check-form-info-with-config
          (config-map2) form opt)))))

(defn check-form
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the ClojureScript
  REPL see cf."
  [form expected expected-provided? opt]
  (with-bindings (ana/default-thread-bindings)
    (maybe-with-analyzer-bindings opt
      (fn []
        (ucljs/with-cljs-typed-env
          (comp/with-core-cljs
            nil
            #(chk-form2/check-form*-with-config
               (config-map2) form expected expected-provided? opt)))))))

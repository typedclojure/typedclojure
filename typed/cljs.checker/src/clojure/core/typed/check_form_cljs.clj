;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.check-form-cljs
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [typed.cljc.checker.check-form-common2 :as chk-form2]
            [typed.cljc.checker.runtime-check :as rt-chk] ;;TODO untested
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.util-cljs :as ucljs]
            [cljs.env :as env]
            [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]))

(defn config-map2 []
  {:impl impl/clojurescript
   :check-top-level chk-cljs/check-top-level
   :unparse-ns (ucljs/cljs-ns)
   ;:runtime-check-expr rt-chk/runtime-check-expr
   ;:runtime-infer-expr (fn [& args]
   ;                      (apply @runtime-infer-expr args))
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] (prn "TODO eval cljs") nil #_(ana-clj/eval-ast ast opts)))
   :custom-expansions? true
   :emit-form ast-u/emit-form-fn
   :check-form-info chk-form2/check-form-info
   :check-form* chk-form2/check-form*
   })

(defn check-form-info
  [form & opt]
  (chk-form2/check-form-info-with-config
    (config-map2) form opt))

(defn check-form-cljs
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided? opt]
  (ucljs/with-cljs-typed-env
    (comp/with-core-cljs
      nil
      #(chk-form2/check-form*-with-config
         (config-map2) form expected expected-provided? opt))))

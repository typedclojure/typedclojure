;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.check-ns
  (:require [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]
            [cljs.env :as env]
            [typed.cljs.analyzer :as ana]
            [typed.cljs.checker.util :as ucljs]
            [typed.cljc.checker.check-ns-common :as chk-ns]))

(defn check-ns-info
  [ns-or-syms opt]
  (with-bindings (ana/default-thread-bindings)
    (ucljs/with-cljs-typed-env
      (comp/with-core-cljs
        nil
        #(chk-ns/check-ns-info impl/clojurescript ns-or-syms opt)))))

(defn check-ns
  [ns-or-syms opt]
  (with-bindings (ana/default-thread-bindings)
    (ucljs/with-cljs-typed-env
      (comp/with-core-cljs
        nil
        #(chk-ns/check-ns impl/clojurescript ns-or-syms opt)))))

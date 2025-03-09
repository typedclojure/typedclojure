;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljs.checker
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.core.typed.load-if-needed :refer [load-if-needed]]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljs.checker.utils :refer [->opts]]
            [typed.cljs.checker.check-form :as check-form-clj]
            [typed.cljs.checker.check-ns :as check-ns-clj]))

(defn default-check-config []
  {:check-ns-dep :never
   :unannotated-def :infer
   :unannotated-var :error
   :unannotated-multi :error
   #_#_:unannotated-arg :any})

(def ^:private cljs-ns #((requiring-resolve 'typed.cljs.checker.util/cljs-ns)))

(defn check-ns-info
  "Check a Clojurescript namespace, or the current namespace (via *ns*).
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see check-ns."
  ([] (check-ns-info (ns-name *ns*)))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   ((requiring-resolve 'typed.cljs.checker.check-ns/check-ns-info) ns-or-syms (update opt :check-config #(into (default-check-config) %))
    (->opts))))

(defn check-ns
  "Check a Clojurescript namespace, or the current namespace (via *ns*).
  Intended to be called from Clojure."
  ([] (check-ns (ns-name *ns*)))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   ((requiring-resolve 'typed.cljs.checker.check-ns/check-ns) ns-or-syms (update opt :check-config #(into (default-check-config) %))
    (->opts))))

(defn check-ns4
  "Check a Clojurescript namespace, or the current namespace (via *ns*).
  Intended to be called from Clojure."
  ([] (check-ns (ns-name *ns*)))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   ((requiring-resolve 'typed.cljs.checker.check-ns/check-ns)
    ns-or-syms (update opt :check-config #(into {:check-ns-dep :never
                                                 :check-form-eval :never
                                                 :check-ns-load :require-before-check}
                                                %))
    (->opts))))

(defn check-form
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided? & {:as opt}]
  (load-if-needed)
  ((requiring-resolve 'typed.cljs.checker.check-form/check-form)
   form expected expected-provided? (update opt :check-config #(into (default-check-config) %))))

(defn check-form-info 
  [form & {:as opt}]
  (load-if-needed)
  (let [opt (update opt :check-config #(into (default-check-config) %))]
    (apply (requiring-resolve 'typed.cljs.checker.check-form/check-form-info) form (apply concat opt))))

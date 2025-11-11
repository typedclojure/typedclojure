;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.fnl.checker
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.core.typed.load-if-needed :refer [load-if-needed]]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.fnl.checker.utils :refer [->opts]]
            [typed.fnl.checker.check-form :as check-form-fnl]
            [typed.fnl.checker.check-ns :as check-ns-fnl]))

(defn default-check-config []
  {:check-ns-dep :never
   :unannotated-def :infer
   :unannotated-var :error
   :unannotated-multi :error
   #_#_:unannotated-arg :any})

(defn check-ns-info
  "Check a Fennel namespace, or the current namespace (via *ns*).
  Intended to be called from Clojure.
  
  Note: Fennel type checking is not yet fully implemented.
  This is a stub for future development."
  ([] (check-ns-info (ns-name *ns*)))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-fnl/check-ns-info ns-or-syms 
                               (update opt :check-config #(into (default-check-config) %))
                               (->opts))))

(defn check-ns
  "Check a Fennel namespace, or the current namespace (via *ns*).
  Intended to be called from Clojure.
  
  Note: Fennel type checking is not yet fully implemented.
  This is a stub for future development."
  ([] (check-ns (ns-name *ns*)))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-fnl/check-ns ns-or-syms 
                          (update opt :check-config #(into (default-check-config) %))
                          (->opts))))

(defn check-form
  "Check a single Fennel form with an optional expected type.
  Intended to be called from Clojure.
  
  Note: Fennel type checking is not yet fully implemented.
  This is a stub for future development."
  [form expected expected-provided? & {:as opt}]
  (load-if-needed)
  (check-form-fnl/check-form* form expected expected-provided? 
                              (update opt :check-config #(into (default-check-config) %))
                              (->opts)))

(defn check-form-info 
  "Check a Fennel form and return detailed information.
  
  Note: Fennel type checking is not yet fully implemented.
  This is a stub for future development."
  [form & {:as opt}]
  (load-if-needed)
  (check-form-fnl/check-form-info form 
                                  (update opt :check-config #(into (default-check-config) %))
                                  (->opts)))

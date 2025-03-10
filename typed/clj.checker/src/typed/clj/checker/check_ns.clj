;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.checker.check-ns
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.check-ns-common :as chk-ns]))

(defn default-check-config []
  {:check-ns-dep :never
   :check-ns-load :require-before-check
   :check-form-eval :never})

(defn check-ns-info
  "Same as check-ns, but returns a map of results from type checking the
  namespace.

  Options
  - :type-provided?  If true, use the expected type to check the form
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str)."
  [ns-or-syms opt opts]
  (chk-ns/check-ns-info impl/clojure ns-or-syms
                        (update opt :check-config #(into (default-check-config) %))
                        opts))

(defn check-ns
  [ns-or-syms opt opts]
  (chk-ns/check-ns impl/clojure ns-or-syms
                   (update opt :check-config #(into (default-check-config) %))
                   opts))

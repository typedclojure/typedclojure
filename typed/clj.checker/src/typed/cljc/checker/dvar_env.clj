;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.dvar-env
  (:require [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.contract-utils :as con]
            [typed.clojure :as t])
  (:import [clojure.lang Symbol]
           [typed.cljc.checker.type_rep F]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted Variable Environment

;symbol -> F
(t/ann *dotted-scope* (t/Map Symbol F))
(defonce ^:dynamic *dotted-scope* {})
(set-validator! #'*dotted-scope* (con/hash-c? symbol? r/F?))

(t/ann bound-index? [t/Any -> t/Any])
(defn bound-index? [n]
  (contains? *dotted-scope* n))

(defmacro with-dotted [dvars & body]
  `(with-dotted-mappings (into {} (for [v# ~dvars]
                                    [(:name v#) v#]))
     ~@body))

(defmacro with-dotted-mappings [dvar-map & body]
  `(binding [*dotted-scope* (merge *dotted-scope* ~dvar-map)]
     ~@body))

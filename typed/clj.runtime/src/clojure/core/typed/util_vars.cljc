;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.util-vars)

(defonce ^:no-doc ^:dynamic *current-env* nil)
(defonce ^:no-doc ^:dynamic *current-expr* nil)
(defonce ^:no-doc ^:dynamic *in-check-form* nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print fully qualified types in error messages
  and return values. Bind around a type checking form like 
  cf or check-ns.

  Can also be a map. Known options:
  - :unique-tvars if true, print type variables as globally unique
  
  eg. 
  (binding [*verbose-types* true] 
    (cf 1 Number))
  ;=> java.lang.Number"}
  *verbose-types* 
  nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print complete forms in error messages. Bind
  around a type checking form like cf or check-ns.
  
  eg.
  (binding [*verbose-forms* true]
    (cf ['deep ['deep ['deep ['deep]]]] Number))
  ;=> <full form in error>"}
  *verbose-forms* 
  nil)

(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *analyze-ns-cache* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *checked-asts* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *can-rewrite* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *in-typed-load* nil)
;; keep track of state throughout a `load`
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *typed-load-atom* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *prepare-infer-ns* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *instrument-infer-config* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *check-config* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *custom-expansions* nil)
;;TODO replace with pass state
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *beta-count* nil)
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *currently-inferring-TypeFns* #{})
(defonce ^{:doc "Internal use only"} ^:no-doc ^:dynamic *check-threadpool* nil)

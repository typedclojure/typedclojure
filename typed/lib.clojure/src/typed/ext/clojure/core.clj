;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.ext.clojure.core
  "Typing rules for base Clojure distribution."
  (:require [typed.clj.checker.check :refer [defuspecial -unanalyzed-special]]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed :as t]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.check-below :as below]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.analyzer :as ana2]))

;;==================
;; clojure.core/ns


;; # slow version
;; fully expands and traverses using ana2, and then sends to Compiler.java to
;; do it all again.
#_
(defuspecial 'clojure.core/ns
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-nil
                              (fo/-FS fl/-bot fl/-top))
                       expected)))

;; # fast version
;; Delegates (possible) expansion and evaluation to Compiler.java.
;; Always returns nil.

;; TODO could improve error messages. old c.c.t.expand approach used:
;; (check-expected
;;   nil
;;   {:msg-fn (fn [_#]
;;              "This 'ns' expression returns nil, which does not agree with the expected type.")
;;    :blame-form ~&form})
(defmethod -unanalyzed-special 'clojure.core/ns
  [expr expected]
  (-> expr
      ana2/eval-top-level
      (assoc
        u/expr-type (below/maybe-check-below
                      (r/ret r/-nil
                             (fo/-FS fl/-bot fl/-top))
                      expected)
        :tag nil)))

(comment
        (binding [*ns* *ns*]
          (t/cf (ns foo)))
        (binding [*ns* *ns*]
          (t/check-form-info '(ns foo)
                             :expected 't/Str
                             :type-provided? true))
        )

;;==================
;; clojure.core/defmacro

(defuspecial 'clojure.core/defmacro
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-any)
                       expected)))

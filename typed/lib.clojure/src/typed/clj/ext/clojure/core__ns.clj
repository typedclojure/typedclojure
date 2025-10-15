;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.clj.ext.clojure.core__ns
  "Typing rules clojure.core/ns"
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.proposition-rep :as fl]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

;;==================
;; clojure.core/ns

;; # slow version
;; fully expands and traverses using ana2, and then sends to Compiler.java to
;; do it all again.
#_
(defuspecial 'clojure.core/ns
  [expr expected opts]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-nil
                              (fo/-FS fl/-bot fl/-top))
                       expected
                       opts)))

;; # fast version
;; Delegates (possible) expansion and evaluation to Compiler.java.
;; Always returns nil.

;; TODO could improve error messages. old c.c.t.expand approach used:
;; (check-expected
;;   nil
;;   {:msg-fn (fn [_#]
;;              "This 'ns' expression returns nil, which does not agree with the expected type.")
;;    :blame-form ~&form})
(defn -unanalyzed-special__ns
  "-unanalyzed-special implementation for clojure.core/ns"
  [expr expected opts]
  (-> expr
      (ana2/eval-top-level opts)
      (assoc
        u/expr-type (below/maybe-check-below
                      (r/ret r/-nil
                             (fo/-FS fl/-bot fl/-top))
                      expected
                      opts)
        :tag nil)))

(comment
  (require [clojure.core.typed :as t])
  (binding [*ns* *ns*]
    (t/cf (ns foo)))
  (binding [*ns* *ns*]
    (t/check-form-info '(ns foo)
                       :expected 't/Str
                       :type-provided? true))
  )

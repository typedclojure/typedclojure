;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.runtime-check
  "Adds runtime checks where annotations are instead of type checking"
  (:require [typed.cljc.checker.check.do :as do]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.def :as def]
            [typed.cljc.checker.check :as check]
            [clojure.core.typed.ast-utils :as ast]))

(defn check-expr
  "Add runtime checks to the output AST.

  Static checking is disabled, outside ill-formed types.
  
  Unsafe contracts can be generated, and contract generation cannot fail.
  
  Assumes collect-expr is already called on this AST."
  ([expr expected opts]
   (let [opts (assoc opts ::check-expr check-expr)
         check-expr (check/->check-expr check-expr opts)]
     (case (:op expr)
       (:def) (if (def/defmacro-or-declare? expr)
                ;; ignore defmacro and declare
                expr
                (def/add-checks-normal-def expr expected opts))
       (:do) (letfn [(default-do [expr expected]
                       (assoc expr
                              :statements (mapv check-expr (:statements expr))
                              :ret (check-expr (:ret expr) expected)))]

               (if (do/internal-form? expr)
                 (case (u/internal-dispatch-val expr)
                   ;; could be an error or another special form, 
                   ;; but we'll let it slide in runtime checking mode.
                   expr)
                 (default-do expr expected)))
       (ast/walk-children check-expr expr)))))

(def runtime-check-expr check-expr)

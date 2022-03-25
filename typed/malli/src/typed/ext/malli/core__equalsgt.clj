;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.ext.malli.core__equalsgt
  "Type rules for malli.core/=>"
  (:require [typed.clojure :as-alias t]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

;;======================
;; malli.core/=>

;; TODO type check schema arg
(defuspecial defuspecial__=>
  "defuspecial implementation for malli.core/=>"
  [expr expected]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (prs/parse-type `t/Sym))
                       expected)))

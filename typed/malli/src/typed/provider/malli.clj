;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.provider.malli
  "Automatically convert malli annotations into types.")

;; (ann-form 1 (typed.provider.malli/Malli number?))

(defn malli->Type [m]
  ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-type)
   ((requiring-resolve 'typed.malli.parse-type/malli-syntax->validator-type)
    m)))

(defn Malli [[_Malli m :as args]]
  (assert (= 2 (count args)) (pr-str args))
  (malli->Type m))

(defn var-type [var-qsym]
  (some-> (get-in ((requiring-resolve 'malli.core/function-schemas))
                  [(symbol (namespace var-qsym))
                   (symbol (name var-qsym))
                   :schema])
          ((requiring-resolve 'malli.core/form))
          malli->Type))

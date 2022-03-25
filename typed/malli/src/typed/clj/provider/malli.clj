;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.provider.malli
  "Automatically convert malli annotations into types."
  (:require [typed.malli.schema-to-type :as s->t]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [malli.core :as m]))

(defonce register!
  (delay
    (configs/register-clj-malli-extensions)))

(defn malli->Type [m opts]
  @register!
  ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-type)
   (s->t/malli->type m opts)))

(defn var-type [var-qsym]
  (some-> (get-in (m/function-schemas)
                  [(symbol (namespace var-qsym))
                   (symbol (name var-qsym))
                   :schema])
          (malli->Type {::s->t/mode :validator-type
                        ::s->t/source var-qsym})))

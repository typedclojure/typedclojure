;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.provider.malli-cljs
  "Automatically convert malli annotations into types."
  (:require [typed.malli.schema-to-type :as s->t]
            [malli.core :as m]))

(m/=> var-type-syntax [:=> [:cat :symbol] :any])
(defn var-type-syntax [var-qsym]
  ;(prn (m/function-schemas :cljs))
  (some-> (some #(get-in % [var-qsym :schema])
                (vals (m/function-schemas :cljs)))
          (s->t/malli->type {::s->t/mode :validator-type
                             ::s->t/source var-qsym})))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.parsed-type-invalidation
  {:clojure.tools.namespace.repl/unload false
   :clojure.tools.namespace.repl/load false})

(defonce ^{:doc "Internal use only"} ^:no-doc parsed-types-invalidation-id (volatile! (str (random-uuid))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.load-if-needed
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  (:require [clojure.core.typed.errors :as err]
            [clojure.java.io :as io]
            [clojure.core.typed.util-vars :as vs]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  ([] (load-if-needed false))
  ([cljs?]
   ((requiring-resolve 'typed.cljc.checker.init/load-impl) cljs?)))

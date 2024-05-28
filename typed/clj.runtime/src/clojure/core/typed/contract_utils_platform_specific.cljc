;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; moved outside clojure.core.typed.contract-utils so we can
;; use that namespace in clj-kondo hooks, which seems to only recognize
;; reader conditions in .clj files, and where PersistentArrayMap doesn't
;; exist.
(ns ^:no-doc clojure.core.typed.contract-utils-platform-specific
  #?(:clj (:require [clojure.core.typed.contract-utils :as con])
     :cljr (:require [clojure.core.typed.contract-utils :as con]))
  #?(:clj (:import (clojure.lang PersistentArrayMap))
     :cljr (:import (clojure.lang PersistentArrayMap))))

#?(:bb nil
   :clj (def namespace? #(instance? clojure.lang.Namespace %))
   :cljr (def namespace? #(instance? clojure.lang.Namespace %)))

#?(:clj (defn array-map-c? [ks-c? vs-c?]
          (every-pred #(instance? PersistentArrayMap %)
                      (con/every-c? (con/hvector-c? ks-c? vs-c?))))
   :cljr (defn array-map-c? [ks-c? vs-c?]
          (every-pred #(instance? PersistentArrayMap %)
                      (con/every-c? (con/hvector-c? ks-c? vs-c?)))))

(defn atom? [v] (instance? #?(:clj clojure.lang.IAtom2 :cljs Atom) v))

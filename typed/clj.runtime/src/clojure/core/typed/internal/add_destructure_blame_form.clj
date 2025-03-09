;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; moved outside clojure.core.typed.internal so we can make another version of this for
;; clj-kondo hooks, which doesn't recognize .cljc files and doesn't have IObj.
(ns ^:typed.clojure ^:no-doc clojure.core.typed.internal.add-destructure-blame-form
  (:require [clojure.walk :as walk])
  (:import [clojure.lang IObj]))

(defn add-destructure-blame-form
  "Recursively annotate destructuring form dform with blame form."
  [dform blame-form]
  (walk/postwalk (fn [dform]
                   (cond-> dform
                     (instance? IObj dform)
                     (vary-meta update :clojure.core.typed.internal/destructure-blame-form
                                #(or % blame-form))))
                 dform))

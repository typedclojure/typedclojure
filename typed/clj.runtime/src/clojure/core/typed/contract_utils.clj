;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.contract-utils
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint shorthands

(defn every-c? [c]
  #(every? c %))

(def nne-seq? (some-fn nil? (every-pred seq? seq)))

(defn =-c? [& as]
  #(apply = (concat as %&)))

(defn hvector-c? [& ps]
  (apply every-pred vector?
         (map (fn [p i] #(p (nth % i false))) ps (range))))

(defn reduced-c? [c]
  (fn [r]
    (and (reduced? r)
         (c @r))))

(defn maybe-reduced-c? [c]
  (fn [r]
    (if (reduced? r)
      (c @r)
      (c r))))

(defrecord OptionalKey [k])

(defn optional [k]
  (->OptionalKey k))

(defn hmap-c? [& key-vals]
  {:pre [(even? (count key-vals))]}
  (every-pred map?
              (fn [m]
                (letfn [(mandatory-check [m k vc]
                          (and (contains? m k)
                               (vc (get m k))))
                        (optional-check [m k vc]
                          (or (not (contains? m k))
                              (mandatory-check m k vc)))]
                  (every? identity 
                    (for [[k vc] (partition 2 key-vals)]
                      (cond
                        (instance? OptionalKey k) (optional-check m (:k k) vc)
                        :else (mandatory-check m k vc))))))))

(defn hash-c? [ks-c? vs-c?]
  (every-pred map?
              (every-c? (hvector-c? ks-c? vs-c?))))

(defn set-c? [c?]
  (every-pred set?
              (every-c? c?)))

(defn vec-c? [c?]
  (every-pred vector?
              (every-c? c?)))

(defn sorted-set-c? [c?]
  (every-pred sorted?
              (set-c? c?)))

(defn sequential-c? [c?]
  (every-pred sequential?
              (every-c? c?)))

(def local-sym? simple-symbol?)

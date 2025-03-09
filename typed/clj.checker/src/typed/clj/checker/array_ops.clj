;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.checker.array-ops
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]))

;Return a Class that generalises what this Clojure type will look like from Java,
;suitable  for use as a Java primitive array member type.
; 
; (Type->array-member-Class (parse-type 'nil)) => Object
; (Type->array-member-Class (parse-type '(U nil Number))) => Number
; (Type->array-member-Class (parse-type '(Array (U nil Number)))) =~> (Array Number)

;(ann Type->array-member-Class (Fn [Type Opts -> (Option Class)]
;                                  [Type t/Any Opts -> (Option Class)]))
(defn Type->array-member-Class
  ([ty opts] (Type->array-member-Class ty false opts))
  ([ty nilok? opts]
   {:pre [(r/Type? ty)]}
   (cond
     (c/requires-resolving? ty opts) (Type->array-member-Class (c/-resolve ty opts) nilok? opts)
     (r/Nil? ty) (if nilok?
                 nil
                 Object)
     (r/Value? ty) (c/Value->Class ty)
     ;; handles most common case of (U nil Type)
     (r/Union? ty) (let [clss (map #(Type->array-member-Class % true opts) (:types ty))
                         prim-and-nil? (and (some nil? clss)
                                            (some #(when % (.isPrimitive ^Class %)) clss))
                         nonil-clss (remove nil? clss)]
                     (if (and (= 1 (count nonil-clss))
                              (not prim-and-nil?))
                       (first nonil-clss)
                       Object))
     (r/Intersection? ty) Object
     (r/RClass? ty) (r/RClass->Class ty)
     (r/PrimitiveArray? ty) (class (make-array (Type->array-member-Class (:jtype ty) false opts) 0))
     :else Object)))

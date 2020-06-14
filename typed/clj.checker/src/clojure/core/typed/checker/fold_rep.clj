;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.fold-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Folding

;1. fold-rhs calls sends
; a. Type to type-rec
; b. Filter to filter-rec
; c. Object to object-rec

;visit a type nested inside ty. Add methods with a mode deriving ::visit-type-default 
(defprotocol IFoldDefault
  (fold-default* [ty options]))

(defmacro def-derived-fold [pname mname]
  `(do (defprotocol ~pname
         (~mname [~'ty ~'options]))
       (extend-protocol ~pname
         Object ;; TODO replace with AnyType
         (~mname [ty# options#]
           ;(prn "default case of in" '~mname (class ty#))
           ;; TODO this zipmap is pretty slow, think of a better solution
           (let [default-options# (zipmap [:type-rec :filter-rec :object-rec :pathelem-rec]
                                          (repeat (fn [ty#]
                                                    (~mname ty# options#))))]
             (fold-default*
               ty# 
               (into default-options# options#)))))))

; fld-fn has type-rec, filter-rec and object-rec in scope
(defmacro add-fold-case [pname mname ty fld-fn]
  `(extend-protocol ~pname
     ~ty
     (~mname [ty# options#]
       ;(prn "in" '~mname '~ty)
       (let [m# ~mname
             ~'[type-rec filter-rec object-rec pathelem-rec]
             (map #(or (% options#) (fn [ty#]
                                      (m# ty# options#)))
                  [:type-rec :filter-rec :object-rec :pathelem-rec])]
         (~fld-fn ty# options#)))))

(defmacro add-default-fold-case [ty fld-fn]
  `(add-fold-case IFoldDefault fold-default* ~ty ~fld-fn))

(defn sub-pe [st f]
  #(f %
      {:type-rec st
       :pathelem-rec (sub-pe st f)}))

(defn sub-f [st f]
  #(f %
      {:type-rec st
       :filter-rec (sub-f st f)
       :pathelem-rec (sub-pe st f)}))

(defn sub-o [st f]
  #(f %
      {:type-rec st
       :object-rec (sub-o st f)
       :pathelem-rec (sub-pe st f)}))

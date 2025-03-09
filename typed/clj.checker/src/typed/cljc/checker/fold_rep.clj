;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.fold-rep
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Folding

;; Here we define generic functions that recursively prewalk types.
;;
;; This is currently implemented via protocols but all of the implementation details are hidden via macros.
;; The main reason is so we can use fixed arguments under the hood instead of allocating map arguments for configuration.
;;
;; Each type implements IFoldDefault whose fold-default* function knows how to prewalk itself.
;; The `type-rec` argument is a function that prewalks the type's children.
;; 
;; However, the fold-default* function is also implementation detail.
;; The entry point here for creating your own type walking algorithms is `def-derived-fold`, which creates
;; _another_ protocol whose default Object case calls `fold-default*` with the correct arguments
;; to ensure `type-rec` continues walking the "derived" algorithm.
;;
;; The "base" algorithm here is IFoldDefault and other folds (walking algorithms) are "derived" from it
;; since if the derived algorithm doesn't define a case for a particular type, it uses IFoldDefault
;; for the "current level" before jumping back to the derived algorithm.
;;
;; Along with `def-derived-fold`, `add-fold-case` extends a walking algorithm with a case for a particular type.
;;
;; See typed-test.cljc.checker.fold-rep for an example.

;1. fold-rhs calls sends
; a. Type to type-rec
; b. Filter to filter-rec
; c. Object to object-rec

(def default-params '[type-rec type-rec+flip type-rec+invariant filter-rec object-rec pathelem-rec])

(defonce all-extra-params (atom {`IFoldDefault []}))

;visit a type nested inside ty
(defmacro ^:private def-fold-default-protocol []
  `(defprotocol IFoldDefault
     (~'fold-default* [~'ty ~'opts ~@default-params])))

(def-fold-default-protocol)

;; (Map QSym (Vec Sym))

(defmacro def-derived-fold [pname mname extra-params]
  {:pre [(simple-symbol? pname)
         (simple-symbol? mname)
         (vector? extra-params)
         (apply distinct? (concat default-params extra-params))]}
  (let [default-gs (map gensym default-params)
        extra-gs (map gensym extra-params)
        default-f (gensym 'default-f)
        qpname (symbol (-> *ns* ns-name name) (name pname))
        qmname (symbol (-> *ns* ns-name name) (name mname))
        gopts (gensym 'opts)
        call-mname-defmacro 
        (let [m (gensym 'm)]
          `(defmacro ~(symbol (str "call-" mname)) [t# opts# ~m]
             {:pre [(map? ~m)]}
             (let [extra# (set/difference (set (keys ~m))
                                          '~(into #{} (map keyword) (concat default-params extra-params)))]
               (assert (empty? extra#) (str "Extra keys: " extra#)))
             (list '~qmname t# opts#
                   ; (:type-rec m) (:filter-rec m) ... (:locals m) ...
                   ~@(map #(list (keyword %) m) (concat default-params extra-params)))))]
    (swap! all-extra-params assoc qpname extra-params)
    `(do (defprotocol ~pname
           (~mname [~'ty ~'opts ~@default-params ~@extra-params]))
         (swap! all-extra-params assoc '~qpname '~extra-params)
         ~call-mname-defmacro
         (extend-protocol ~qpname
           Object ;; TODO replace with AnyType
           (~qmname [ty# opts# ~@default-gs ~@extra-gs]
             (let [~default-f (fn rec#
                                ([ty#] (rec# ty# opts#))
                                ([ty# opts#] (~qmname ty# opts# ~@default-gs ~@extra-gs)))
                   ~@(mapcat (fn [g]
                               ;; g is local
                               [g `(if ~g ~g ~default-f)])
                             default-gs)]
               ;; the only place fold-default* is called
               (fold-default* ty# opts# ~@default-gs)))))))

(defmacro add-fold-case [pname mname ty fld-fn]
  (let [qpname (some-> (resolve pname) symbol)
        extra-params (@all-extra-params qpname)
        _ (assert (vector? extra-params) [pname qpname])
        extra-gs (map gensym extra-params)
        qmname (some-> (resolve mname) symbol)
        default-f (gensym 'default-f)
        _ (assert (qualified-symbol? qmname) mname)]
    (assert (not-any? #(= 'opts %) (concat default-params extra-params)))
    `(extend-protocol ~pname ;; can be captured
       ~ty ;; can be captured, including opts
       (~mname [ty# opts# ~@default-params ~@extra-gs]
         ; ~@all-extra-params in scope now
         (let [~default-f (fn rec#
                            ([ty#] (rec# ty# opts#))
                            ([ty# opts#] (~qmname ty# opts# ~@default-params ~@extra-gs)))
               ;; defaults for all-extra-params
               ~@(mapcat (fn [d]
                           ;; d is local
                           [d `(if ~d ~d ~default-f)])
                         default-params)
               ~'opts opts#]
           (~fld-fn ty# ~@extra-gs))))))

(defmacro add-default-fold-case [ty fld-fn]
  `(add-fold-case IFoldDefault fold-default* ~ty ~fld-fn))

(defn ^:private def-sub-function* [sym st f opts]
  (assert (qualified-symbol? f) f)
  (assert (resolve f) f)
  ;; expects a macro call created by def-derived-fold to inline
  ;; map arg into positional
  (assert (.startsWith (name f) "call-") f)
  (let [{:syms [sub-pe sub-f sub-o] :as m}
        (into {}
              (map (juxt identity gensym))
              '[sub-pe sub-f sub-o])
        gsym (get m sym)
        _ (assert (symbol? gsym))]
    `(letfn [(~sub-pe [st# opts#]
               (fn rec#
                 ([ty#] (rec# ty# opts#))
                 ([ty# opts#] (~f ty# opts#
                                  {:type-rec st#
                                   :pathelem-rec (~sub-pe st# opts#)}))))
             (~sub-f [st# opts#]
               (fn rec#
                 ([ty#] (rec# ty# opts#))
                 ([ty# opts#] (~f ty# opts#
                                  {:type-rec st#
                                   :filter-rec (~sub-f st# opts#)
                                   :pathelem-rec (~sub-pe st# opts#)}))))
             (~sub-o [st# opts#]
               (fn rec#
                 ([ty#] (rec# ty# opts#))
                 ([ty# opts#] (~f ty# opts#
                                  {:type-rec st#
                                   :object-rec (~sub-o st# opts#)
                                   :pathelem-rec (~sub-pe st# opts#)}))))]
       (~gsym ~st ~opts))))

(defmacro def-sub-functions []
  (let [mk-sub-fn (fn [sym]
                    (let [st (gensym 'st)
                          f (gensym 'f)
                          opts (gensym 'opts)]
                      `(defmacro ~sym [~st ~f ~opts]
                         (assert (and (seq? ~f)
                                      ('#{quote} (first ~f))
                                      (qualified-symbol? (second ~f)))
                                 (str "Must provide qualified symbol (like `call-subst-dots*): " ~f))
                         (def-sub-function* '~sym ~st (second ~f) ~opts))))]
    `(do ~@(map mk-sub-fn '[sub-pe sub-f sub-o]))))

;; defines sub-pe, sub-f, sub-o
(def-sub-functions)

(comment
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
)

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.app-error
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check.utils :as cu]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.ast-utils :as ast-u]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.subtype :as sub]
            [clojure.string :as str]))

;; true if domains of l is a subtype of domains of r.
;; returns false if any of arg-ret-types are a subtype of r.
(defn domain-subtype? [l r arg-ret-types opts]
  {:pre [((every-pred r/Function?) l r)]}
  (boolean
    (when ((every-pred #(= :fixed (:kind %))) l r)
      (and (= (count (:dom l))
              (count (:dom r))
              (count arg-ret-types))
           (every? identity 
                   (map (fn [ld rd ad]
                          (and (sub/subtype? ld rd opts)
                               (not (sub/subtype? rd (r/ret-t ad) opts))))
                        (:dom l) 
                        (:dom r)
                        arg-ret-types))))))

(defn trim-arities [arities arg-ret-types opts]
  ;try and prune some of the arities
  ; Lots more improvements we can port from Typed Racket:
  ;  typecheck/tc-app-helper.rkt
  (let [matching-arities 
        (remove (fn [{:keys [dom rest drest kws rng kind]}]
                  ;remove arities that have a differing
                  ; number of fixed parameters than what we
                  ; require
                  (or
                    (and (= :fixed kind)
                         (not= (count dom)
                               (count arg-ret-types)))
                    ; remove if we don't have even the fixed args
                    (< (count arg-ret-types)
                       (count dom))))
                arities)
        remove-sub
        (reduce (fn [acc ar]
                  ;; assumes most general arities come last
                  (conj (into [] (remove #(domain-subtype? % ar arg-ret-types opts)) acc) 
                        ar))
                [] matching-arities)]
    (or (seq remove-sub)
        ;if we remove all the arities, default to all of them
        arities)))

;[Expr (Seqable Expr) (Seqable TCResult) (Option TCResult) Boolean
; -> Any]
(defn app-type-error [fexpr args fin arg-ret-types expected poly? opts]
  {:pre [(r/FnIntersection? fin)
         (or (not poly?)
             ((some-fn r/Poly? r/PolyDots?) poly?))]
   :post [(r/TCResult? %)]}
  (let [fin (apply r/make-FnIntersection (trim-arities (:types fin) arg-ret-types opts))
        static-method? (= :static-call (:op fexpr))
        instance-method? (= :instance-call (:op fexpr))
        method-sym (when (or static-method? instance-method?)
                     (cu/MethodExpr->qualsym fexpr opts))]
    (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                             (or (some-> fexpr (cu/expr-ns opts))
                                 (some-> (::vs/current-expr opts) (cu/expr-ns opts))))
      (err/tc-delayed-error
        (str
          (if poly?
            (str "Polymorphic " 
                 (cond static-method? "static method "
                       instance-method? "instance method "
                       :else "function "))
            (cond static-method? "Static method "
                  instance-method? "Instance method "
                  :else "Function "))
          (if (or static-method?
                  instance-method?)  
            method-sym
            (if fexpr
              (ast-u/emit-form-fn fexpr opts)
              "<NO FORM>"))
          " could not be applied to arguments:\n"
          (when poly?
            (let [names (cond 
                          (r/Poly? poly?) (c/Poly-fresh-symbols* poly?)
                          ;PolyDots
                          :else (c/PolyDots-fresh-symbols* poly?))
                  bnds (if (r/Poly? poly?)
                         (c/Poly-bbnds* names poly? opts)
                         (c/PolyDots-bbnds* names poly? opts))]
              (str "Polymorphic Variables:\n\t"
                   (str/join "\n\t"
                             (map (partial apply pr-str)
                                  (map (fn [bnd nme]
                                         {:pre [((some-fn r/Bounds? r/Regex?) bnd)
                                                (symbol? nme)]}
                                         (cond
                                           (r/Regex? bnd) (do (assert (= r/dotted-no-bounds bnd)
                                                                      "TODO interesting dotted bounds")
                                                              [nme :..])
                                           :else (let [_ (assert (r/Bounds? bnd) [(pr-str bnd)])
                                                       {:keys [lower-bound upper-bound]} bnd]
                                                   (concat [nme]
                                                           (when-not (= r/-any upper-bound)
                                                             [:< (prs/unparse-type upper-bound opts)])
                                                           (when-not (r/Bottom? lower-bound)
                                                             [:> (prs/unparse-type lower-bound opts)])))))
                                       bnds (map (comp #(prs/unparse-type % opts) r/make-F) names)))))))
          "\n\nDomains:\n\t" 
          (str/join "\n\t" 
                    (map (partial apply pr-str) 
                         (map (fn [{:keys [dom rest drest kws prest pdot]}]
                                (concat (map #(prs/unparse-type % opts) dom)
                                        (when rest
                                          [(prs/unparse-type rest opts) :*])
                                        (when-some [{:keys [pre-type name]} drest]
                                          [(prs/unparse-type pre-type opts)
                                           :..
                                           (-> name r/make-F r/F-original-name)])
                                        (letfn [(readable-kw-map [m]
                                                  (reduce-kv (fn [m k v]
                                                               {:keys [(r/Value? k)]}
                                                               (assoc m (:val k) (prs/unparse-type v opts)))
                                                             {} m))]
                                          (when-some [{:keys [mandatory optional]} kws]
                                            (concat ['&]
                                                    (when (seq mandatory)
                                                      [:mandatory (readable-kw-map mandatory)])
                                                    (when (seq optional)
                                                      [:optional (readable-kw-map optional)]))))
                                        (when prest
                                          [(prs/unparse-type prest opts) '<*])
                                        (when-some [{:keys [pre-type name]} pdot]
                                          [(prs/unparse-type pre-type opts)
                                           '<...
                                           (-> name r/make-F r/F-original-name)])))
                              (:types fin))))
          "\n\n"
          "Arguments:\n\t" (apply prn-str (mapv (comp #(prs/unparse-type % opts) r/ret-t) arg-ret-types))
          "\n"
          "Ranges:\n\t"
          (str/join "\n\t" 
                    (map (partial apply pr-str) (map (comp #(prs/unparse-result % opts) :rng) (:types fin))))
          "\n\n"
          (when-some [t (some-> expected r/ret-t)]
            (when-not (r/wild? t) 
              (str "with expected type:\n\t" (pr-str (prs/unparse-type t opts)) "\n\n")))
          #_#_"in: "
          (if fexpr
            (if (or static-method? instance-method?)
              (ast-u/emit-form-fn fexpr opts)
              (list* (ast-u/emit-form-fn fexpr opts)
                     (map #(ast-u/emit-form-fn % opts) args)))
            "<NO FORM>"))
        {:expected expected
         :return (or expected (r/ret r/Err))}
        opts))))

(defn polyapp-type-error [fexpr args fexpr-type arg-ret-types expected opts]
  {:pre [((some-fn r/Poly? r/PolyDots?) fexpr-type)]
   :post [(r/TCResult? %)]}
  (let [fin (if (r/Poly? fexpr-type)
              (c/Poly-body* (c/Poly-fresh-symbols* fexpr-type) fexpr-type opts)
              (c/PolyDots-body* (c/PolyDots-fresh-symbols* fexpr-type) fexpr-type opts))]
    (app-type-error fexpr args fin arg-ret-types expected fexpr-type opts)))

(defn plainapp-type-error [fexpr args fexpr-type arg-ret-types expected opts]
  {:pre [(r/FnIntersection? fexpr-type)]
   :post [(r/TCResult? %)]}
  (app-type-error fexpr args fexpr-type arg-ret-types expected false opts))

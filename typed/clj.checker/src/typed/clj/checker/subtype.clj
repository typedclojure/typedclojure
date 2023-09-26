;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.subtype
  "Use [[subtype?]] to check if s <: t, and [[subtype-filter?]] for filters."
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.set :as set]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.check :as chk]
            [typed.clj.checker.assoc-utils :as assoc-u]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.datatype-ancestor-env :as ancest]
            [typed.cljc.checker.filter-ops :as fops]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.path-rep :as pth-rep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defn ^:private gen-repeat [times repeated]
  (reduce into [] (repeat times repeated)))

(defn ^:private every?'
  "Like `every?`, but supports varargs."
  ([f coll]
   (reduce (fn [acc e]
             (if (f e)
               true
               (reduced false)))
           true
           coll))
  ([f c1 c2]
   (loop [s1 (seq c1) s2 (seq c2)]
     (or (not (and s1 s2))
         (and (boolean (f (first s1) (first s2)))
              (recur (next s1) (next s2))))))
  ([f c1 c2 c3]
   (loop [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
     (or (not (and s1 s2 s3))
         (and (boolean (f (first s1) (first s2) (first s3)))
              (recur (next s1) (next s2) (next s3))))))
  ([f c1 c2 c3 c4]
   (loop [s1 (seq c1) s2 (seq c2) s3 (seq c3) s4 (seq c4)]
     (or (not (and s1 s2 s3 s4))
         (and (boolean (f (first s1) (first s2) (first s3) (first s4)))
              (recur (next s1) (next s2) (next s3) (next s4))))))
  ([f c1 c2 c3 c4 & colls]
   (loop [ss (list* (seq c1) (seq c2) (seq c3) (seq c4) (map seq colls))]
     (or (not (every?' identity ss))
         (and (boolean (apply f (map first ss)))
              (recur (map next ss)))))))

;(defalias Seen Any)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

;[Type Type -> nil]
(defmacro ^:private report-not-subtypes [s t]
  `(do ;(prn "not" ~s ~t)
       nil))

;keeps track of currently seen subtype relations for recursive types.
;(Nilable (Set [Type Type]))
;; nil => not subtyping
;; set => subtyping, contains the 'seen' pairs since the last subtype? call.
;;        internal functions like subtypeA* don't update *sub-current-seen*.
(defonce ^:dynamic *sub-current-seen* nil)

(defn currently-subtyping? []
  (some? *sub-current-seen*))

(declare subtypeA* supertype-of-one-arr subtypes*-varargs subtype?)

(defmacro ^:private do-top-level-subtype-using [f & args]
  ;; maintain *sub-current-seen* for subtyping checks that span
  ;; beyond just this file (eg., subtyping => cs-gen => subtyping)
  `(let [f# (fn [A#] (~f A# ~@args))]
     (if-some [A# *sub-current-seen*]
       (f# A#)
       (let [A# #{}]
         (binding [*sub-current-seen* A#]
           (f# A#))))))

;[(t/Vec Type) (t/Vec Type) Type -> Boolean]
(defn subtypes-varargs?
  "True if argtys are under dom"
  [argtys dom rst kws]
  (boolean
    (do-top-level-subtype-using
      subtypes*-varargs argtys dom rst kws)))

(defn subtypes-prest?
  "True if argtys are under dom and prest"
  [argtys dom prest]
  (let [dom-count (count dom)
        [argtys-dom argtys-rest] (split-at dom-count argtys)]
    (boolean
      (and (do-top-level-subtype-using
             subtypes*-varargs (vec argtys-dom) dom nil nil)
           (do-top-level-subtype-using
             subtypeA* (r/-hvec (vec argtys-rest)) prest)))))

;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument (the current-seen subtypes), called by subtype
;
; In short, only call subtype (or subtype?)

(declare subtype-filter*)

(defonce subtype-cache (atom {}))

; (ann reset-subtype-cache [-> nil])
(defn reset-subtype-cache []
  (reset! subtype-cache {})
  nil)

;[Type Type -> Boolean]
(defn subtype? [s t]
  {:post [(boolean? %)]}
  (letfn [(do-subtype []
            (boolean
              (do-top-level-subtype-using subtypeA* s t)))]
    (if true ;; true == disable cache
      (do-subtype)
      (if-let [[_ res] (find @subtype-cache [s t])]
        res
        (let [res (do-subtype)]
          (when-not (currently-subtyping?)
            (swap! subtype-cache assoc [s t] res))
          res)))))

(defn subtype-filter? [f1 f2]
  (boolean
    (do-top-level-subtype-using
      subtype-filter* f1 f2)))

(declare subtype-type-filter* subtype-not-type-filter*)

(defn subtype-type-filter? [f1 f2]
  (boolean
    (do-top-level-subtype-using
      subtype-type-filter* f1 f2)))

(defn subtype-not-type-filter? [f1 f2]
  (boolean
    (do-top-level-subtype-using
      subtype-not-type-filter* f1 f2)))

;[(Map Symbol Bounds) (Map Symbol Bounds) (t/Seqable Type) (t/Seqable Type)
;  -> Boolean]
(defn unify [X Y S T R]
  (boolean 
    (u/handle-cs-gen-failure
      (ind/infer X Y S T R))))

(declare protocol-extenders
         subtype-datatypes-or-records subtype-Result subtype-PrimitiveArray
         subtype-CountRange subtype-TypeFn subtype-RClass subtype-rclass-or-datatype-with-protocol
         boxed-primitives subtype-datatype-rclass subtype-TApp)

(defn subtype-HSet [A s t]
  {:pre [(r/HSet? s)
         (r/HSet? t)]}
  (if (and (= (:fixed s) (:fixed t))
           (:complete? s)
           (:complete? t))
    A
    (report-not-subtypes s t)))

(defn subtype-compatible-HSequential [A s t]
  {:pre [(r/HSequential? s)
         (r/HSequential? t)
         (r/compatible-HSequential-kind? (:kind s) (:kind t))]}
  (if (and (cond
             ; simple case, no rest, drest, repeat types
             (and (not-any? :rest [s t])
                  (not-any? :drest [s t])
                  (not-any? :repeat [s t]))
             (let []
               (and (= (count (:types s))
                       (count (:types t)))
                    (every?' (partial subtypeA* A) (:types s) (:types t))))

             ; repeat on left
             (and (:repeat s)
                  (not (:drest t)))
             (let [s-types (:types s)
                   t-types (:types t)
                   s-types-count (count s-types)
                   t-types-count (count t-types)]
               (cond
                 (:rest t)
                 (and (= 1 s-types-count)
                      (every?' (partial subtypeA* A)
                               (repeat (first s-types))
                               t-types)
                      (subtypeA* A (first s-types) (:rest t)))

                 ; both s & t have :repeat
                 (:repeat t)
                 (and (<= t-types-count
                          s-types-count)
                      (zero? (rem s-types-count
                                  t-types-count))
                      (every?' (partial subtypeA* A)
                               s-types
                               (gen-repeat (/ (count s-types)
                                              (count t-types))
                                           t-types)))

                 ; nothing on right
                 :else
                 false))

             ; repeat on right
             (and (:repeat t)
                  (not (:drest s)))
             (let [s-types (:types s)
                   t-types (:types t)
                   s-types-count (count s-types)
                   t-types-count (count t-types)]
               (if (:rest s)
                 (and (= 1 t-types-count)
                      (every?' (partial subtypeA* A)
                               s-types
                               (repeat (first t-types)))
                      (subtypeA* A (:rest s) (first t-types)))

                 ; nothing on left
                 (and (or (zero? s-types-count)
                          (<= t-types-count
                              s-types-count))
                      (if (zero? (rem s-types-count
                                      t-types-count))
                        (every?' (partial subtypeA* A)
                                 s-types
                                 (gen-repeat (/ s-types-count
                                                t-types-count)
                                             t-types))
                        false))))

             ; rest on right
             (and (:rest t)
                  (not ((some-fn :drest :repeat) s)))
             (and (>= (count (:types s))
                      (count (:types t)))
                  (or (not (:rest s))
                      (subtypeA* A (:rest s) (:rest t)))
                  ;pad t to the right
                  (every?' (partial subtypeA* A)
                           (:types s)
                           (concat (:types t)
                                   (repeat (- (count (:types s)) (count (:types t)))
                                           (:rest t)))))

             (and (:drest s)
                  (:rest t))
             (and
               (every?' (partial subtypeA* A)
                        (:types s)
                        (concat (:types t)
                                (repeat (- (count (:types s)) (count (:types t)))
                                        (:rest t))))
               (r/Top? (:rest t)))

             ;TODO other cases
             :else nil)
           ; ignore interesting results
           (every?' (fn _hvec1 [f1 f2]
                      (or (= (fops/-FS fr/-top fr/-top) f2)
                          (= f1 f2)))
                    (:fs s) (:fs t))
           ; ignore interesting results
           (every?' (fn _hvec2 [o1 o2]
                      (or (orep/EmptyObject? o2)
                          (= o1 o2)))
                    (:objects s) (:objects t)))
    A
    (report-not-subtypes s t)))

(defn simplify-In [t]
  {:pre [(r/Intersection? t)]}
  (:types t)
  ;; very slow
  #_
  (let [mi (apply c/In (:types t))]
    (if (r/Intersection? mi)
      (:types mi)
      [mi])))

(defn subtype-symbolic-closure
  "Return A if symbolic closure s is a subtype of t, otherwise nil."
  [A s t]
  {:pre [(set? A)
         (r/SymbolicClosure? s)
         (r/AnyType? t)]
   :post [((some-fn nil? set?) %)]}
  ;(prn :subtype-symbolic-closure s t)
  (with-bindings (assoc (:bindings s)
                        #'vs/*delayed-errors* (err/-init-delayed-errors))
    (when (try (binding [*sub-current-seen* A]
                 (chk/check-expr (:fexpr s) (r/ret t)))
               (catch clojure.lang.ExceptionInfo e
                 ;(prn e) ;;tmp
                 (when-not (-> e ex-data err/tc-error?)
                   (throw e))))
      ;(prn @vs/*delayed-errors*) ;;tmp
      (when (empty? @vs/*delayed-errors*)
        A))))

(defn check-symbolic-closure
  "Check symbolic closure s against type t (propagating all errors to caller),
  returning the checked expression."
  [s t]
  {:pre [(r/SymbolicClosure? s)
         (r/AnyType? t)]
   :post [(-> % u/expr-type r/TCResult?)]}
  ;(prn :check-symbolic-closure s t)
  (with-bindings (dissoc (:bindings s)
                         ;; hmm, additional error msg context needed to orient the user
                         ;; to the problem? symbolic closure will be blamed
                         #'vs/*delayed-errors*)
    (chk/check-expr (:fexpr s) (r/ret t))))

(defn subtype-regex [A s t]
  {:pre [(r/Regex? s)
         (r/Regex? t)]}
  ;;TODO
  (report-not-subtypes s t))

;TODO replace hardcoding cases for unfolding Mu? etc. with a single case for unresolved types.
;[(t/Set '[Type Type]) Type Type -> (t/Nilable (t/Set '[Type Type]))]
(defn ^:private subtypeA* [A s t]
  {:pre [(r/AnyType? s)
         (r/AnyType? t)]
   ;; for recur
   ;:post [(or (set? %) (nil? %))]
   }
  ;(prn "subtypeA*" s t)
  (if (or ; FIXME TypeFn's are probably not between Top/Bottom
          (r/Top? t)
          (r/wild? t)
          (r/Bottom? s)
          ;; Unchecked is both bottom and top
          (r/Unchecked? s)
          (r/Unchecked? t)
          ;TCError is top and bottom
          (r/TCError? s)
          (r/TCError? t)
          (contains? A [s t])
          (= s t))
    A
    (let [A (conj A [s t])]
      (cond
        (or (r/TCResult? s)
            (r/TCResult? t))
        (assert nil "Cannot give TCResult to subtype")

        ; use bounds to determine subtyping between frees and types
        ; 2 frees of the same name are handled in the (= s t) case.
        (and (r/F? s)
             (let [{:keys [upper-bound lower-bound] :as bnd} (free-ops/free-with-name-bnds (:name s))]
               (if-not bnd 
                 (do #_(err/int-error (str "No bounds for " (:name s)))
                     nil)
                 (and (subtypeA* A upper-bound t)
                      (subtypeA* A lower-bound t)))))
        A

        (and (r/F? t)
             (let [{:keys [upper-bound lower-bound] :as bnd} (free-ops/free-with-name-bnds (:name t))]
               (if-not bnd 
                 (do #_(err/int-error (str "No bounds for " (:name t)))
                     nil)
                 (and (subtypeA* A s upper-bound)
                      (subtypeA* A s lower-bound)))))
        A

        (r/TypeOf? s)
        (recur A (c/resolve-TypeOf s) t)

        (r/TypeOf? t)
        (recur A s (c/resolve-TypeOf t))

        (and (r/Value? s)
             (r/Value? t))
        ;already (not= s t)
        (report-not-subtypes s t)

        ;; handle before unwrapping polymorphic types
        (and (r/SymbolicClosure? s)
             ((some-fn r/FnIntersection? r/Poly? r/PolyDots?) (c/fully-resolve-type t)))
        (or (subtype-symbolic-closure A s t)
            (report-not-subtypes s t))

        (and (r/Poly? s)
             (r/Poly? t)
             (= (:nbound s) (:nbound t)))
        (let [;instantiate both sides with the same fresh variables
              names (repeatedly (:nbound s) gensym)
              bbnds1 (c/Poly-bbnds* names s)
              bbnds2 (c/Poly-bbnds* names t)
              b1 (c/Poly-body* names s)
              b2 (c/Poly-body* names t)]
          (if (and (= bbnds1 bbnds2)
                   (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds1)
                     (subtypeA* A b1 b2)))
            A
            (report-not-subtypes s t)))

        ;use unification to see if we can use the Poly type here
        (and (r/Poly? s)
             (let [names (c/Poly-fresh-symbols* s)
                   bnds (c/Poly-bbnds* names s)
                   b1 (c/Poly-body* names s)
                   ;_ (prn "try unify on left")
                   u (unify (zipmap names bnds) {} [b1] [t] r/-any)]
               ;(prn "unified on left")
               u))
        A

        (and (r/PolyDots? s)
             (let [names (c/PolyDots-fresh-symbols* s)
                   bnds (c/PolyDots-bbnds* names s)
                   b1 (c/PolyDots-body* names s)
                   ;_ (prn "try PolyDots unify on left")
                   u (unify (zipmap (butlast names) (butlast bnds)) {(last names) (last bnds)} 
                            [b1] [t] r/-any)]
               ;(prn "unified on left" u)
               u))
        A

        ;; go after presumably cheaper unification cases
        (and ((some-fn r/Poly? r/PolyDots?) s)
             (r/FnIntersection? t)
             (= 1 (count (:types t)))
             (every? #(= :fixed (:kind %)) (:types t))
             (binding [vs/*delayed-errors* (err/-init-delayed-errors)]
               (let [_ ((requiring-resolve 'typed.cljc.checker.check.funapp/check-funapp)
                        nil nil
                        (r/ret s)
                        (mapv r/ret (-> t :types first :dom))
                        (-> t :types first :rng r/Result->TCResult))]
                 (empty? @vs/*delayed-errors*))))
        A

        (and (r/Poly? t)
             (let [names (c/Poly-fresh-symbols* t)
                   b (c/Poly-body* names t)]
               (empty? (frees/fv t))))
        ;;FIXME don't recompute Poly-body*
        (let [names (c/Poly-fresh-symbols* t)
              b (c/Poly-body* names t)]
          (recur A s b))

        (r/Name? s)
        (recur A (c/resolve-Name s) t)

        (r/Name? t)
        (recur A s (c/resolve-Name t))

        ;;only pay cost of dynamic binding when absolutely necessary (eg., before unfolding Mu)
        (r/Mu? s)
        (binding [*sub-current-seen* A]
          (subtypeA* A (c/unfold s) t))

        (r/Mu? t)
        (binding [*sub-current-seen* A]
          (subtypeA* A s (c/unfold t)))

        (r/App? s)
        (recur A (c/resolve-App s) t)

        (r/App? t)
        (recur A s (c/resolve-App t))

        (r/Bottom? t)
        (report-not-subtypes s t)

        (and (r/TApp? s)
             (r/TApp? t)
             (= (c/fully-resolve-type (:rator s))
                (c/fully-resolve-type (:rator t))))
        (subtype-TApp A s t)

        (and (r/TApp? s)
             (r/TypeFn? (c/fully-resolve-type (:rator s))))
        (let [{:keys [rands]} s
              rator (c/fully-resolve-type (:rator s))]
          (cond
            (r/F? rator) (report-not-subtypes s t)

            (r/TypeFn? rator)
            (let [names (c/TypeFn-fresh-symbols* rator)
                  bbnds (c/TypeFn-bbnds* names rator)
                  res (c/instantiate-typefn rator rands :names names)]
              (subtypeA* A res t))

            :else (err/int-error (str "First argument to TApp must be TFn, actual: " (prs/unparse-type rator)))))

        (and (r/TApp? t)
             (r/TypeFn? (c/fully-resolve-type (:rator t))))
        (let [{:keys [rands]} t
              rator (c/fully-resolve-type (:rator t))]
          (cond
            (r/F? rator) (report-not-subtypes s t)

            (r/TypeFn? rator)
            (let [names (c/TypeFn-fresh-symbols* rator)
                  res (c/instantiate-typefn rator rands :names names)]
              (recur A s res))

            :else (err/int-error (str "First argument to TApp must be TFn, actual: " (prs/unparse-type rator)))))

        (r/Union? s)
        (if (every? (fn union-left [s] (subtypeA* A s t)) (:types s))
          A
          (report-not-subtypes s t))

        (r/Union? t)
        (some (fn union-right [t] (subtypeA* A s t)) (:types t))

        (and (r/FnIntersection? s)
             (r/FnIntersection? t))
        (loop [A* A
               arr2 (:types t)]
          (let [arr1 (:types s)]
            (if (empty? arr2) 
              A*
              (if-let [A (supertype-of-one-arr A* (first arr2) arr1)]
                (recur A (next arr2))
                (report-not-subtypes s t)))))

;does it matter what order the Intersection cases are?
        (r/Intersection? t)
        (let [ts (simplify-In t)]
          (if (every? #(subtypeA* A s %) ts)
            A
            (report-not-subtypes s t)))

        (r/Intersection? s)
        (let [ss (simplify-In s)]
          (some #(subtypeA* A % t) ss))

        (and (r/Extends? s)
             (r/Extends? t))
        (if (and ;all positive information matches.
                 ; Each t should occur in at least one s.
                 (every? (fn _extends-t [t*]
                           (some #(subtypeA* A % t*) (:extends s)))
                         (:extends t))
                 ;lhs does not explicitly implement any forbidden types.
                 ; No negative t should be a supertype of a positive s
                 (not-any? (fn extends-not-t [not-t*]
                             (some #(subtypeA* A % not-t*) (:extends s)))
                           (:without t))
                 ;lhs explicitly disallows same types as rhs
                 ; Each negative t should be a supertype of some negative s
                 (every? (fn _extends-without-t [not-t*]
                           (some #(subtypeA* A % not-t*) (:without s)))
                         (:without t)))
          A
          (report-not-subtypes s t))

        (r/Extends? s)
        (if (and (some #(subtypeA* A % t) (:extends s))
                 (not-any? #(subtypeA* A % t) (:without s)))
          A
          (report-not-subtypes s t))

        (r/Extends? t)
        (if (and (every? #(subtypeA* A s %) (:extends t))
                 (not-any? #(subtypeA* A s %) (:without t)))
          A
          (report-not-subtypes s t))

        (and (r/TopFunction? t)
             (r/FnIntersection? s))
        A

        ;       B <: A
        ;_______________________________
        ; (Not A) <: (Not B)
        (and (r/NotType? s)
             (r/NotType? t))
        (subtypeA* A (:type t) (:type s))

        ;  A <!: B  A is not free  B is not free
        ;________________________________________
        ; A <: (Not B)
;   Should this also require (fv s) U (fv t) to be empty?
        (r/NotType? t)
        (if (and (not-any? (some-fn r/B? r/F?) [s (:type t)])
                 (not (subtypeA* A s (:type t))))
          A
          (report-not-subtypes s t))

; delegate to NotType
        (r/DifferenceType? s)
        (recur A
               (apply c/In (:type s) (map r/NotType-maker (:without s)))
               t)

        (r/DifferenceType? t)
        (recur A
               s
               (apply c/In (:type t) (map r/NotType-maker (:without t))))

        (or (and (r/GetType? s)
                 (c/Get-requires-resolving? s))
            (and (r/MergeType? s)
                 (c/Merge-requires-resolving? s)))
        (recur A (c/-resolve s) t)

        (or (and (r/GetType? t)
                 (c/Get-requires-resolving? t))
            (and (r/MergeType? t)
                 (c/Merge-requires-resolving? t)))
        (recur A s (c/-resolve t))

        (and (r/AssocType? s)
             (r/RClass? t)
             ; (Map xx yy)
             (= 'clojure.lang.IPersistentMap (:the-class t)))
        (let [{:keys [target entries dentries]} s
              {:keys [poly? the-class]} t
              ; _ (when-not (nil? dentries) (err/nyi-error (pr-str "NYI subtype of dentries AssocType " s)))
              ; we assume its all right
              entries-keys (map first entries)
              entries-vals (map second entries)]
          (if (and (subtypeA* A target t)
                   (every?' (partial subtypeA* A) entries-keys (repeat (first poly?)))
                   (every?' (partial subtypeA* A) entries-vals (repeat (second poly?))))
            A
            (report-not-subtypes s t)))

        (and (r/AssocType? s)
             (r/AssocType? t)
             (r/F? (:target s))
             (r/F? (:target t))
             (not-any? :dentries [s t]))
        (if (= (:target s) (:target t))
          (recur A
                 (apply assoc-u/assoc-pairs-noret (c/-complete-hmap {}) (:entries s))
                 (apply assoc-u/assoc-pairs-noret (c/-complete-hmap {}) (:entries t)))
          (report-not-subtypes s t))

        (and (r/AssocType? s)
             (r/F? (:target s))
             (not (r/AssocType? t)))
        (let [bnds (free-ops/free-with-name-bnds (-> s :target :name))
              _ (assert bnds
                        (str "Bounds not found for free variable: " (-> s :target :name)))]
          (if (subtypeA* A (:upper-bound bnds) t)
            (recur A
                   (apply assoc-u/assoc-pairs-noret (c/-complete-hmap {}) (:entries s))
                   t)
            (report-not-subtypes s t)))
      
        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? s)
             (not (r/F? (:target s))))
        (if-let [s-or-n (apply assoc-u/assoc-pairs-noret (:target s) (:entries s))]
          (recur A s-or-n t)
          (report-not-subtypes s t))

        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? t)
             (not (r/F? (:target t))))
        (if-let [t-or-n (apply assoc-u/assoc-pairs-noret (:target t) (:entries t))]
          (recur A s t-or-n)
          (report-not-subtypes s t))

        (and (r/HSequential? s)
             (r/HSequential? t)
             (r/compatible-HSequential-kind? (:kind s) (:kind t)))
        (subtype-compatible-HSequential A s t)

        ; repeat Heterogeneous* can always accept nil
        (and (r/Nil? s)
             (r/HSequential? t)
             (:repeat t))
        A

        (and (r/HSequential? s)
             (r/TopHSequential? t))
        A

        ;every rtype entry must be in ltypes
        ;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
        (and (r/HeterogeneousMap? s)
             (r/HeterogeneousMap? t))
        (let [; convention: prefix things on left with l, right with r
              {ltypes :types labsent :absent-keys :as s} s
              {rtypes :types rabsent :absent-keys :as t} t]
          (if (and ; if t is complete, s must be complete ..
                   (if (c/complete-hmap? t)
                     (if (c/complete-hmap? s)
                       ; mandatory keys on the right must appear as
                       ; mandatory on the left, but extra keys may appear
                       ; on the left
                       (and (let [right-mkeys (set (keys rtypes))
                                  left-mkeys (set (keys ltypes))]
                              (set/subset? right-mkeys
                                           left-mkeys))
                            ; extra mandatory keys on the left must appear
                            ; as optional on the right
                            (let [left-extra-mkeys (set/difference (set (keys ltypes))
                                                                       (set (keys rtypes)))
                                  right-optional-keys (set (keys (:optional t)))]
                              (set/subset? left-extra-mkeys
                                           right-optional-keys)))
                            ;Note:
                            ; optional key keys on t must be optional or mandatory or absent in s,
                            ; which is always the case so we don't need to check.
                       false)
                     true)
                   ; all absent keys in t should be absent in s
                   (every? (fn [rabsent-key]
                             ; Subtyping is good if rabsent-key is:
                             ; 1. Absent in s
                             ; 2. Not present in s, but s is complete
                             (or ((set labsent) rabsent-key)
                                 (when (c/complete-hmap? s)
                                   (not ((set (keys ltypes)) rabsent-key)))))
                           rabsent)
                   ; all present keys in t should be present in s
                   (every? (fn [[k v]]
                             (when-let [t (get ltypes k)]
                               (subtypeA* A t v)))
                           rtypes)
                   ; all optional keys in t should match optional/mandatory entries in s
                   (every? (fn [[k v]]
                             (let [matches-entry?
                                   (if-let [actual-v 
                                            ((merge-with c/In
                                                         (:types s)
                                                         (:optional s))
                                             k)]
                                     (subtypeA* A actual-v v)
                                     (c/complete-hmap? s))]
                               (cond
                                 (c/partial-hmap? s)
                                 (or (contains? (:absent-keys s) k)
                                     matches-entry?)
                                 :else matches-entry?)))
                           (:optional t)))
            A
            (report-not-subtypes s t)))

        (r/HeterogeneousMap? s)
        (recur A (c/upcast-hmap s) t)

        ;; JSObj is covariant, taking after TypeScript & Google Closure. Obviously unsound.
        (and (r/JSObj? s)
             (r/JSObj? t))
        (let [; convention: prefix things on left with l, right with r
              {ltypes :types} s
              {rtypes :types} t]
          (if (every? (fn [[k rt]]
                        (when-let [lt (get ltypes k)]
                          (subtypeA* A lt rt)))
                      rtypes)
            A
            (report-not-subtypes s t)))

        (and (r/HSet? s)
             (r/HSet? t))
        (subtype-HSet A s t)

        (r/HSet? s)
        (recur A (c/upcast-hset s) t)

        (r/KwArgsSeq? s)
        (if (r/TopKwArgsSeq? t)
          A
          (recur A (c/upcast-kw-args-seq s) t))

        ; TODO add repeat support
        (r/HSequential? s)
        (recur A (c/upcast-HSequential s) t)

; The order of checking protocols and datatypes is subtle.
; It is easier to calculate the ancestors of a datatype than
; the descendants of a protocol, so Datatype <: Any comes 
; before Protocol <: Any.
        (and (r/Protocol? s)
             (r/Protocol? t))
        (let [{var1 :the-var variances* :variances poly1 :poly?} s
              {var2 :the-var poly2 :poly?} t]
          ;(prn "protocols subtype" s t)
          (if (and (= var1 var2)
                   (every?' (fn _prcol-variance [v l r]
                              (case v
                                :covariant (subtypeA* A l r)
                                :contravariant (subtypeA* A r l)
                                :invariant (and (subtypeA* A l r)
                                                (subtypeA* A r l))))
                            variances* poly1 poly2))
            A
            (report-not-subtypes s t)))

        (and (r/DataType? s)
             (r/DataType? t))
        (subtype-datatypes-or-records A s t)

        (and ((some-fn r/RClass? r/DataType?) s)
             (r/Protocol? t))
        (subtype-rclass-or-datatype-with-protocol A s t)

        (and (r/Nil? s)
             (r/Protocol? t)
             (impl/checking-clojure?))
        (if (contains? (c/Protocol-normal-extenders t) nil)
          A
          (report-not-subtypes s t))

        ((some-fn r/JSNull? r/JSUndefined?) s)
        (recur A r/-nil t)

        ;values are subtypes of their classes
        (r/Value? s)
        (let [sval (:val s)]
          (impl/impl-case
            :clojure (cond 
                       ; this is after the nil <: Protocol case, so we fail
                       (nil? sval) (report-not-subtypes s t)
                       ; this is a faster path than the final case
                       (r/RClass? t) (let [cls (let [cls (coerce/symbol->Class (:the-class t))]
                                                 (or (boxed-primitives cls)
                                                     cls))]
                                       (cond
                                         (#{Integer Long} cls) (if (or (instance? Integer sval)
                                                                       (instance? Long sval))
                                                                 A
                                                                 (report-not-subtypes s t))
                                         ;handle string-as-seqable
                                         (string? sval) (subtypeA* A (c/RClass-of String) t)
                                         :else (if (instance? cls sval) 
                                                 A
                                                 (report-not-subtypes s t))))
                       :else (recur A
                                    (apply c/In (c/RClass-of (class sval))
                                           (cond
                                             ;keyword values are functions
                                             (keyword? sval) [(c/keyword->Fn sval)]
                                             ;strings have a known length as a seqable
                                             (string? sval) [(r/make-ExactCountRange (count sval))]))
                                    t))
            :cljs (cond
                    (integer? sval) (recur A (r/CLJSInteger-maker) t)
                    (number? sval) (recur A (r/JSNumber-maker) t)
                    (string? sval) (recur A (r/JSString-maker) t)
                    (boolean? sval) (recur A (r/JSBoolean-maker) t)
                    (symbol? sval) (recur A (c/DataType-of 'cljs.core/Symbol) t)
                    (keyword? sval) (recur A (c/DataType-of 'cljs.core/Keyword) t)
                    :else (report-not-subtypes s t))))

        (and (r/Result? s)
             (r/Result? t))
        (subtype-Result A s t)

        (and (r/PrimitiveArray? s)
             (r/PrimitiveArray? t))
        (subtype-PrimitiveArray A s t)

        (r/PrimitiveArray? s)
        (recur A (r/PrimitiveArray-maker Object r/-any r/-any) t)
      
        (and (r/TypeFn? s)
             (r/TypeFn? t))
        (subtype-TypeFn A s t)

        (and (r/RClass? s)
             (r/RClass? t))
        (subtype-RClass A s t)

        (and (r/DataType? s)
             (r/RClass? t))
        (subtype-datatype-rclass A s t)

        ; handles classes with FnIntersection ancestors
        (and (r/RClass? s)
             (r/FnIntersection? t))
        (cond
          ; Var doesn't actually have an FnIntersection ancestor,
          ; but this case simulates it.
          (#{'clojure.lang.Var} (:the-class s))
          (let [[_ read-type :as poly] (:poly? s)
                _ (when-not (#{2} (count (:poly? s)))
                    (err/int-error
                      (str "Assuming Var takes 2 arguments, "
                           "given " (count (:poly? s)))))]
            (recur A read-type t))

          :else (some #(when (r/FnIntersection? %)
                         (subtypeA* A % t))
                      (map c/fully-resolve-type (c/RClass-supers* s))))

        ; handles classes with heterogeneous vector ancestors (eg. IMapEntry)
        (and (r/RClass? s)
             ((some-fn r/HSequential?
                       r/TopHSequential?)
              t))
        (some #(when (r/HSequential? %)
                 (subtypeA* A % t))
              (map c/fully-resolve-type (c/RClass-supers* s)))

        ; hack for FnIntersection <: clojure.lang.IFn
        (when (and (r/FnIntersection? s)
                   (impl/checking-clojure?))
          (subtypeA* A (c/RClass-of clojure.lang.IFn) t))
        A

        (and (r/CountRange? s)
             (r/CountRange? t))
        (subtype-CountRange A s t)

        ; CLJS special types
        (and (r/CLJSInteger? s)
             (r/JSNumber? t))
        A

        (and (r/PolyDots? s)
             (r/PolyDots? t)
             (= (:nbound s) (:nbound t)))
        (let [;instantiate both sides with the same fresh variables
              names (repeatedly (:nbound s) gensym)
              bbnds1 (c/PolyDots-bbnds* names s)
              bbnds2 (c/PolyDots-bbnds* names t)
              b1 (c/PolyDots-body* names s)
              b2 (c/PolyDots-body* names t)]
          (if (and (= bbnds1 bbnds2)
                   (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds1)
                     (subtypeA* A b1 b2)))
            A
            (report-not-subtypes s t)))

        (and (r/Regex? s)
             (r/Regex? t))
        (subtype-regex A s t)

        ; TODO (All [r x ...] [x ... x -> r]) <: (All [r x] [x * -> r]) ?

        :else (report-not-subtypes s t)))))

(defn ^:private resolve-JS-reference [sym]
  (impl/assert-cljs)
  (cond
    (= "js" (namespace sym)) (c/JSNominal-with-unknown-params sym)
    :else (let [_ (assert nil "FIXME typed.clj.checker.analyze-cljs/analyze-qualified-symbol has been deleted")
                {{:keys [protocol-symbol name]} :info} ((requiring-resolve 'typed.clj.checker.analyze-cljs/analyze-qualified-symbol) sym)]
            (if protocol-symbol
              (c/Protocol-with-unknown-params name)
              (c/DataType-with-unknown-params name)))))


(defn protocol-extenders [p]
  {:pre [(r/Protocol? p)]
   :post [(every? r/Type? %)]}
  (impl/impl-case
    :clojure (let [exts (c/Protocol-normal-extenders p)]
               (for [ext exts]
                 (cond
                   (class? ext) (c/RClass-of-with-unknown-params ext)
                   (nil? ext) r/-nil
                   :else (throw (Exception. (str "What is this?" ext))))))
    :cljs (let [exts ((requiring-resolve 'typed.clj.checker.analyze-cljs/extenders) (:the-var p))]
            (for [ext exts]
              (cond
                (symbol? ext) (resolve-JS-reference ext)
                (nil? ext) r/-nil
                :else (throw (Exception. (str "What is this?" ext))))))))

;[(IPersistentSet '[Type Type]) (t/Seqable Type) (t/Seqable Type) (Option Type)
;  -> (IPersistentSet '[Type Type])]
(defn ^:private subtypes*-varargs-info [A0 argtys dom rst kws]
  {:pre [(vector? argtys)
         (vector? dom)
         ((some-fn nil? r/Type?) rst)
         ((some-fn nil? r/KwArgs?) kws)]
   :post [((some-fn map? nil?) %)]}
  (letfn [(all-mandatory-kws? [found-kws]
            {:pre [(set? found-kws)]}
            (or (not kws)
                (empty? (set/difference (set (keys (:mandatory kws)))
                                        found-kws))))]
    (loop [dom dom
           argtys argtys
           A A0
           found-kws #{}]
      (let [ndom (count dom)
            nargtys (count argtys)]
        (cond
          (and (zero? ndom) (zero? nargtys))
          (if (all-mandatory-kws? found-kws)
            {:result :ok
             :A A}
            (report-not-subtypes argtys dom))

          (zero? nargtys) (report-not-subtypes argtys dom)

          (zero? ndom)
          (cond rst
                (let [fargty (nth argtys 0)]
                  (if-some [A (subtypeA* A fargty rst)]
                    (recur dom (subvec argtys 1) A found-kws)
                    (report-not-subtypes fargty rst)))

                (and kws (<= 2 nargtys))
                (let [kw (c/fully-resolve-type (nth argtys 0))
                      val (nth argtys 1)
                      expected-val ((some-fn (:mandatory kws) (:optional kws))
                                    kw)]
                  (if (and expected-val (subtypeA* A val expected-val))
                    (recur dom (subvec argtys 2) A (conj found-kws kw))
                    (report-not-subtypes (subvec argtys 0 2) kws)))

                :else (report-not-subtypes argtys dom))

          :else
          (let [[arg-t] argtys
                [dom-t] dom]
            (if-some [A (subtypeA* A0 arg-t dom-t)]
              (recur (subvec dom 1) (subvec argtys 1) A found-kws)
              (report-not-subtypes arg-t dom-t))))))))

(defn ^:private subtypes*-varargs
  [A0 argtys dom rst kws]
  {:pre [(vector? argtys)
         (vector? dom)
         ((some-fn nil? r/Type?) rst)
         ((some-fn nil? r/KwArgs?) kws)]
   :post [((some-fn set? nil?) %)]}
  (let [{:keys [result A]} (subtypes*-varargs-info A0 argtys dom rst kws)]
    (when (= :ok result)
      A)))

;FIXME
(defn subtype-kwargs* [A s t]
  {:pre [((some-fn r/KwArgs? nil?) s)
         ((some-fn r/KwArgs? nil?) t)]}
  (if (= s t)
    A
    (err/nyi-error "subtype kwargs")))

;; simple co/contra-variance for ->
;[(IPersistentSet '[Type Type]) Function Function -> (IPersistentSet '[Type Type])]
(defn ^:private arr-subtype [A0 s t]
  {:pre [(r/Function? s)
         (r/Function? t)]}
  ;; top for functions is above everything
  (cond
    ;; top for functions is above everything
    (r/TopFunction? t) A0
    ;; the really simple case
    ((every-pred #(= :fixed (:kind %))) s t)
    (if (and (= (count (:dom s))
                (count (:dom t)))
             (some-> (reduce (fn [A* [s t]]
                               (cond-> (subtypeA* A* s t)
                                 nil? reduced))
                             A0
                             (map vector (:dom t) (:dom s)))
                     (subtypeA* (:rng s) (:rng t))))
      A0
      (report-not-subtypes s t))

    (and (:prest s)
         (:prest t))
    (if (and (= (count (:dom s))
                (count (:dom t)))
             (some-> (reduce (fn [A* [s t]]
                               (cond-> (subtypeA* A* s t)
                                 nil? reduced))
                             A0
                             (map vector (:dom t) (:dom s)))
                     (subtypeA* (:rng s) (:rng t)))
             (subtypeA* A0 (:prest s) (:prest t)))
      A0
      (report-not-subtypes s t))

    (and (:rest s)
         (:prest t))
    (let [subtype-list? (fn [s t]
                          (every? identity
                                  (for [s s
                                        t t]
                                    (subtypeA* A0 s t))))
          s-dom (:dom s)
          s-dom-count (count s-dom)
          t-dom (:dom t)
          t-dom-count (count t-dom)
          s-rest (:rest s)
          t-prest-types (-> t :prest :types)
          t-prest-types-count (count t-prest-types)]
      (if-not (and (subtypeA* A0 (:rng s) (:rng t))
                   (subtype-list? (repeat t-prest-types-count s-rest) t-prest-types))
        (report-not-subtypes s t)
        (if (> s-dom-count t-dom-count)
          ; hard mode
          (let [[s-dom-short s-dom-rest] (split-at t-dom-count s-dom)]
            (if-not (subtype-list? t-dom s-dom-short)
              (report-not-subtypes s t)
              (let [remain-repeat-count (rem (count s-dom-rest) t-prest-types-count)
                    ceiling (fn [up low]
                              {:pre [(every? integer? [up low])]}
                              (let [result (quot up low)]
                                (if (zero? (rem up low))
                                  result
                                  (inc result))))
                    repeat-times (if (empty? s-dom-rest)
                                   0
                                   (ceiling (count s-dom-rest) t-prest-types-count))
                    _ (subtype-list? (concat s-dom-rest (repeat remain-repeat-count s-rest))
                                     (gen-repeat repeat-times t-prest-types))]
                A0)))
          ; easy mode
          (let [[t-dom-short t-dom-rest] (split-at s-dom-count t-dom)]
            (if (and (subtype-list? t-dom-short s-dom)
                     (subtype-list? t-dom-rest (repeat (count t-dom-rest) s-rest)))
              A0
              (report-not-subtypes s t))))))

    ;kw args
    (and (:kws s)
         (:kws t))
    (if (and (every?' (partial subtypeA* A0) (:dom t) (:dom s))
             (subtypeA* A0 (:rng s) (:rng t))
             (subtype-kwargs* A0 (:kws t) (:kws s)))
      A0
      (report-not-subtypes s t))

    (and (:rest s)
         (= :fixed (:kind t)))
    (if (some-> A0
                (subtypes*-varargs (:dom t) (:dom s) (:rest s) nil)
                (subtypeA* (:rng s) (:rng t)))
      A0
      (report-not-subtypes s t))

    (and (= :fixed (:kind t))
         (:rest t))
    (report-not-subtypes s t)

    (and (:rest s)
         (:rest t))
    (if (some-> A0
                (subtypes*-varargs (:dom t) (:dom s) (:rest s) nil)
                (subtypeA* (:rest t) (:rest s))
                (subtypeA* (:rng s) (:rng t)))
      A0
      (report-not-subtypes s t))

    ;; TODO unit test
    ;; handle ... varargs when the bounds are the same
    (and (:drest s)
         (:drest t)
         (= (-> s :drest :name)
            (-> t :drest :name)))
    (if (and (subtypeA* A0 (-> t :drest :pre-type) (-> s :drest :pre-type))
             (some-> (reduce (fn [A* [s t]]
                               (cond-> (subtypeA* A* s t)
                                 nil? reduced))
                             A0 (map vector (:dom t) (:dom s)))
                     (subtypeA* (:rng s) (:rng t))))
      A0
      (report-not-subtypes s t))
    :else (report-not-subtypes s t)))

;[(IPersistentSet '[Type Type]) Function (t/Seqable Function) -> (Option (IPersistentSet '[Type Type]))]
(defn supertype-of-one-arr [A s ts]
  (some #(arr-subtype A % s) ts))

(defn fully-resolve-filter [fl]
  {:pre [(fr/Filter? fl)]
   :post [(fr/Filter? %)]}
  (cond
    (fr/TypeFilter? fl) (update fl :type c/fully-resolve-type)
    (fr/NotTypeFilter? fl) (update fl :type c/fully-resolve-type)
    (fr/AndFilter? fl) (update fl :fs #(into #{} (map fully-resolve-filter) %))
    (fr/OrFilter? fl) (update fl :fs #(into #{} (map fully-resolve-filter) %))
    (fr/ImpFilter? fl) (-> fl
                           (update :a fully-resolve-filter)
                           (update :c fully-resolve-filter))
    :else fl))

(defn simplify-type-filter [f]
  {:pre [(fr/TypeFilter? f)]}
  (let [[fpth & rstpth] (:path f)]
    (cond 
      (empty? (:path f)) 
      f

      (pth-rep/KeyPE? fpth)
      (simplify-type-filter
        (fops/-filter 
          (c/make-HMap :mandatory {(r/-val (:val fpth)) (:type f)})
          (:id f)
          rstpth))
      :else f)))

(defn ^:private subtype-type-filter* [A s t]
  {:pre [(fr/TypeFilter? s)
         (fr/TypeFilter? t)]}
  (let [s (simplify-type-filter s)
        t (simplify-type-filter t)]
    (if (fr/equal-paths? s t)
      (subtypeA* A (:type s) (:type t))
      (report-not-subtypes s t))))

(defn simplify-not-type-filter [f]
  {:pre [(fr/NotTypeFilter? f)]}
  (let [[fpth & rstpth] (:path f)]
    (cond 
      (empty? (:path f)) 
      f

      (pth-rep/KeyPE? fpth)
      (simplify-not-type-filter
        (fops/-not-filter 
          ; keys is optional
          (c/make-HMap 
            :optional {(r/-val (:val fpth)) (:type f)})
          (:id f)
          rstpth))
      :else f)))

(defn ^:private subtype-not-type-filter* [A s t]
  {:pre [(fr/NotTypeFilter? s)
         (fr/NotTypeFilter? t)]
   :post [(or (nil? %) (set? %))]}
  (let [s (simplify-not-type-filter s)
        t (simplify-not-type-filter t)]
    (and (fr/equal-paths? s t)
         (subtypeA* A (:type t) (:type s)))))

(defn ^:private subtype-filter-set* [A f1 f2]
  {:pre [(fr/FilterSet? f1)
         (fr/FilterSet? f2)]
   :post [(or (nil? %) (set? %))]}
  ;(prn `subtype-filter-set* f1 f2)
  (if (= f2 (fops/-simple-filter))
    A
    (letfn [(sub-helper [f1 f2 pred field subf]
              (let [fld1 (field f1)
                    fld2 (field f2)]
                (when (and (pred fld1)
                           (pred fld2))
                  (subf A fld1 fld2))))]
      (or
        (and (sub-helper f1 f2 fr/TypeFilter? :then subtype-type-filter*)
             (sub-helper f1 f2 fr/TypeFilter? :else subtype-not-type-filter*))
        (and (sub-helper f1 f2 fr/TypeFilter? :then subtype-type-filter*)
             (sub-helper f1 f2 fr/NotTypeFilter? :else subtype-not-type-filter*))
        (and (sub-helper f1 f2 fr/NotTypeFilter? :then subtype-not-type-filter*)
             (sub-helper f1 f2 fr/NotTypeFilter? :else subtype-not-type-filter*))
        (and (sub-helper f1 f2 fr/NotTypeFilter? :then subtype-not-type-filter*)
             (sub-helper f1 f2 fr/TypeFilter? :else subtype-type-filter*))))))

(defn ^:private subtype-filter* [A f1 f2]
  {:pre [(fr/Filter? f1)
         (not (fr/NoFilter? f1))
         (fr/Filter? f2)
         (not (fr/NoFilter? f2))]
   :post [(or (nil? %) (set? %))]}
  ; assume conjunctive normal form
  ; ie. (V (^ ...) (^ ...))
  (cond
    (= f1 f2) A
    (fr/TopFilter? f2) A
    (fr/BotFilter? f1) A

    (and (fr/TypeFilter? f1)
         (fr/TypeFilter? f2))
    (subtype-type-filter* A f1 f2)

    (and (fr/NotTypeFilter? f1)
         (fr/NotTypeFilter? f2))
    (subtype-not-type-filter* A f1 f2)

    (fr/AndFilter? f2) (if (every? (fn [f2*]
                                     (subtype-filter* A f1 f2*))
                                   (:fs f2))
                         A
                         (report-not-subtypes f1 f2))
    (fr/AndFilter? f1) (some (fn [f1*]
                               (subtype-filter* A f1* f2))
                             (:fs f1))
    (fr/OrFilter? f1) (if (every? (fn [f1*]
                                    (subtype-filter* A f1* f2))
                                  (:fs f1))
                        A
                        (report-not-subtypes f1 f2))
    (fr/OrFilter? f2) (some (fn [f2*]
                              (subtype-filter* A f1 f2*))
                            (:fs f2))
    :else (report-not-subtypes f1 f2)))


(defn subtype-Result
  [A
   {t1 :t f1 :fl o1 :o :as s}
   {t2 :t f2 :fl o2 :o :as t}]
  (cond
    ;trivial case
    (and (= o1 o2)
         (subtype-filter-set* A f1 f2))
    (subtypeA* A t1 t2)

    ;we can ignore some interesting results
    (and (orep/EmptyObject? o2)
         (= f2 (fops/-FS fr/-top fr/-top)))
    (subtypeA* A t1 t2)

    (and (orep/EmptyObject? o2)
         (= f1 f2))
    (subtypeA* A t1 t2)

    ;special case for (& (is y sym) ...) <: (is y sym)
    (and (fr/AndFilter? (:then f1))
         (fr/TypeFilter? (:then f2))
         (every? fops/atomic-filter? (:fs (:then f1)))
         (= 1 (count (filter fr/TypeFilter? (:fs (:then f1)))))
         (= fr/-top (:else f2))
         (= o1 o2))
    (let [f1-tf (first (filter fr/TypeFilter? (:fs (:then f1))))]
      (if (= f1-tf (:then f2))
        (subtypeA* A t1 t2)
        (report-not-subtypes t1 t2)))

    :else (report-not-subtypes t1 t2)))

(defn ^:private subtype-TypeFn-rands
  [A tfn rands1 rands2]
  {:pre [(r/TypeFn? tfn)
         (every? r/Type? rands1)
         (every? r/Type? rands2)]
   :post [(or (nil? %) (set? %))]}
  (if (and (== (count rands1)
               (count rands2)
               (count (:variances tfn)))
           (every?' (fn [v l r]
                      {:pre [(r/variance? v)
                             (r/Type? l)
                             (r/Type? r)]}
                      (case v
                        (:covariant) (subtypeA* A l r)
                        (:contravariant) (subtypeA* A r l)
                        (:invariant) (and (subtypeA* A l r)
                                          (subtypeA* A r l))
                        (err/int-error (str "Unknown variance: " v))))
                    (:variances tfn) rands1 rands2))
    A
    (report-not-subtypes rands1 rands2)))

(defn subtype-TApp
  [A s t]
  (let [s (update s :rator c/fully-resolve-type)
        t (update t :rator c/fully-resolve-type)
        _ (assert (= (:rator s) (:rator t)))]
    (cond
      (and (r/F? (:rator s))
           (r/F? (:rator t)))
      (let [{:keys [upper-bound] :as bnd} (free-ops/free-with-name-bnds (-> s :rator :name))]
        (cond 
          (not bnd) (err/int-error (str "No bounds for " (:name s)))
          :else (let [upper-bound (c/fully-resolve-type upper-bound)]
                  (if (r/TypeFn? upper-bound)
                    (subtype-TypeFn-rands A upper-bound (:rands s) (:rands t))
                    (report-not-subtypes s t)))))
      (r/TypeFn? (:rator s))
      (let [rator (:rator s)
            variances (:variances rator)
            names (repeatedly (:nbound rator) gensym)
            bbnds (c/TypeFn-bbnds* names rator)]
        (if (and (= (count variances)
                    (count (:rands s))
                    (count (:rands t)))
                 (every?' (fn [variance {:keys [lower-bound upper-bound]} s t]
                            (and (subtypeA* A lower-bound s)
                                 (subtypeA* A lower-bound t)
                                 (subtypeA* A s upper-bound)
                                 (subtypeA* A t upper-bound)
                                 (case variance
                                   :covariant (subtypeA* A s t)
                                   :contravariant (subtypeA* A t s)
                                   :invariant (and (subtypeA* A s t)
                                                   (subtypeA* A t s)))))
                          variances bbnds (:rands s) (:rands t)))
          A
          (report-not-subtypes s t)))
      :else (report-not-subtypes s t))))

(defn subtype-TypeFn
  [A S T]
  {:pre [(r/TypeFn? S)
         (r/TypeFn? T)]}
  (let [;instantiate both type functions with the same names
        names (repeatedly (:nbound S) gensym)
        sbnds (c/TypeFn-bbnds* names S)
        tbnds (c/TypeFn-bbnds* names T)
        sbody (c/TypeFn-body* names S)
        tbody (c/TypeFn-body* names T)]
    (if (and (= (:nbound S) (:nbound T))
             (= (:variances S) (:variances T))
             (every?' (fn [lbnd rbnd]
                        (and (subtypeA* A (:upper-bound lbnd) (:upper-bound rbnd))
                             (subtypeA* A (:lower-bound rbnd) (:lower-bound lbnd))
                             (subtypeA* A (:lower-bound lbnd) (:upper-bound lbnd))
                             (subtypeA* A (:lower-bound rbnd) (:upper-bound rbnd))))
                      sbnds tbnds))
      (subtypeA* A sbody tbody)
      (report-not-subtypes S T))))

(defn subtype-PrimitiveArray
  [A s t]
  (if (and ;(= (.jtype s) (.jtype t))
           ;contravariant
           (subtypeA* A
                      (:input-type t)
                      (:input-type s))
           ;covariant
           (subtypeA* A
                      (:output-type s)
                      (:output-type t)))
    A
    (report-not-subtypes s t)))

(defn datatype-ancestors 
  "Returns a set of Types which are ancestors of this datatype.
  Only useful when checking Clojure. This is because we need to query datatypes
  for their ancestors, as sometimes datatypes do not appear in `extenders`
  of a protocol (this happens when a protocol is extend directly in a deftype)."
  [{:keys [the-class] :as dt}]
  {:pre [(r/DataType? dt)]}
  (impl/assert-clojure)
  (let [overidden-by (fn [sym o]
                       ;(prn "overriden by" sym (class o) o)
                       (cond
                         ((some-fn r/DataType? r/RClass?) o)
                         (when (= sym (:the-class o))
                           o)
                         (r/Protocol? o)
                         ; protocols are extended via their interface if they
                         ; show up in the ancestors of the datatype
                         (when (= sym (:on-class o))
                           o)))
        overrides (map c/fully-resolve-type (ancest/get-datatype-ancestors dt))
        ;_ (prn "datatype name" the-class)
        ;_ (prn "datatype overrides" overrides)
        _ (assert (every? (some-fn r/Protocol? r/DataType? r/RClass?) overrides)
                  "Overriding datatypes to things other than datatypes, protocols and classes NYI")
        ; the classes that this datatype extends.
        ; No vars should occur here because protocol are extend via
        ; their interface.
        normal-asyms (->> (ancestors (coerce/symbol->Class the-class))
                          (filter class?)
                          (map coerce/Class->symbol))
        ;_ (prn "normal-asyms" normal-asyms)
        post-override (set
                        (for [sym normal-asyms]
                          ; either we override this ancestor ...
                          (if-let [o (some #(overidden-by sym %) overrides)]
                            o
                            (let [protocol-varsym (c/Protocol-interface->on-var sym)]
                              (if (resolve protocol-varsym)
                                ;... or we make a protocol type from the varified interface ...
                                (c/Protocol-with-unknown-params protocol-varsym)
                                ;... or we make an RClass from the actual ancestor.
                                (c/RClass-of-with-unknown-params sym))))))]
    post-override))

(defn ^:private subtype-rclass-or-datatype-with-protocol
  [A s t]
  {:pre [((some-fn r/RClass? r/DataType?) s)
         (r/Protocol? t)]
   :post [(or (set? %) (nil? %))]}
  (let [s-kind (cond
                 (r/RClass? s) (do (impl/assert-clojure (str "subtype-rclass-or-datatype-with-protocol not yet implemented for implementations other than Clojure: "
                                                             (prs/unparse-type s) " " (prs/unparse-type t)))
                                   :RClass)
                 (r/DataType? s) :DataType
                 :else (err/int-error (str "what is this? " s)))
        ;first try and find the datatype in the protocol's extenders
        in-protocol-extenders? (boolean
                                 (seq
                                   (sequence
                                     (comp (filter class?)
                                           (map coerce/Class->symbol)
                                           (filter #{(:the-class s)}))
                                     (c/Protocol-normal-extenders t))))
        relevant-ancestor (some (fn [p] 
                                  (let [p (c/fully-resolve-type p)]
                                    (when (and (r/Protocol? p)
                                               (= (:the-var p) (:the-var t)))
                                      p)))
                                ((case s-kind
                                   :RClass c/RClass-supers*
                                   :DataType datatype-ancestors)
                                 s))]
    (cond 
      ; the extension is via the protocol
      (or in-protocol-extenders?
          ; extension via the protocol's interface, or explicitly overriden
          relevant-ancestor)
      (let [relevant-protocol-extender (or relevant-ancestor
                                           ((case s-kind
                                              :RClass c/RClass-of-with-unknown-params
                                              :DataType c/DataType-with-unknown-params)
                                            (:the-class s)))]
        (subtypeA* A s relevant-protocol-extender))
      :else (report-not-subtypes s t))))

;(t/ann subtype-datatype-rclass [DataType RClass -> Seen])
(defn ^:private subtype-datatype-rclass
  [A s t]
  {:pre [(r/DataType? s)
         (r/RClass? t)]}
  (impl/assert-clojure)
  (if-some [relevant-datatype-ancestor (some (fn [p]
                                               (let [p (c/fully-resolve-type p)]
                                                 (when (and (r/RClass? p)
                                                            (= (:the-class p) (:the-class t)))
                                                   p)))
                                             (datatype-ancestors s))]
    (subtypeA* A s relevant-datatype-ancestor)
    (report-not-subtypes s t)))

(defn- subtype-datatypes-or-records
  [A
   {cls1 :the-class poly1 :poly? :as s} 
   {cls2 :the-class poly2 :poly? :as t}]
  {:pre [(every? r/DataType? [s t])]}
  (if (and (= cls1 cls2)
           (every?' (fn [v l r]
                      (case v
                        :covariant (subtypeA* A l r)
                        :contravariant (subtypeA* A r l)
                        :invariant (and (subtypeA* A l r)
                                        (subtypeA* A r l))))
                    (:variances s) poly1 poly2))
    A
    (report-not-subtypes s t)))

; does this really help?
(defn class-isa?
  "A faster version of isa?, both parameters must be classes"
  [s ^Class t]
  (.isAssignableFrom t s))

; (Cons Integer) <: (Seqable Integer)
; (ancestors (Seqable Integer)

(defn- subtype-RClass-common-base 
  [A
   {polyl? :poly? lcls-sym :the-class :as s}
   {polyr? :poly? rcls-sym :the-class :as t}]
  (impl/assert-clojure)
  (let [{variances :variances} s]
    (and (= lcls-sym rcls-sym)
         (or (and (empty? polyl?) (empty? polyr?))
             (and (seq polyl?)
                  (seq polyr?)
                  (every?' #(case %1
                              :covariant (subtypeA* A %2 %3)
                              :contravariant (subtypeA* A %3 %2)
                              (and (subtypeA* A %2 %3)
                                   (subtypeA* A %3 %2)))
                           variances
                           polyl?
                           polyr?))))))

;(IPersistentMap Class Class)
(def boxed-primitives
  {Byte/TYPE Byte
   Short/TYPE Short
   Integer/TYPE Integer
   Long/TYPE Long
   Float/TYPE Float
   Double/TYPE Double
   Character/TYPE Character
   Boolean/TYPE Boolean})

;[RClass RClass -> Boolean]
(defn coerce-RClass-primitive
  [s t]
  (impl/assert-clojure)
  (cond
    ; (U Integer Long) <: (U int long)
    (and 
      (#{(c/RClass-of Integer) (c/RClass-of Long)} s)
      (#{(c/RClass-of 'int) (c/RClass-of 'long)} t))
    true

    :else
    (let [spcls (coerce/symbol->Class (:the-class s))
          tpcls (coerce/symbol->Class (:the-class t))
          scls (or (boxed-primitives spcls)
                   spcls)
          tcls (or (boxed-primitives tpcls)
                   tpcls)]
      (class-isa? scls tcls))))

(defn subtype-RClass
  [A
   {polyl? :poly? :as s}
   {polyr? :poly? :as t}]
  (impl/assert-clojure)
  (let [scls (r/RClass->Class s)
        tcls (r/RClass->Class t)]
    ;(prn "subtype RClass" (prs/unparse-type s) (prs/unparse-type t))
    (cond
      (or
        ; use java subclassing
        (and (empty? polyl?)
             (empty? polyr?)
             (class-isa? scls tcls))

        ;same base class
        (and (= scls tcls)
             (subtype-RClass-common-base A s t))

        ;one is a primitive, coerce
        (and (or (.isPrimitive scls)
                 (.isPrimitive tcls))
             (coerce-RClass-primitive s t))

        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(when (r/RClass? %)
                 (and (= (:the-class t) (:the-class %))
                      (subtype-RClass-common-base A % t)))
              (map c/fully-resolve-type (c/RClass-supers* s))))
      A

      ;try each ancestor

      :else (report-not-subtypes s t))))

;subtype if t includes all of s. 
;tl <= sl, su <= tu
(defn subtype-CountRange
  [A
   {supper :upper slower :lower :as s}
   {tupper :upper tlower :lower :as t}]
  (if (and (<= tlower slower)
           (or (not tupper)
               (and supper (<= supper tupper))))
    A
    (report-not-subtypes s t)))

(defmacro sub-clj? [s t]
  `(impl/with-clojure-impl
     (subtype? (prs/parse-type '~s)
               (prs/parse-type '~t))))

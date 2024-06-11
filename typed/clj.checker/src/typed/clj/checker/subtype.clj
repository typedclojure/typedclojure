;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.subtype
  "Use [[subtype?]] to check if s <: t, and [[subtype-filter?]] for filters."
  (:refer-clojure :exclude [requiring-resolve repeatedly])
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [typed.cljc.runtime.perf-utils :refer [repeatedly]]
            [clojure.set :as set]
            [clojure.core.typed.util-vars :as vs]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljc.checker.check :as check]
            [typed.clj.checker.assoc-utils :as assoc-u]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.filter-ops :as fops]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.path-rep :as pth-rep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  (:import (typed.cljc.checker.type_rep Poly TApp F FnIntersection Intersection
                                        Extends NotType DifferenceType AssocType
                                        RClass Bounds HSequential HeterogeneousMap
                                        Protocol JSObj)))

(set! *warn-on-reflection* true)

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
  [argtys dom rst kws opts]
  (boolean
    (do-top-level-subtype-using
      subtypes*-varargs argtys dom rst kws opts)))

(defn subtypes-prest?
  "True if argtys are under dom and prest"
  [argtys dom prest opts]
  (let [dom-count (count dom)
        [argtys-dom argtys-rest] (split-at dom-count argtys)]
    (boolean
      (and (do-top-level-subtype-using
             subtypes*-varargs (vec argtys-dom) dom nil nil opts)
           (do-top-level-subtype-using
             subtypeA* (r/-hvec (vec argtys-rest)) prest opts)))))

;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument (the current-seen subtypes), called by subtype
;
; In short, only call subtype (or subtype?)

(declare subtype-filter*)

(def subtype-cache (atom {}))

; (ann reset-subtype-cache [-> nil])
(defn reset-subtype-cache []
  (reset! subtype-cache {})
  nil)

;[Type Type -> Boolean]
(defn subtype? [s t opts]
  {:post [(boolean? %)]}
  (letfn [(do-subtype []
            (boolean
              (do-top-level-subtype-using subtypeA* s t opts)))]
    (if true ;; true == disable cache
      (do-subtype)
      (if-let [[_ res] (find @subtype-cache [s t])]
        res
        (let [res (do-subtype)]
          (when-not (currently-subtyping?)
            (swap! subtype-cache assoc [s t] res))
          res)))))

(defn subtype-filter? [f1 f2 opts]
  (boolean
    (do-top-level-subtype-using
      subtype-filter* f1 f2 opts)))

(declare subtype-type-filter* subtype-not-type-filter*)

(defn subtype-type-filter? [f1 f2 opts]
  (boolean
    (do-top-level-subtype-using
      subtype-type-filter* f1 f2 opts)))

(defn subtype-not-type-filter? [f1 f2 opts]
  (boolean
    (do-top-level-subtype-using
      subtype-not-type-filter* f1 f2 opts)))

;[(Map Symbol Bounds) (Map Symbol Bounds) (t/Seqable Type) (t/Seqable Type)
;  -> Boolean]
(defn unify [X Y S T R opts]
  (boolean 
    (u/handle-cs-gen-failure
      (ind/infer X Y S T R opts))))

(declare protocol-extenders
         subtype-datatypes-or-records subtype-Result subtype-PrimitiveArray
         subtype-CountRange subtype-TypeFn subtype-RClass subtype-rclass-or-datatype-with-protocol
         boxed-primitives subtype-datatype-rclass subtype-TApp
         subtype-Satisfies)

(defn subtype-HSet [A s t]
  {:pre [(r/HSet? s)
         (r/HSet? t)]}
  (if (and (= (:fixed s) (:fixed t))
           (:complete? s)
           (:complete? t))
    A
    (report-not-subtypes s t)))

(defn subtype-compatible-HSequential [A s t opts]
  {:pre [(r/HSequential? s)
         (r/HSequential? t)
         (r/compatible-HSequential-kind? (:kind s) (:kind t))]}
  (let [subtypeA* #(subtypeA* A %1 %2 opts)]
  (if (and (cond
             ; simple case, no rest, drest, repeat types
             (and (not-any? :rest [s t])
                  (not-any? :drest [s t])
                  (not-any? :repeat [s t]))
             (let []
               (and (= (count (:types s))
                       (count (:types t)))
                    (every?' subtypeA* (:types s) (:types t))))

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
                      (every?' subtypeA*
                               (repeat (first s-types))
                               t-types)
                      (subtypeA* (first s-types) (:rest t)))

                 ; both s & t have :repeat
                 (:repeat t)
                 (and (<= t-types-count
                          s-types-count)
                      (zero? (rem s-types-count
                                  t-types-count))
                      (every?' subtypeA*
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
                      (every?' subtypeA*
                               s-types
                               (repeat (first t-types)))
                      (subtypeA* (:rest s) (first t-types)))

                 ; nothing on left
                 (and (or (zero? s-types-count)
                          (<= t-types-count
                              s-types-count))
                      (if (zero? (rem s-types-count
                                      t-types-count))
                        (every?' subtypeA*
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
                      (subtypeA* (:rest s) (:rest t)))
                  ;pad t to the right
                  (every?' subtypeA*
                           (:types s)
                           (concat (:types t)
                                   (repeat (- (count (:types s)) (count (:types t)))
                                           (:rest t)))))

             (and (:drest s)
                  (:rest t))
             (and
               (every?' subtypeA*
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
    (report-not-subtypes s t))))

(defn simplify-In [t opts]
  {:pre [(r/Intersection? t)]}
  (:types t)
  ;; very slow
  #_
  (let [mi (c/In (:types t) opts)]
    (if (r/Intersection? mi)
      (:types mi)
      [mi])))

(defn subtype-symbolic-closure
  "Return A if symbolic closure s is a subtype of t, otherwise nil."
  [A s t {::check/keys [check-expr] :as opts}]
  {:pre [(set? A)
         (r/SymbolicClosure? s)
         (r/AnyType? t)]
   :post [((some-fn nil? set?) %)]}
  ;(prn :subtype-symbolic-closure s t)
  (with-bindings (assoc (:bindings s)
                        #'vs/*delayed-errors* (err/-init-delayed-errors))
    (when (try (binding [*sub-current-seen* A]
                 (check-expr (:fexpr s) (r/ret t)))
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
  [{{::check/keys [check-expr]} :opts :as s} t]
  {:pre [(r/SymbolicClosure? s)
         (r/AnyType? t)]
   :post [(-> % u/expr-type r/TCResult?)]}
  ;(prn :check-symbolic-closure s t)
  (with-bindings (dissoc (:bindings s)
                         ;; hmm, additional error msg context needed to orient the user
                         ;; to the problem? symbolic closure will be blamed
                         #'vs/*delayed-errors*)
    (check-expr (:fexpr s) (r/ret t))))

(defn subtype-regex [A s t]
  {:pre [(r/Regex? s)
         (r/Regex? t)]}
  ;;TODO
  (report-not-subtypes s t))


(defmacro ^:private AND
  "Like `clojure.core/and` but produces better bytecode when used with
  compile-time known booleans."
  ([] true)
  ([x] `(if ~x true false))
  ([x & next]
   `(let [and# ~x]
      (if ~x (AND ~@next) false))))

(defmacro ^:private OR
  "Like `clojure.core/or` but produces better bytecode when used with
  compile-time known booleans."
  ([] nil)
  ([x] `(if ~x true false))
  ([x & next]
   `(if ~x true (OR ~@next))))

(defn subtype-heterogeneous-map [A s t opts]
  (let [; convention: prefix things on left with l, right with r
        ltypes (:types s)
        labsent (:absent-keys s)
        rtypes (:types t)
        rabsent (:absent-keys t)]
    (if (and                           ; if t is complete, s must be complete ..
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
                     (subtypeA* A t v opts)))
                 rtypes)
                                        ; all optional keys in t should match optional/mandatory entries in s
         (every? (fn [[k v]]
                   (let [matches-entry?
                         (if-let [actual-v 
                                  ((merge-with #(c/In [%1 %2] opts)
                                               (:types s)
                                               (:optional s))
                                   k)]
                           (subtypeA* A actual-v v opts)
                           (c/complete-hmap? s))]
                     (cond
                       (c/partial-hmap? s)
                       (or (contains? (:absent-keys s) k)
                           matches-entry?)
                       :else matches-entry?)))
                 (:optional t)))
      A
      (report-not-subtypes s t))))

(defprotocol SubtypeA*Protocol
  (subtypeA*-for-s [s t A opts]))

(extend-protocol SubtypeA*Protocol
  typed.cljc.checker.type_rep.Regex
  (subtypeA*-for-s [s t A opts]
    (when (r/Regex? t)
      (subtype-regex A s t)))

  typed.cljc.checker.type_rep.CountRange
  (subtypeA*-for-s [s t A opts]
    (when (r/CountRange? t)
      (subtype-CountRange A s t)))

  typed.cljc.checker.type_rep.CLJSInteger
  (subtypeA*-for-s [s t A opts]
    (when (r/JSNumber? t)
      A))

  FnIntersection
  ;; hack for FnIntersection <: clojure.lang.IFn
  (subtypeA*-for-s [s t A opts]
    (when (impl/checking-clojure? opts)
      (subtypeA* A (c/RClass-of clojure.lang.IFn opts) t opts)))

  RClass
  (subtypeA*-for-s [s t A opts]
    (cond
      (OR (r/RClass? t)
          (r/Instance? t))
      (subtype-RClass A s t opts)

      (OR (r/Protocol? t)
          (r/Satisfies? t))
      (subtype-rclass-or-datatype-with-protocol A s t opts)

      ;; handles classes with FnIntersection ancestors
      (r/FnIntersection? t)
      (if (= 'clojure.lang.Var (.the-class s))
        ;; Var doesn't actually have an FnIntersection ancestor,
        ;; but this case simulates it.
        (let [args (:poly? s)
              read-type (first args)
              _ (when-not (= 1 (count args))
                  (err/int-error
                   (str "clojure.lang.Var takes 1 argument, "
                        "given " (count (.poly? s)))
                   opts))]
          (subtypeA* A read-type t opts))

        (some #(let [s (c/fully-resolve-type % opts)]
                 (when (r/FnIntersection? s)
                   (subtypeA* A s t opts)))
              (c/RClass-supers* s opts)))

      ;; handles classes with heterogeneous vector ancestors (eg. IMapEntry)
      (OR (r/HSequential? t)
          (r/TopHSequential? t))
      (some #(let [s (c/fully-resolve-type % opts)]
               (when (r/HSequential? s)
                 (subtypeA* A s t opts)))
            (c/RClass-supers* s opts))))

  typed.cljc.checker.type_rep.Instance
  (subtypeA*-for-s [s t A opts]
    (when (OR (r/RClass? t)
              (r/Instance? t))
      (subtype-RClass A s t opts)))

  typed.cljc.checker.type_rep.DataType
  (subtypeA*-for-s [s t A opts]
    (cond (r/DataType? t)
          (subtype-datatypes-or-records A s t opts)

          (r/RClass? t)
          (subtype-datatype-rclass A s t opts)

          (OR (r/Protocol? t)
              (r/Satisfies? t))
          (subtype-rclass-or-datatype-with-protocol A s t opts)))

  typed.cljc.checker.type_rep.Result
  (subtypeA*-for-s [s t A opts]
    (when (r/Result? t)
      (subtype-Result A s t opts)))

  typed.cljc.checker.type_rep.PrimitiveArray
  (subtypeA*-for-s [s t A opts]
    (if (r/PrimitiveArray? t)
      (subtype-PrimitiveArray A s t opts)
      (subtypeA* A (c/upcast-PrimitiveArray s opts) t opts)))

  typed.cljc.checker.type_rep.TypeFn
  (subtypeA*-for-s [s t A opts]
    (when (r/TypeFn? t)
      (subtype-TypeFn A s t opts)))

  typed.cljc.checker.type_rep.JSNull
  (subtypeA*-for-s [s t A opts] (subtypeA* A r/-nil t opts))

  typed.cljc.checker.type_rep.JSUndefined
  (subtypeA*-for-s [s t A opts] (subtypeA* A r/-nil t opts))

  ;;values are subtypes of their classes
  typed.cljc.checker.type_rep.Value
  (subtypeA*-for-s [s t A opts]
    (cond
      ;; repeat Heterogeneous* can always accept nil
      (AND (= s r/-nil)
           (r/HSequential? t)
           (:repeat t))
      A

      (AND (= s r/-nil)
           (r/Protocol? t)
           (impl/checking-clojure? opts)
           (contains? (c/Protocol-normal-extenders t opts) nil))
      A

      :else
      (let [sval (.val s)]
        (impl/impl-case opts
         :clojure (if (nil? sval)
                    ; this is after the nil <: Protocol case, so just add non-protocol ancestors here
                    (subtypeA* A (r/make-ExactCountRange 0) t opts)
                    (subtypeA* A
                               (c/In (cons (c/RClass-of (class sval) opts)
                                           (cond
                                             ;keyword values are functions
                                             (keyword? sval) [(c/keyword->Fn sval opts)]
                                             ;strings have a known length as a seqable
                                             (string? sval) [(r/make-ExactCountRange (count sval))]))
                                     opts)
                               t
                               opts))
         :cljs (cond
                 (integer? sval) (subtypeA* A (r/CLJSInteger-maker) t opts)
                 (number? sval) (subtypeA* A (r/JSNumber-maker) t opts)
                 (string? sval) (subtypeA* A (r/JSString-maker) t opts)
                 (boolean? sval) (subtypeA* A (r/JSBoolean-maker) t opts)
                 (symbol? sval) (subtypeA* A (c/DataType-of 'cljs.core/Symbol opts) t opts)
                 (keyword? sval) (subtypeA* A (c/DataType-of 'cljs.core/Keyword opts) t opts)
                 :else (report-not-subtypes s t))))))

  ;; The order of checking protocols and datatypes is subtle.
  ;; It is easier to calculate the ancestors of a datatype than
  ;; the descendants of a protocol, so Datatype <: Any comes
  ;; before Protocol <: Any.
  Protocol
  (subtypeA*-for-s [s ^Protocol t A opts]
    (cond
      (r/Protocol? t)
      (let [var1 (.the-var s)
            variances* (.variances s)
            poly1 (.poly? s)
            var2 (.the-var t)
            poly2 (.poly? t)]
        ;(prn "protocols subtype" s t)
        (when (AND (= var1 var2)
                   (every?' (fn _prcol-variance [v l r]
                              (case v
                                :covariant (subtypeA* A l r opts)
                                :contravariant (subtypeA* A r l opts)
                                :invariant (and (subtypeA* A l r opts)
                                                (subtypeA* A r l opts))))
                            variances* poly1 poly2))
          A))

      (r/Satisfies? t)
      (subtype-Satisfies A s t)))

  typed.cljc.checker.type_rep.Satisfies
  (subtypeA*-for-s [s t A opts]
    (when (r/Satisfies? t)
      (subtype-Satisfies A s t)))

  HeterogeneousMap
  (subtypeA*-for-s [s t A opts]
    (subtypeA* A (c/upcast-hmap s opts) t opts))

  ;; JSObj is covariant, taking after TypeScript & Google Closure. Obviously unsound.
  JSObj
  (subtypeA*-for-s [s ^JSObj t A opts]
    (when (r/JSObj? t)
      (let [; convention: prefix things on left with l, right with r
            ltypes (.types s)
            rtypes (.types t)]
        (when (every? (fn [[k rt]]
                        (when-let [lt (get ltypes k)]
                          (subtypeA* A lt rt opts)))
                      rtypes)
          A))))

  typed.cljc.checker.type_rep.HSet
  (subtypeA*-for-s [s t A opts]
    (if (r/HSet? t)
      (subtype-HSet A s t)
      (subtypeA* A (c/upcast-hset s opts) t opts)))

  typed.cljc.checker.type_rep.KwArgsSeq
  (subtypeA*-for-s [s t A opts]
    (if (r/TopKwArgsSeq? t)
      A
      (subtypeA* A (c/upcast-kw-args-seq s opts) t opts)))

  ;; TODO add repeat support
  HSequential
  (subtypeA*-for-s [s ^HSequential t A opts]
    (cond (and (r/HSequential? t)
               (r/compatible-HSequential-kind? (.kind s)
                                               (.kind t)))
          (subtype-compatible-HSequential A s t opts)

          (r/TopHSequential? t) A

          :else
          (subtypeA* A (c/upcast-HSequential s opts) t opts)))

  ;;every rtype entry must be in ltypes
  ;;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
  HeterogeneousMap
  (subtypeA*-for-s [s t A opts]
    (if (r/HeterogeneousMap? t)
      (subtype-heterogeneous-map A s t opts)
      (subtypeA* A (c/upcast-hmap s opts) t opts)))

  Poly
  (subtypeA*-for-s [s ^Poly t A opts]
    (when (AND (r/PolyDots? t) ;; test t first to short-circuit if -Poly? fails
               (= :PolyDots (.kind s))
               (= (.nbound s) (.nbound t)))
      (let [;instantiate both sides with the same fresh variables
            names (repeatedly (.nbound s) gensym)
            bbnds1 (c/PolyDots-bbnds* names s opts)
            bbnds2 (c/PolyDots-bbnds* names t opts)
            b1 (c/PolyDots-body* names s opts)
            b2 (c/PolyDots-body* names t opts)]
        (when (= bbnds1 bbnds2)
          (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds1)
            (subtypeA* A b1 b2 opts)))))))

;;TODO replace hardcoding cases for unfolding Mu? etc. with a single case for unresolved types.
;;[(t/Set '[Type Type]) Type Type -> (t/Nilable (t/Set '[Type Type]))]
(defn ^:private subtypeA* [A s t opts]
  {:pre [(r/AnyType? s)
         (r/AnyType? t)]
   ;; for recur
   ;:post [(or (set? %) (nil? %))]
   }
  ;(prn "subtypeA*" s t)
  (let [subtypeA* #(subtypeA* %1 %2 %3 opts)]
  (if (OR ; FIXME TypeFn's are probably not between Top/Bottom
          (r/Top? t)
          (r/Wildcard? t)
          (= r/empty-union s)
          ;; Unchecked is both bottom and top
          (r/Unchecked? s)
          (r/Unchecked? t)
          ;TCError is top and bottom
          (r/TCError? s)
          (r/TCError? t)
          (= s t)
          (contains? A [s t]))
    A
    (let [A (conj A [s t])]
      (cond
        (OR (r/TCResult? s)
            (r/TCResult? t))
        (assert nil "Cannot give TCResult to subtype")

        ; use bounds to determine subtyping between frees and types
        ; 2 frees of the same name are handled in the (= s t) case.
        (AND (r/F? s)
             (if-some [^Bounds bnd
                       (free-ops/free-with-name-bnds (:name s))]
               (and (subtypeA* A (:upper-bound bnd) t)
                    (subtypeA* A (:lower-bound bnd) t))
               (do #_(err/int-error (str "No bounds for " (:name s)) opts)
                   false)))
        A

        (AND (r/F? t)
             (if-some [^Bounds bnd
                       (free-ops/free-with-name-bnds (:name t))]
               (and (subtypeA* A s (:upper-bound bnd))
                    (subtypeA* A s (:lower-bound bnd)))
               (do #_(err/int-error (str "No bounds for " (:name s)) opts)
                   false)))
        A

        (r/TypeOf? s)
        (recur A (c/resolve-TypeOf s opts) t opts)

        (r/TypeOf? t)
        (recur A s (c/resolve-TypeOf t opts) opts)

        (AND (r/MatchType? s)
             (c/Match-can-resolve? s))
        (recur A (c/resolve-Match s opts) t opts)

        (AND (r/MatchType? t)
             (c/Match-can-resolve? t))
        (recur A s (c/resolve-Match t opts) opts)

        (AND (r/Value? s)
             (r/Value? t))
        ;already (not= s t)
        (report-not-subtypes s t)

        ;; handle before unwrapping polymorphic types
        (AND (r/SymbolicClosure? s)
             (let [frt (c/fully-resolve-type t opts)]
               (OR (r/FnIntersection? frt)
                   (r/Poly? frt)
                   (r/PolyDots? frt))))
        (or (subtype-symbolic-closure A s t opts)
            (report-not-subtypes s t))

        (AND (r/Poly? s)
             (r/Poly? t)
             (= (:nbound s) (:nbound t))
             (= (:bbnds s) (:bbnds t)))
        (let [;instantiate both sides with the same fresh variables
              names (repeatedly (:nbound s) gensym)
              bbnds1 (c/Poly-bbnds* names s opts)
              b1 (c/Poly-body* names s opts)
              b2 (c/Poly-body* names t opts)]
          (if (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds1)
                (subtypeA* A b1 b2))
            A
            (report-not-subtypes s t)))

        ;use unification to see if we can use the Poly type here
        (and (r/Poly? s)
             (let [names (c/Poly-fresh-symbols* s)
                   bnds (c/Poly-bbnds* names s opts)
                   b1 (c/Poly-body* names s opts)
                   ;_ (prn "try unify on left")
                   X (zipmap names bnds)
                   u (free-ops/with-bounded-frees (update-keys X r/make-F)
                       (unify X {} [b1] [t] r/-any opts))]
               ;(prn "unified on left")
               u))
        A

        (and (r/PolyDots? s)
             (let [names (c/PolyDots-fresh-symbols* s)
                   bnds (c/PolyDots-bbnds* names s opts)
                   b1 (c/PolyDots-body* names s opts)
                   ;_ (prn "try PolyDots unify on left")
                   X (zipmap (pop names) (pop bnds))
                   Y {(peek names) (peek bnds)}
                   u (free-ops/with-bounded-frees (update-keys (into X Y) r/make-F)
                       (unify X Y [b1] [t] r/-any opts))]
               ;(prn "unified on left" u)
               u))
        A

        ;; go after presumably cheaper unification cases
        (and (or (r/Poly? s) (r/PolyDots? s))
             (r/FnIntersection? t)
             (= 1 (count (:types t)))
             (every? #(= :fixed (:kind %)) (:types t))
             (binding [vs/*delayed-errors* (err/-init-delayed-errors)]
               ((requiring-resolve 'typed.cljc.checker.check.funapp/check-funapp)
                nil nil
                (r/ret s)
                (mapv r/ret (-> t :types first :dom))
                (-> t :types first :rng r/Result->TCResult)
                {} opts)
               (empty? @vs/*delayed-errors*)))
        A

        (and (r/Poly? t)
             (empty? (frees/fv-variances t opts))
             (let [names (c/Poly-fresh-symbols* t)
                   bbnds (c/Poly-bbnds* names t opts)
                   b (c/Poly-body* names t opts)]
               (free-ops/with-bounded-frees (zipmap (map r/F-maker names) bbnds)
                 (subtypeA* A s b))))
        A

        (r/Name? s)
        (recur A (c/resolve-Name s opts) t opts)

        (r/Name? t)
        (recur A s (c/resolve-Name t opts) opts)

        ;;only pay cost of dynamic binding when absolutely necessary (eg., before unfolding Mu)
        (r/Mu? s)
        (binding [*sub-current-seen* A]
          (subtypeA* A (c/unfold s opts) t))

        (r/Mu? t)
        (binding [*sub-current-seen* A]
          (subtypeA* A s (c/unfold t opts)))

        (r/App? s)
        (recur A (c/resolve-App s opts) t opts)

        (r/App? t)
        (recur A s (c/resolve-App t opts) opts)

        (= r/empty-union t)
        (report-not-subtypes s t)

        (AND (r/TApp? s)
             (r/TApp? t)
             (= (c/fully-resolve-type (:rator s) opts)
                (c/fully-resolve-type (:rator t) opts)))
        (subtype-TApp A s t opts)

        (and (r/TApp? s)
             (r/TypeFn? (c/fully-resolve-type (:rator s) opts)))
        (let [rands (:rands s)
              rator (c/fully-resolve-type (:rator s) opts)]
          (cond
            (r/F? rator) (report-not-subtypes s t)

            (r/TypeFn? rator)
            (let [names (c/TypeFn-fresh-symbols* rator)
                  bbnds (c/TypeFn-bbnds* names rator opts)
                  res (c/instantiate-typefn rator rands {:names names} opts)]
              (subtypeA* A res t))

            :else (err/int-error (str "First argument to TApp must be TFn, actual: " (prs/unparse-type rator opts)) opts)))

        (and (r/TApp? t)
             (r/TypeFn? (c/fully-resolve-type (:rator t) opts)))
        (let [rands (:rands t)
              rator (c/fully-resolve-type (:rator t) opts)]
          (cond
            (r/F? rator) (report-not-subtypes s t)

            (r/TypeFn? rator)
            (let [names (c/TypeFn-fresh-symbols* rator)
                  res (c/instantiate-typefn rator rands {:names names} opts)]
              (recur A s res opts))

            :else (err/int-error (str "First argument to TApp must be TFn, actual: " (prs/unparse-type rator opts)) opts)))

        (r/Union? s)
        (if (every? (fn union-left [s] (subtypeA* A s t)) (:types s))
          A
          (report-not-subtypes s t))

        (r/Union? t)
        (some (fn union-right [t] (subtypeA* A s t)) (:types t))

        (AND (r/FnIntersection? s)
             (r/FnIntersection? t))
        (loop [A* A
               arr2 (:types t)]
          (let [arr1 (:types s)]
            (if (empty? arr2)
              A*
              (if-let [A (supertype-of-one-arr A* (first arr2) arr1 opts)]
                (recur A (next arr2))
                (report-not-subtypes s t)))))

;does it matter what order the Intersection cases are?
        (r/Intersection? t)
        (let [ts (simplify-In t opts)]
          (if (every? #(subtypeA* A s %) ts)
            A
            (report-not-subtypes s t)))

        (r/Intersection? s)
        (let [ss (simplify-In s opts)]
          (some #(subtypeA* A % t) ss))

        (AND (r/Extends? s)
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

        (AND (r/TopFunction? t)
             (r/FnIntersection? s))
        A

        ;       B <: A
        ;_______________________________
        ; (Not A) <: (Not B)
        (AND (r/NotType? s)
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
               (c/In (cons (:type s) (map r/NotType-maker (:without s))) opts)
               t
               opts)

        (r/DifferenceType? t)
        (recur A
               s
               (c/In (cons (:type t) (map r/NotType-maker (:without t))) opts)
               opts)

        (or (and (r/GetType? s)
                 (c/Get-requires-resolving? s opts))
            (and (r/MergeType? s)
                 (c/Merge-requires-resolving? s opts)))
        (recur A (c/-resolve s opts) t opts)

        (or (and (r/GetType? t)
                 (c/Get-requires-resolving? t opts))
            (and (r/MergeType? t)
                 (c/Merge-requires-resolving? t opts)))
        (recur A s (c/-resolve t opts) opts)

        (AND (r/AssocType? s)
             (r/RClass? t)
             ; (Map xx yy)
             (= 'clojure.lang.IPersistentMap (:the-class t)))
        (let [target (:target s)
              entries (:entries s)
              dentries (:dentries s)
              poly? (:poly? t)
              the-class (:the-class t)
              ; _ (when-not (nil? dentries) (err/nyi-error (pr-str "NYI subtype of dentries AssocType " s) opts))
              ; we assume its all right
              entries-keys (map first entries)
              entries-vals (map second entries)]
          (if (and (subtypeA* A target t)
                   (every?' (partial subtypeA* A) entries-keys (repeat (first poly?)))
                   (every?' (partial subtypeA* A) entries-vals (repeat (second poly?))))
            A
            (report-not-subtypes s t)))

        (AND (r/AssocType? s)
             (r/AssocType? t)
             (r/F? (:target s))
             (r/F? (:target t))
             (not-any? :dentries [s t]))
        (if (= (:target s) (:target t))
          (recur A
                 (assoc-u/assoc-pairs-noret (c/-complete-hmap {} opts) (:entries s) opts)
                 (assoc-u/assoc-pairs-noret (c/-complete-hmap {} opts) (:entries t) opts)
                 opts)
          (report-not-subtypes s t))

        (and (r/AssocType? s)
             (r/F? (:target s))
             (not (r/AssocType? t)))
        (let [bnds (free-ops/free-with-name-bnds (-> s :target :name))
              _ (assert bnds
                        (str "Bounds not found for free variable: " (-> s :target :name)))]
          (if (subtypeA* A (:upper-bound bnds) t)
            (recur A
                   (assoc-u/assoc-pairs-noret (c/-complete-hmap {} opts) (:entries s) opts)
                   t
                   opts)
            (report-not-subtypes s t)))
      
        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? s)
             (not (r/F? (:target s))))
        (if-let [s-or-n (assoc-u/assoc-pairs-noret (:target s) (:entries s) opts)]
          (recur A s-or-n t opts)
          (report-not-subtypes s t))

        ; avoids infinite expansion because associng an F is a fixed point
        (and (r/AssocType? t)
             (not (r/F? (:target t))))
        (if-let [t-or-n (assoc-u/assoc-pairs-noret (:target t) (:entries t) opts)]
          (recur A s t-or-n opts)
          (report-not-subtypes s t))

        :else (or (when (extends? SubtypeA*Protocol (class s))
                    (subtypeA*-for-s s t A opts))
                  ;; TODO (All [r x ...] [x ... x -> r]) <: (All [r x] [x * -> r]) ?
                  (report-not-subtypes s t))
        )))))

(defn ^:private resolve-JS-reference [sym opts]
  (impl/assert-cljs opts)
  (cond
    (= "js" (namespace sym)) (c/JSNominal-with-unknown-params sym opts)
    :else (let [_ (assert nil "FIXME typed.clj.checker.analyze-cljs/analyze-qualified-symbol has been deleted")
                {{:keys [protocol-symbol name]} :info} ((requiring-resolve 'typed.clj.checker.analyze-cljs/analyze-qualified-symbol) sym)]
            (if protocol-symbol
              (c/Protocol-with-unknown-params name opts)
              (c/DataType-with-unknown-params name opts)))))


(defn protocol-extenders [p opts]
  {:pre [(r/Protocol? p)]
   :post [(every? r/Type? %)]}
  (impl/impl-case opts
    :clojure (let [exts (c/Protocol-normal-extenders p opts)]
               (for [ext exts]
                 (cond
                   (class? ext) (c/RClass-of-with-unknown-params ext opts)
                   (nil? ext) r/-nil
                   :else (throw (Exception. (str "What is this?" ext))))))
    :cljs (let [exts ((requiring-resolve 'typed.clj.checker.analyze-cljs/extenders) (:the-var p))]
            (for [ext exts]
              (cond
                (symbol? ext) (resolve-JS-reference ext opts)
                (nil? ext) r/-nil
                :else (throw (Exception. (str "What is this?" ext))))))))

;[(IPersistentSet '[Type Type]) (t/Seqable Type) (t/Seqable Type) (Option Type)
;  -> (IPersistentSet '[Type Type])]
(defn ^:private subtypes*-varargs-info [A0 argtys dom rst kws opts]
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
                  (if-some [A (subtypeA* A fargty rst opts)]
                    (recur dom (subvec argtys 1) A found-kws)
                    (report-not-subtypes fargty rst)))

                (and kws (<= 2 nargtys))
                (let [kw (c/fully-resolve-type (nth argtys 0) opts)
                      val (nth argtys 1)
                      expected-val ((some-fn (:mandatory kws) (:optional kws))
                                    kw)]
                  (if (and expected-val (subtypeA* A val expected-val opts))
                    (recur dom (subvec argtys 2) A (conj found-kws kw))
                    (report-not-subtypes (subvec argtys 0 2) kws)))

                :else (report-not-subtypes argtys dom))

          :else
          (let [[arg-t] argtys
                [dom-t] dom]
            (if-some [A (subtypeA* A0 arg-t dom-t opts)]
              (recur (subvec dom 1) (subvec argtys 1) A found-kws)
              (report-not-subtypes arg-t dom-t))))))))

(defn ^:private subtypes*-varargs
  [A0 argtys dom rst kws opts]
  {:pre [(vector? argtys)
         (vector? dom)
         ((some-fn nil? r/Type?) rst)
         ((some-fn nil? r/KwArgs?) kws)]
   :post [((some-fn set? nil?) %)]}
  (let [{:keys [result A]} (subtypes*-varargs-info A0 argtys dom rst kws opts)]
    (when (= :ok result)
      A)))

;FIXME
(defn subtype-kwargs* [A s t opts]
  {:pre [((some-fn r/KwArgs? nil?) s)
         ((some-fn r/KwArgs? nil?) t)]}
  (if (= s t)
    A
    (err/nyi-error "subtype kwargs" opts)))

;; simple co/contra-variance for ->
;[(IPersistentSet '[Type Type]) Function Function -> (IPersistentSet '[Type Type])]
(defn ^:private arr-subtype [A0 s t opts]
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
                               (cond-> (subtypeA* A* s t opts)
                                 nil? reduced))
                             A0
                             (map vector (:dom t) (:dom s)))
                     (subtypeA* (:rng s) (:rng t) opts)))
      A0
      (report-not-subtypes s t))

    (and (:prest s)
         (:prest t))
    (if (and (= (count (:dom s))
                (count (:dom t)))
             (some-> (reduce (fn [A* [s t]]
                               (cond-> (subtypeA* A* s t opts)
                                 nil? reduced))
                             A0
                             (map vector (:dom t) (:dom s)))
                     (subtypeA* (:rng s) (:rng t) opts))
             (subtypeA* A0 (:prest s) (:prest t) opts))
      A0
      (report-not-subtypes s t))

    (and (:rest s)
         (:prest t))
    (let [subtype-list? (fn [s t]
                          (every? identity
                                  (for [s s
                                        t t]
                                    (subtypeA* A0 s t opts))))
          s-dom (:dom s)
          s-dom-count (count s-dom)
          t-dom (:dom t)
          t-dom-count (count t-dom)
          s-rest (:rest s)
          t-prest-types (-> t :prest :types)
          t-prest-types-count (count t-prest-types)]
      (if-not (and (subtypeA* A0 (:rng s) (:rng t) opts)
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
    (if (and (every?' #(subtypeA* A0 %1 %2 opts) (:dom t) (:dom s))
             (subtypeA* A0 (:rng s) (:rng t) opts)
             (subtype-kwargs* A0 (:kws t) (:kws s) opts))
      A0
      (report-not-subtypes s t))

    (and (:rest s)
         (= :fixed (:kind t)))
    (if (some-> A0
                (subtypes*-varargs (:dom t) (:dom s) (:rest s) nil opts)
                (subtypeA* (:rng s) (:rng t) opts))
      A0
      (report-not-subtypes s t))

    (and (= :fixed (:kind t))
         (:rest t))
    (report-not-subtypes s t)

    (and (:rest s)
         (:rest t))
    (if (some-> A0
                (subtypes*-varargs (:dom t) (:dom s) (:rest s) nil opts)
                (subtypeA* (:rest t) (:rest s) opts)
                (subtypeA* (:rng s) (:rng t) opts))
      A0
      (report-not-subtypes s t))

    ;; TODO unit test
    ;; handle ... varargs when the bounds are the same
    (and (:drest s)
         (:drest t)
         (= (-> s :drest :name)
            (-> t :drest :name)))
    (if (and (subtypeA* A0 (-> t :drest :pre-type) (-> s :drest :pre-type) opts)
             (some-> (reduce (fn [A* [s t]]
                               (cond-> (subtypeA* A* s t opts)
                                 nil? reduced))
                             A0 (map vector (:dom t) (:dom s)))
                     (subtypeA* (:rng s) (:rng t) opts)))
      A0
      (report-not-subtypes s t))
    :else (report-not-subtypes s t)))

;[(IPersistentSet '[Type Type]) Function (t/Seqable Function) -> (Option (IPersistentSet '[Type Type]))]
(defn supertype-of-one-arr [A s ts opts]
  (some #(arr-subtype A % s opts) ts))

#_
(defn fully-resolve-filter [fl opts]
  {:pre [(fr/Filter? fl)]
   :post [(fr/Filter? %)]}
  (cond
    (fr/TypeFilter? fl) (update fl :type c/fully-resolve-type opts)
    (fr/NotTypeFilter? fl) (update fl :type c/fully-resolve-type opts)
    (fr/AndFilter? fl) (update fl :fs #(into #{} (map (fn [s] (fully-resolve-filter s opts))) %))
    (fr/OrFilter? fl) (update fl :fs #(into #{} (map (fn [s] (fully-resolve-filter s opts))) %))
    (fr/ImpFilter? fl) (-> fl
                           (update :a fully-resolve-filter opts)
                           (update :c fully-resolve-filter opts))
    :else fl))

(defn simplify-type-filter [f opts]
  {:pre [(fr/TypeFilter? f)]}
  (let [[fpth & rstpth] (:path f)]
    (cond 
      (empty? (:path f)) 
      f

      (pth-rep/KeyPE? fpth)
      (simplify-type-filter
        (fops/-filter 
          (c/make-HMap opts {:mandatory {(r/-val (:val fpth)) (:type f)}})
          (:id f)
          rstpth)
        opts)
      :else f)))

(defn ^:private subtype-type-filter* [A s t opts]
  {:pre [(fr/TypeFilter? s)
         (fr/TypeFilter? t)]}
  (let [s (simplify-type-filter s opts)
        t (simplify-type-filter t opts)]
    (if (fr/equal-paths? s t)
      (subtypeA* A (:type s) (:type t) opts)
      (report-not-subtypes s t))))

(defn simplify-not-type-filter [f opts]
  {:pre [(fr/NotTypeFilter? f)]}
  (let [[fpth & rstpth] (:path f)]
    (cond 
      (empty? (:path f)) 
      f

      (pth-rep/KeyPE? fpth)
      (simplify-not-type-filter
        (fops/-not-filter 
          ; keys is optional
          (c/make-HMap opts
            {:optional {(r/-val (:val fpth)) (:type f)}})
          (:id f)
          rstpth)
        opts)
      :else f)))

(defn ^:private subtype-not-type-filter* [A s t opts]
  {:pre [(fr/NotTypeFilter? s)
         (fr/NotTypeFilter? t)]
   :post [(or (nil? %) (set? %))]}
  (let [s (simplify-not-type-filter s opts)
        t (simplify-not-type-filter t opts)]
    (and (fr/equal-paths? s t)
         (subtypeA* A (:type t) (:type s) opts))))

(defn ^:private subtype-filter-set* [A f1 f2 opts]
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
                  (subf A fld1 fld2 opts))))]
      (or
        (and (sub-helper f1 f2 fr/TypeFilter? :then subtype-type-filter*)
             (sub-helper f1 f2 fr/TypeFilter? :else subtype-not-type-filter*))
        (and (sub-helper f1 f2 fr/TypeFilter? :then subtype-type-filter*)
             (sub-helper f1 f2 fr/NotTypeFilter? :else subtype-not-type-filter*))
        (and (sub-helper f1 f2 fr/NotTypeFilter? :then subtype-not-type-filter*)
             (sub-helper f1 f2 fr/NotTypeFilter? :else subtype-not-type-filter*))
        (and (sub-helper f1 f2 fr/NotTypeFilter? :then subtype-not-type-filter*)
             (sub-helper f1 f2 fr/TypeFilter? :else subtype-type-filter*))))))

(defn ^:private subtype-filter* [A f1 f2 opts]
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
    (subtype-type-filter* A f1 f2 opts)

    (and (fr/NotTypeFilter? f1)
         (fr/NotTypeFilter? f2))
    (subtype-not-type-filter* A f1 f2 opts)

    (fr/AndFilter? f2) (if (every? (fn [f2*]
                                     (subtype-filter* A f1 f2* opts))
                                   (:fs f2))
                         A
                         (report-not-subtypes f1 f2))
    (fr/AndFilter? f1) (some (fn [f1*]
                               (subtype-filter* A f1* f2 opts))
                             (:fs f1))
    (fr/OrFilter? f1) (if (every? (fn [f1*]
                                    (subtype-filter* A f1* f2 opts))
                                  (:fs f1))
                        A
                        (report-not-subtypes f1 f2))
    (fr/OrFilter? f2) (some (fn [f2*]
                              (subtype-filter* A f1 f2* opts))
                            (:fs f2))
    :else (report-not-subtypes f1 f2)))


(defn subtype-Result
  [A
   {t1 :t f1 :fl o1 :o :as s}
   {t2 :t f2 :fl o2 :o :as t}
   opts]
  (cond
    ;trivial case
    (and (= o1 o2)
         (subtype-filter-set* A f1 f2 opts))
    (subtypeA* A t1 t2 opts)

    ;we can ignore some interesting results
    (and (orep/EmptyObject? o2)
         (= f2 (fops/-FS fr/-top fr/-top)))
    (subtypeA* A t1 t2 opts)

    (and (orep/EmptyObject? o2)
         (= f1 f2))
    (subtypeA* A t1 t2 opts)

    ;special case for (& (is y sym) ...) <: (is y sym)
    (and (fr/AndFilter? (:then f1))
         (fr/TypeFilter? (:then f2))
         (every? fops/atomic-filter? (:fs (:then f1)))
         (= 1 (count (filter fr/TypeFilter? (:fs (:then f1)))))
         (= fr/-top (:else f2))
         (= o1 o2))
    (let [f1-tf (first (filter fr/TypeFilter? (:fs (:then f1))))]
      (if (= f1-tf (:then f2))
        (subtypeA* A t1 t2 opts)
        (report-not-subtypes t1 t2)))

    :else (report-not-subtypes t1 t2)))

(defn ^:private subtype-TypeFn-rands
  [A tfn rands1 rands2 opts]
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
                        (:covariant) (subtypeA* A l r opts)
                        (:contravariant) (subtypeA* A r l opts)
                        (:invariant) (and (subtypeA* A l r opts)
                                          (subtypeA* A r l opts))
                        (err/int-error (str "Unknown variance: " v) opts)))
                    (:variances tfn) rands1 rands2))
    A
    (report-not-subtypes rands1 rands2)))

(defn subtype-TApp
  [A s t opts]
  (let [s (update s :rator c/fully-resolve-type opts)
        t (update t :rator c/fully-resolve-type opts)
        _ (assert (= (:rator s) (:rator t)))]
    (cond
      (and (r/F? (:rator s))
           (r/F? (:rator t)))
      (let [{:keys [upper-bound] :as bnd} (free-ops/free-with-name-bnds (-> s :rator :name))]
        (cond 
          (not bnd) (err/int-error (str "No bounds for " (:name s)) opts)
          :else (let [upper-bound (c/fully-resolve-type upper-bound opts)]
                  (if (r/TypeFn? upper-bound)
                    (subtype-TypeFn-rands A upper-bound (:rands s) (:rands t) opts)
                    (report-not-subtypes s t)))))
      (r/TypeFn? (:rator s))
      (let [rator (:rator s)
            variances (:variances rator)
            names (repeatedly (:nbound rator) gensym)
            bbnds (c/TypeFn-bbnds* names rator opts)]
        (if (and (= (count variances)
                    (count (:rands s))
                    (count (:rands t)))
                 (every?' (fn [variance {:keys [lower-bound upper-bound]} s t]
                            (and (subtypeA* A lower-bound s opts)
                                 (subtypeA* A lower-bound t opts)
                                 (subtypeA* A s upper-bound opts)
                                 (subtypeA* A t upper-bound opts)
                                 (case variance
                                   :covariant (subtypeA* A s t opts)
                                   :contravariant (subtypeA* A t s opts)
                                   :invariant (and (subtypeA* A s t opts)
                                                   (subtypeA* A t s opts)))))
                          variances bbnds (:rands s) (:rands t)))
          A
          (report-not-subtypes s t)))
      :else (report-not-subtypes s t))))

(defn subtype-TypeFn
  [A S T opts]
  {:pre [(r/TypeFn? S)
         (r/TypeFn? T)]}
  (let [;instantiate both type functions with the same names
        names (repeatedly (:nbound S) gensym)
        sbnds (c/TypeFn-bbnds* names S opts)
        tbnds (c/TypeFn-bbnds* names T opts)
        sbody (c/TypeFn-body* names sbnds S opts)
        tbody (c/TypeFn-body* names tbnds T opts)]
    (if (and (= (:nbound S) (:nbound T))
             (= (:variances S) (:variances T))
             (every?' (fn [lbnd rbnd]
                        (and (subtypeA* A (:upper-bound lbnd) (:upper-bound rbnd) opts)
                             (subtypeA* A (:lower-bound rbnd) (:lower-bound lbnd) opts)
                             (subtypeA* A (:lower-bound lbnd) (:upper-bound lbnd) opts)
                             (subtypeA* A (:lower-bound rbnd) (:upper-bound rbnd) opts)))
                      sbnds tbnds))
      (subtypeA* A sbody tbody opts)
      (report-not-subtypes S T))))

(defn subtype-PrimitiveArray
  [A s t opts]
  (if (and ;(= (.jtype s) (.jtype t))
           ;contravariant
           (subtypeA* A
                      (:input-type t)
                      (:input-type s)
                      opts)
           ;covariant
           (subtypeA* A
                      (:output-type s)
                      (:output-type t)
                      opts))
    A
    (report-not-subtypes s t)))


(defn ^:private subtype-rclass-or-datatype-with-protocol
  [A s t opts]
  {:pre [((some-fn r/RClass? r/DataType?) s)
         ((some-fn r/Protocol? r/Satisfies?) t)]
   :post [(or (set? %) (nil? %))]}
  (let [s-kind (cond
                 (r/RClass? s) (do (impl/assert-clojure (str "subtype-rclass-or-datatype-with-protocol not yet implemented for implementations other than Clojure: "
                                                             (prs/unparse-type s opts) " " (prs/unparse-type t opts))
                                                        opts)
                                   :RClass)
                 (r/DataType? s) :DataType
                 :else (err/int-error (str "what is this? " s) opts))
        ;first try and find the datatype in the protocol's extenders
        in-protocol-extenders? (boolean
                                 (seq
                                   (sequence
                                     (comp (filter class?)
                                           (map coerce/Class->symbol)
                                           (filter #{(:the-class s)}))
                                     (c/Protocol-normal-extenders t opts))))
        relevant-ancestor (some (fn [p] 
                                  (let [p (c/fully-resolve-type p opts)]
                                    (when (and ((some-fn r/Protocol? r/Satisfies?) p)
                                               (= (:the-var p) (:the-var t)))
                                      p)))
                                ((case s-kind
                                   :RClass c/RClass-supers*
                                   :DataType c/Datatype-ancestors)
                                 s
                                 opts))]
    (cond
      ; the extension is via the protocol
      (or in-protocol-extenders?
          ; extension via the protocol's interface, or explicitly overriden
          relevant-ancestor)
      (let [relevant-protocol-extender (or relevant-ancestor
                                           ((case s-kind
                                              :RClass c/RClass-of-with-unknown-params
                                              :DataType c/DataType-with-unknown-params)
                                            (:the-class s)
                                            opts))]
        (subtypeA* A relevant-protocol-extender t opts))
      :else (report-not-subtypes s t))))

(defn subtype-Satisfies
  [A s t]
  {:pre [((some-fn r/Protocol? r/Satisfies?) s)
         (r/Satisfies? t)]}
  (if (= (:the-var s) (:the-var t))
    A
    (report-not-subtypes s t)))

;(t/ann subtype-datatype-rclass [DataType RClass -> Seen])
(defn ^:private subtype-datatype-rclass
  [A s t opts]
  {:pre [(r/DataType? s)
         (r/RClass? t)]}
  (impl/assert-clojure opts)
  (if-some [relevant-datatype-ancestor (some (fn [p]
                                               (let [p (c/fully-resolve-type p opts)]
                                                 (when (and (r/RClass? p)
                                                            (= (:the-class p) (:the-class t)))
                                                   p)))
                                             (c/Datatype-ancestors s opts))]
    (subtypeA* A s relevant-datatype-ancestor opts)
    (report-not-subtypes s t)))

(defn- subtype-datatypes-or-records
  [A
   {cls1 :the-class poly1 :poly? :as s} 
   {cls2 :the-class poly2 :poly? :as t}
   opts]
  {:pre [(every? r/DataType? [s t])]}
  (if (and (= cls1 cls2)
           (every?' (fn [v l r]
                      (case v
                        :covariant (subtypeA* A l r opts)
                        :contravariant (subtypeA* A r l opts)
                        :invariant (and (subtypeA* A l r opts)
                                        (subtypeA* A r l opts))))
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
   {polyr? :poly? rcls-sym :the-class :as t}
   opts]
  (impl/assert-clojure opts)
  (let [subtypeA* #(subtypeA* A %1 %2 opts)
        {variances :variances} s]
    (and (= lcls-sym rcls-sym)
         (or (and (empty? polyl?) (empty? polyr?))
             (and (seq polyl?)
                  (seq polyr?)
                  (every?' #(case %1
                              :covariant (subtypeA* %2 %3)
                              :contravariant (subtypeA* %3 %2)
                              (and (subtypeA* %2 %3)
                                   (subtypeA* %3 %2)))
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
  [s t opts]
  (impl/assert-clojure opts)
  (cond
    ; (U Integer Long) <: (U int long)
    (and 
      (#{(c/RClass-of Integer opts) (c/RClass-of Long opts)} s)
      (#{(c/RClass-of 'int opts) (c/RClass-of 'long opts)} t))
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
  [A s t opts]
  {:pre [((some-fn r/RClass? r/Instance?) s)
         ((some-fn r/RClass? r/Instance?) t)]}
  (impl/assert-clojure opts)
  (let [scls (r/RClass->Class s)
        tcls (r/RClass->Class t)]
    (cond
      ((some-fn r/Instance?) s t)
      (if (r/Instance? t)
        ;; (U RClass Instance) <: Instance
        (if (class-isa? scls tcls)
          A
          (report-not-subtypes s t))
        ;;  :< (U RClass Instance)
        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(when (and ((some-fn r/RClass? r/Instance?) %)
                          (= (:the-class t) (:the-class %)))
                 (subtype-RClass-common-base A % t opts))
              (map #(c/fully-resolve-type % opts) (c/RClass-supers* s opts))))

      :else
      ;; RClass <: RClass
      (let [{polyl? :poly?} s
            {polyr? :poly?} t]
        (if (or
              ; use java subclassing
              (and (empty? polyl?)
                   (empty? polyr?)
                   (class-isa? scls tcls))

              ;same base class
              (and (= scls tcls)
                   (subtype-RClass-common-base A s t opts))

              ;one is a primitive, coerce
              (and (or (.isPrimitive scls)
                       (.isPrimitive tcls))
                   (coerce-RClass-primitive s t opts))

              ;find a supertype of s that is the same base as t, and subtype of it
              (some #(when (r/RClass? %)
                       (and (= (:the-class t) (:the-class %))
                            (subtype-RClass-common-base A % t opts)))
                    (map #(c/fully-resolve-type % opts) (c/RClass-supers* s opts))))
          A
          (report-not-subtypes s t))))))

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

#_
(defmacro sub-clj? [s t]
  `(impl/with-clojure-impl
     (subtype? (prs/parse-type '~s)
               (prs/parse-type '~t))))

(defn has-kind? [t kind opts]
  {:post [(boolean? %)]}
  (cond
    (r/Bounds? kind) (and (subtype? (:lower-bound kind) t opts)
                          (subtype? t (:upper-bound kind) opts))
    :else (err/nyi-error "non-type kinds")))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.cs-gen
  (:refer-clojure :exclude [requiring-resolve repeatedly])
  (:require [clojure.core.typed :as t :refer [letfn>]]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.set :as set]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljc.runtime.perf-utils :refer [repeatedly]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub] ; use subtype? utility defined in this namespace
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.cs-rep :as cr]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.fold-rep :as f]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.object-rep :as or]
            [typed.cljc.checker.promote-demote :as prmt]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  (:import (typed.cljc.checker.cs_rep c cset dcon dmap cset-entry)
           (typed.cljc.checker.type_rep Top Wildcard DottedPretype F DataType Function Protocol Bounds TCResult HSequential SymbolicClosure
                                        GetType MergeType AssocType Regex MatchType)))

(t/typed-deps typed.cljc.checker.free-ops
              typed.cljc.checker.promote-demote)

(t/ann ^:no-check typed.clj.checker.subtype/subtype? [r/AnyType r/AnyType t/Any -> Boolean])
(t/ann ^:no-check clojure.core.typed.current-impl/current-impl [t/Any -> t/Any])
(t/ann ^:no-check clojure.core.typed.current-impl/checking-clojure? [t/Any -> t/Any])

(t/ann gen-repeat (t/All [x] [t/Int (t/Seqable x) -> (t/Vec x)]))
(defn ^:private gen-repeat [times repeated]
  (reduce into [] (repeat times repeated)))

(def inferrable-symbolic-closure-expected-type?
  (comp (some-fn r/FnIntersection? r/Poly? r/PolyDots?)
        c/fully-resolve-type))

; (partition-by-nth 2 [1 2 3 4 5 6]) -> ((1 3 5) (2 4 6))
; (partition-by-nth 3 [1 2 3 4 5 6]) -> ((1 4) (2 5) (3 6))
; util for infer-pdot
(t/ann partition-by-nth (t/All [a] [Number (t/Seqable a) -> (t/Seq (t/Seq a))]))
(defn partition-by-nth [n lst]
  {:pre [(zero? (rem (count lst) n))]}
  (let [keep-rem-of (t/fn keep-rem-of [i :- Number]
                      (keep-indexed (t/fn [index :- Number
                                           item :- a]
                                      (when (= (rem index n) i)
                                        item))
                                    lst))]
    (map keep-rem-of (range n))))

(t/ann subtype? [r/AnyType r/AnyType t/Any -> Boolean])
(defn ^:private subtype? [s t opts]
  (sub/subtype? s t opts))

(t/ann fail! [t/Any t/Any -> t/Nothing])
(defn fail! [s t]
  (throw u/cs-gen-exn))

(defmacro handle-failure [& body]
  `(u/handle-cs-gen-failure ~@body))

;; scoped type variable bounds
(def X? (con/hash-c? symbol? r/Bounds?))
;; scoped dotted variable bounds
(def Y? (con/hash-c? symbol? r/Regex?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation

(t/ann meet [r/Type r/Type t/Any -> r/Type])
(defn meet [s t opts] opts (c/In [s t] opts))

(t/ann join [r/Type r/Type t/Any -> r/Type])
(defn join [s t opts] (c/Un [s t] opts))

(t/ann c-meet (t/IFn [c c (t/U nil t/Sym) t/Any -> c]
                     [c c t/Any -> c]))
(defn c-meet
  ([c1 c2 opts] (c-meet c1 c2 nil opts))
  ([{S  :S X  :X T  :T bnds  :bnds :as c1}
    {S* :S X* :X T* :T bnds* :bnds :as c2}
    var
    opts]
   (when-not (or var (= X X*))
     (err/int-error (str "Non-matching vars in c-meet:" X X*) opts))
   (when-not (= bnds bnds*)
     (err/int-error (str "Non-matching bounds in c-meet:" bnds bnds*) opts))
   (let [;_ (prn "joining" S S* #_(mapv r/wild? [S S*]))
         S (join S S* opts)
         ;_ (prn "joined:" S)
         ;_ (prn "meeting" T T* #_(mapv r/wild? [T T*]))
         T (meet T T* opts)
         ;_ (prn "meet:" T)
         ]
     (when-not (subtype? S T opts)
       (fail! S T))
     (cr/c-maker S (or var X) T bnds))))

(declare dmap-meet)

;FIXME flow error when checking
(t/ann cset-meet [cset cset t/Any -> cset])
(defn cset-meet [{maps1 :maps :as x} {maps2 :maps :as y} opts]
  {:pre [(cr/cset? x)
         (cr/cset? y)]
   :post [(cr/cset? %)]}
  (let [maps (persistent!
              (reduce (fn [acc {map1 :fixed dmap1 :dmap}]
                        (reduce (fn [acc {map2 :fixed dmap2 :dmap}]
                                  (if-let [r (handle-failure
                                               (cr/cset-entry-maker
                                                (merge-with #(c-meet %1 %2 opts) map1 map2)
                                                (dmap-meet dmap1 dmap2 opts)))]
                                    (conj! acc r)
                                    acc))
                                acc maps2))
                      (transient []) maps1))]
    (when (empty? maps)
      (fail! maps1 maps2))
    (cr/cset-maker maps)))

(t/ann cset-meet* [(t/Seqable cset) t/Any -> cset])
(defn cset-meet* [args opts]
  {:pre [(every? cr/cset? args)]
   :post [(cr/cset? %)]}
  (reduce #(cset-meet %1 %2 opts) (cr/empty-cset {} {}) args))

(t/ann cset-combine [(t/Seqable cset) -> cset])
(defn cset-combine [l]
  {:pre [(every? cr/cset? l)]}
  (cr/cset-maker (mapcat :maps l)))

;add new constraint to existing cset
(t/ann insert-constraint [cset t/Sym r/Type r/Type Bounds -> cset])
(defn insert-constraint [cs var S T bnds]
  {:pre [(cr/cset? cs)
         (symbol? var)
         (r/Type? S)
         (r/Type? T)
         (r/Bounds? bnds)]
   :post [(cr/cset? %)]}
  (cr/cset-maker
    (doall
      (for [{fmap :fixed dmap :dmap} (:maps cs)]
        (cr/cset-entry-maker 
          (assoc fmap var (cr/c-maker S var T bnds))
          dmap)))))

; FIXME no-checked because of massive performance issues. revisit
(t/ann ^:no-check dcon-meet [cr/DCon cr/DCon t/Any -> cr/DCon])
(defn dcon-meet [dc1 dc2 opts]
  {:pre [(cr/dcon-c? dc1)
         (cr/dcon-c? dc2)]
   :post [(cr/dcon-c? %)]}
  (cond
    (and (cr/dcon-exact? dc1)
         (or (cr/dcon? dc2) 
             (cr/dcon-exact? dc2)))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2]
      (when-not (and rest2 (= (count fixed1) (count fixed2)))
        (fail! fixed1 fixed2))
      (cr/dcon-exact-maker
        (mapv (fn [c1 c2] (c-meet c1 c2 (:X c1) opts))
              fixed1 fixed2)
        (c-meet rest1 rest2 (:X rest1) opts)))
    ;; redo in the other order to call the first case
    (and (cr/dcon? dc1)
         (cr/dcon-exact? dc2))
    (dcon-meet dc2 dc1 opts)

    (and (cr/dcon? dc1)
         (not (:rest dc1))
         (cr/dcon? dc2)
         (not (:rest dc2)))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed} dc2]
      (when-not (= (count fixed1) (count fixed2))
        (fail! fixed1 fixed2))
      (cr/dcon-maker
        (mapv (fn [c1 c2] (c-meet c1 c2 (:X c1) opts))
              fixed1 fixed2)
        nil))

    (and (cr/dcon? dc1)
         (not (:rest dc1))
         (cr/dcon? dc2))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed rest :rest} dc2]
      (assert rest)
      (when-not (>= (count fixed1) (count fixed2))
        (fail! fixed1 fixed2))
      (cr/dcon-maker
        (mapv (fn [c1 c2] (c-meet c1 c2 (:X c1) opts))
              fixed1 (concat fixed2 (repeat rest)))
        nil))

    (and (cr/dcon? dc1)
         (cr/dcon? dc2)
         (not (:rest dc2)))
    (dcon-meet dc2 dc1 opts)

    (and (cr/dcon? dc1)
         (cr/dcon? dc2))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2
          [shorter longer srest lrest]
          (if (< (count fixed1) (count fixed2))
            [fixed1 fixed2 rest1 rest2]
            [fixed2 fixed1 rest2 rest1])]
      (cr/dcon-maker
        (mapv (fn [c1 c2] (c-meet c1 c2 (:X c1) opts))
              longer (concat shorter (repeat srest)))
        (c-meet lrest srest (:X lrest) opts)))

    (and (cr/dcon-dotted? dc1)
         (cr/dcon-dotted? dc2))
    (let [{fixed1 :fixed c1 :dc {bound1 :name} :dbound} dc1
          {fixed2 :fixed c2 :dc {bound2 :name} :dbound} dc2]
      (when-not (and (= (count fixed1) (count fixed2))
                     (= bound1 bound2))
        (fail! bound1 bound2))
      (cr/dcon-dotted-maker 
        (mapv (fn [c1 c2] (c-meet c1 c2 (:X c1) opts)) fixed1 fixed2)
        (c-meet c1 c2 bound1 opts) bound1))

    (and (cr/dcon? dc1)
         (cr/dcon-dotted? dc2))
    (fail! dc1 dc2)

    (and (cr/dcon-dotted? dc1)
         (cr/dcon? dc2))
    (fail! dc1 dc2)

    (and (cr/dcon-repeat? dc1)
         (cr/dcon? dc2)
         (not (:rest dc2)))
    (let [{fixed1 :fixed repeated :repeat} dc1
          {fixed2 :fixed} dc2
          fixed1-count (count fixed1)
          fixed2-count (count fixed2)
          repeat-count (count repeated)
          diff (- fixed2-count fixed1-count)]
      (assert repeated)
      (when-not (and (>= fixed2-count fixed1-count)
                    (zero? (rem diff repeat-count)))
        (fail! fixed1 fixed2))
      (cr/dcon-repeat-maker
        (mapv (fn [c1 c2]
                (c-meet c1 c2 (:X c1) opts))
              fixed2
              (concat fixed1
                      (gen-repeat (quot diff repeat-count) repeated)))
        repeated))
    (and (cr/dcon-repeat? dc2)
         (cr/dcon? dc1)
         (not (:rest dc1)))
    (dcon-meet dc2 dc1 opts)

    (every? cr/dcon-repeat? [dc1 dc2])
    (let [[{short-fixed :fixed short-repeat :repeat}
           {long-fixed :fixed long-repeat :repeat}]
          (sort-by (fn [x] (-> x :fixed count)) [dc1 dc2])
          s-fixed-count (count short-fixed)
          l-fixed-count (count long-fixed)
          s-repeat-count (count short-repeat)
          l-repeat-count (count long-repeat)
          diff (- l-fixed-count s-fixed-count)
          _ (assert (= s-repeat-count l-repeat-count))
          merged-repeat (for [[c1 c2] (map vector short-repeat long-repeat)]
                          (c-meet c1 c2 (:X c1) opts))]
      (assert (zero? (rem diff s-repeat-count)))
      (cr/dcon-repeat-maker
        (mapv (fn [c1 c2]
                (c-meet c1 c2 (:X c1) opts))
              long-fixed
              (concat short-fixed
                      (gen-repeat (quot diff s-repeat-count) short-repeat)))
        merged-repeat))

    :else (err/nyi-error (str "NYI dcon-meet " dc1 dc2) opts)))

(t/ann dmap-meet [dmap dmap t/Any -> dmap])
(defn dmap-meet [dm1 dm2 opts]
  {:pre [(cr/dmap? dm1)
         (cr/dmap? dm2)]
   :post [(cr/dmap? %)]}
  (cr/dmap-maker (merge-with #(dcon-meet %1 %2 opts) (:map dm1) (:map dm2))))

(t/defalias NoMentions
  "A set of variables not to mention in the constraints"
  (t/Set t/Sym))

(t/defalias ConstrainVars
  "The map of variables to be constrained to their bounds"
  (t/Map t/Sym Bounds))

(t/defalias ConstrainDVars
  "The map of dotted variables to be constrained to their bounds"
  (t/Map t/Sym Regex))

;; V : a set of variables not to mention in the constraints
;; X : the map of type variables to be constrained to their bounds
;; Y : the map of index variables to be constrained to their bounds
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner, extended with
;; the index variables from the TOPLAS paper

(declare cs-gen-right-F cs-gen-left-F cs-gen-datatypes-or-records cs-gen-list
         cs-gen-filter-set cs-gen-object cs-gen-HSequential cs-gen-TApp
         cs-gen-Function cs-gen-FnIntersection cs-gen-Result cs-gen-RClass
         cs-gen-Protocol get-c-from-cmap)

(defn homogeneous-dbound->bound [dbnd opts]
  {:pre [(r/Regex? dbnd)]
   :post [(r/Bounds? %)]}
  (let [fst (-> dbnd :types first)]
    (when-not (and (= :* (:kind dbnd))
                   (r/Bounds? fst))
      (err/nyi-error "Inference with interesting dotted bounds" opts))
    fst))

(t/ann ^:no-check cs-gen 
       [(t/Set t/Sym) 
        (t/Map t/Sym Bounds)
        (t/Map t/Sym Bounds)
        r/AnyType
        r/AnyType
        -> cset])
(defn cs-gen [V X Y S T {::keys [cs-current-seen] :as opts
                         :or {cs-current-seen #{}}}]
  {:pre [((con/set-c? symbol?) V)
         (X? X)
         (Y? Y)
         (r/AnyType? S)
         (r/AnyType? T)]
   :post [(cr/cset? %)]}
  ;(prn "cs-gen" (class S) (class T) S T (count cs-current-seen))
  (if (or (cs-current-seen [S T])
          (subtype? S T opts))
    ;already been around this loop, is a subtype
    (cr/empty-cset X Y)
    (let [opts (assoc opts ::cs-current-seen (conj cs-current-seen [S T]))]
      (cond
        ;IMPORTANT: handle frees first
        (and (r/F? S)
             (contains? X (:name S)))
        (cs-gen-left-F V X Y S T opts)

        (and (r/F? T)
             (contains? X (:name T)))
        (cs-gen-right-F V X Y S T opts)
        
        ;values are subtypes of their classes
        (and (r/Value? S)
             (impl/checking-clojure? opts))
        (let [sval (:val S)]
          (impl/impl-case opts
            :clojure (if (nil? sval)
                       (fail! S T)
                       (cs-gen V X Y
                               (c/In (cons (c/RClass-of (class sval) opts)
                                           (cond 
                                             ;keyword values are functions
                                             (keyword? sval) [(c/keyword->Fn sval opts)]
                                             ;strings have a known length as a seqable
                                             (string? sval) [(r/make-ExactCountRange (count sval))]))
                                     opts)
                               T opts))
            :cljs (cond
                    (integer? sval) (cs-gen V X Y (r/CLJSInteger-maker) T opts)
                    (number? sval) (cs-gen V X Y (r/JSNumber-maker) T opts)
                    (string? sval) (cs-gen V X Y (r/JSString-maker) T opts)
                    (boolean? sval) (cs-gen V X Y (r/JSBoolean-maker) T opts)
                    (symbol? sval) (cs-gen V X Y (c/DataType-of 'cljs.core/Symbol opts) T opts)
                    (keyword? sval) (cs-gen V X Y (c/DataType-of 'cljs.core/Keyword opts) T opts)
                    :else (fail! S T))))

        (r/Name? S)
        (cs-gen V X Y (c/resolve-Name S opts) T opts)

        (r/Name? T)
        (cs-gen V X Y S (c/resolve-Name T opts) opts)

        (and (r/TApp? S)
             (r/TApp? T)
             (= (:rator S) (:rator T)))
        (cs-gen-TApp V X Y S T opts)

        (and (r/TApp? S)
             (not (r/F? (:rator S))))
        (cs-gen V X Y (c/resolve-TApp S opts) T opts)

        (and (r/TApp? T)
             (not (r/F? (:rator T))))
        (cs-gen V X Y S (c/resolve-TApp T opts) opts)

        ; copied from TR's infer-unit
        ;; if we have two mu's, we rename them to have the same variable
        ;; and then compare the bodies
        ;; This relies on (B 0) only unifying with itself, and thus only hitting the first case of this `match'
        (and (r/Mu? S)
             (r/Mu? T))
        (cs-gen V X Y (r/Mu-body-unsafe S) (r/Mu-body-unsafe T) opts)

        ;; other mu's just get unfolded
        (r/Mu? S) (cs-gen V X Y (c/unfold S opts) T opts)
        (r/Mu? T) (cs-gen V X Y S (c/unfold T opts) opts)

        ;; similar to Mu+Mu case
        (and (r/Poly? S)
             (r/Poly? T)
             (= (:nbound S) (:nbound T))
             (= (:bbnds S) (:bbnds T)))
        (let [names (c/Poly-fresh-symbols* T)
              bbnds (c/Poly-bbnds* names T opts)
              S' (c/Poly-body* names S opts)
              T' (c/Poly-body* names T opts)]
          (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
            (cs-gen V X Y S' T' opts)))


        ;constrain *each* element of S to be below T, and then combine the constraints
        (r/Union? S)
        (cset-meet*
          (cons (cr/empty-cset X Y)
                (mapv #(cs-gen V X Y % T opts) (:types S)))
          opts)

        ;; find *an* element of T which can be made a supertype of S
        (r/Union? T)
        (if-let [cs (not-empty
                     (into [] (keep #(handle-failure (cs-gen V X Y S % opts)))
                           (:types T)))]
          (cset-combine cs)
          (fail! S T))

        ; Does it matter which order the Intersection cases go?

        ;constrain *every* element of T to be above S, and then meet the constraints
        ; we meet instead of cset-combine because we want all elements of T to be under
        ; S simultaneously.
        (r/Intersection? T)
        (let [ts (sub/simplify-In T opts)]
          (cset-meet*
            (cons (cr/empty-cset X Y)
                  (mapv #(cs-gen V X Y S % opts) ts))
            opts))

        ;; find *an* element of S which can be made a subtype of T
        (r/Intersection? S)
        (let [ss (sub/simplify-In S opts)]
          (if-let [cs (some #(handle-failure (cs-gen V X Y % T opts))
                            ss)]
            (do ;(prn "intersection S normal case" (map #(prs/unparse-type % opts) [S T]))
                cs)
            (fail! S T)))

        (r/App? S) (cs-gen V X Y (c/resolve-App S opts) T opts)
        (r/App? T) (cs-gen V X Y S (c/resolve-App T opts opts))

        ;; constrain body to be below T, but don't mention the new vars
        (r/Poly? S)
        (let [nms (c/Poly-fresh-symbols* S)
              body (c/Poly-body* nms S opts)
              bbnds (c/Poly-bbnds* nms S opts)]
          (free-ops/with-bounded-frees (zipmap (map r/make-F nms) bbnds)
            (cs-gen (set/union (set nms) V) X Y body T opts)))

        (and (r/DataType? S)
             (r/DataType? T)) (cs-gen-datatypes-or-records V X Y S T)

        (and ((some-fn r/RClass? r/DataType?) S)
             (r/Protocol? T))
        (or (some #(when (and (r/Protocol? %)
                              (= (:the-var %)
                                 (:the-var T)))
                     (cs-gen V X Y % T opts))
                  (map #(c/fully-resolve-type % opts)
                       (if (r/RClass? S)
                         (c/RClass-supers* S opts)
                         (c/Datatype-ancestors S opts))))
            (fail! S T))

        ; handle Record as HMap
        (r/Record? S) (cs-gen V X Y (c/Record->HMap S) T opts)

        (and (r/HSequential? S)
             (r/HSequential? T))
        (cs-gen-HSequential V X Y S T opts)

        (and (r/HeterogeneousMap? S)
             (r/HeterogeneousMap? T))
    ; assumes optional/mandatory/absent keys are disjoint
        (let [Skeys (set (keys (:types S)))
              Tkeys (set (keys (:types T)))
              Soptk (set (keys (:optional S)))
              Toptk (set (keys (:optional T)))
              Sabsk (:absent-keys S)
              Tabsk (:absent-keys T)]
          ; All keys must be values
          (when-not (every? r/Value? 
                            (concat
                              Skeys Tkeys
                              Soptk Toptk
                              Sabsk Tabsk))
            (fail! S T))
          ; If the right is complete, the left must also be complete
          (when (c/complete-hmap? T)
            (when-not (c/complete-hmap? S)
              (fail! S T)))
          ; check mandatory keys
          (if (c/complete-hmap? T)
            ; If right is complete, mandatory keys must be identical
            (when-not (= Tkeys Skeys)
              (fail! S T))
            ; If right is partial, all mandatory keys on the right must also appear mandatory on the left
            (when-not (empty? (set/difference Tkeys 
                                Skeys))
              (fail! S T)))
          ; All optional keys on the right must appear either absent, mandatory or optional
          ; on the left
          (when-not (empty? (set/difference Toptk 
                              (set/union Skeys 
                                         Soptk 
                                         Sabsk)))
            (fail! S T))
          ; All absent keys on the right must appear absent on the left
          (when-not (empty? (set/difference Tabsk
                              Sabsk))
            (fail! S T))
          ; now check the values with cs-gen
          (let [;only check mandatory entries that appear on the right
                check-mandatory-keys Tkeys
                Svals (map (:types S) check-mandatory-keys)
                Tvals (map (:types T) check-mandatory-keys)
                _ (assert (every? r/Type? Svals))
                _ (assert (every? r/Type? Tvals))
                ;only check optional entries that appear on the right
                ; and also appear as mandatory or optional on the left
                check-optional-keys (set/intersection
                                      Toptk (set/union Skeys Soptk))
                Sopts (map (some-fn (:types S) (:optional S)) check-optional-keys)
                Topts (map (:optional T) check-optional-keys)
                _ (assert (every? r/Type? Sopts))
                _ (assert (every? r/Type? Topts))]
            (cset-meet* [(cs-gen-list V X Y Svals Tvals {} opts)
                         (cs-gen-list V X Y Sopts Topts {} opts)]
                        opts)))

        ; covariant, as in TS and GClosure
        (and (r/JSObj? S)
             (r/JSObj? T))
        (let [{Stypes :types} S
              {Ttypes :types} T
              Svals (map Stypes (keys Ttypes))
              Tvals (vals Ttypes)]
          (if (every? r/Type? Svals)
            (cs-gen-list V X Y Svals Tvals {} opts)
            (fail! S T)))

        ((every-pred r/GetType?) S T)
        (cset-meet* [(cs-gen V X Y (:target S) (:target T) opts)
                     (cs-gen V X Y (:key S) (:key T) opts)
                     (cs-gen V X Y (:not-found S) (:not-found T) opts)]
                    opts)

        (or (and (r/GetType? S)
                 (c/Get-requires-resolving? S opts))
            (and (r/MergeType? S)
                 (c/Merge-requires-resolving? S opts)))
        (cs-gen V X Y (c/-resolve S opts) T opts)

        (or (and (r/GetType? T)
                 (c/Get-requires-resolving? T opts))
            (and (r/MergeType? T)
                 (c/Merge-requires-resolving? T opts)))
        (cs-gen V X Y S (c/-resolve T opts) opts)

        (and (r/AssocType? S)
             (r/AssocType? T))
        (let [{S-target :target S-entries :entries S-dentries :dentries} S
              {T-target :target T-entries :entries T-dentries :dentries} T
              cg #(cs-gen V X Y %1 %2 opts)
              target-cset (cg S-target T-target)
              S-entries (apply concat S-entries)
              T-entries (apply concat T-entries)
              entries-cset (cs-gen-list V X Y S-entries T-entries {} opts)
              _ (when (or S-dentries T-dentries)
                  (err/nyi-error "NYI dentries of Assoc in cs-gen" opts))]
          (cset-meet* [target-cset entries-cset] opts))

        (and (r/AssocType? S)
             (r/RClass? T)
             ; (Map xx yy)
             (= 'clojure.lang.IPersistentMap (:the-class T)))
        (let [{:keys [target entries dentries]} S
              {:keys [poly? the-class]} T
              dentries-cset (when-some [{dty :pre-type dbound :name} dentries]
                              (when-not (Y dbound)
                                (fail! S T))
                              ;(println "passed when")
                              (let [merged-X (assoc X dbound (homogeneous-dbound->bound (Y dbound) opts))
                                    get-list-of-c (fn get-list-of-c [t-list]
                                                    (mapv #(get-c-from-cmap % dbound opts)
                                                          (for [t t-list]
                                                            (cs-gen V merged-X Y dty t opts))))
                                    repeat-c (get-list-of-c poly?)]
                                (assoc-in (cr/empty-cset X Y)
                                          [:maps 0 :dmap :map dbound]
                                          ; don't constrain on fixed, otherwise will fail
                                          ; on (assoc m x y)
                                          (cr/dcon-repeat-maker [] repeat-c))))
              ;_ (println "dentries-cset" dentries-cset)

              ; if it's nil, we also accept it
              map-cset (when-not (sub/subtype? target r/-nil opts)
                         (cs-gen V X Y target T opts))
              entries-keys (map first entries)
              entries-vals (map second entries)
              cg #(cs-gen V X Y %1 %2 opts)
              key-cset (map cg entries-keys (repeat (first poly?)))
              val-cset (map cg entries-vals (repeat (second poly?)))]
          (cset-meet* (concat (when map-cset [map-cset]) key-cset val-cset) opts))

        ; transform Record to HMap, this is not so useful until we can do
        ; cs-gen Assoc with dentries with HMap
        (and (r/AssocType? S)
             (r/Record? T))
        (let [{:keys [target]} S
              target-cset (cs-gen V X Y target T opts)
              cset (cs-gen V X Y S (c/Record->HMap T) opts)]
          (cset-meet* [target cset] opts))

; Completeness matters:
;
; (Assoc x ':a Number ':b Long) <: (HMap {:a Number :b Long} :complete? true)
; (Assoc x ':a Number ':b Long ':c Foo) <!: (HMap {:a Number :b Long} :complete? true)
        (and (r/AssocType? S)
             (r/HeterogeneousMap? T))
        (let [;_ (prn "cs-gen Assoc HMap")
              {:keys [target entries dentries]} S
              {:keys [types absent-keys]} T
              _ (when-not (nil? dentries) (err/nyi-error (pr-str "NYI cs-gen of dentries AssocType with HMap " S T) opts))
              Assoc-keys (map first entries)
              Tkeys (keys types)
              ; All keys must be keyword values
              _ (when-not (every? c/keyword-value? (concat Tkeys Assoc-keys absent-keys))
                  (fail! S T))
              ; All keys explicitly not in T should not appear in the Assoc operation
              absents-satisfied?
              (if (c/complete-hmap? T)
                ; if T is partial, we just need to ensure the absent keys in T
                ; don't appear in the entries of the Assoc.
                (empty?
                  (set/intersection
                    (set absent-keys)
                    (set (map first entries))))
                ; if T is complete, all entries of the Assoc should *only* have
                ; keys that are mandatory keys of T.
                (empty?
                  (set/difference
                    (set (map first entries))
                    (set Tkeys))))
              _ (when-not absents-satisfied?
                  (fail! S T))
              ;; Isolate the entries of Assoc in a new HMap, with a corresponding expected HMap.
              ; keys on the right overwrite those on the left.
              assoc-args-hmap (c/make-HMap opts {:mandatory (into {} entries)})
              expected-assoc-args-hmap (c/make-HMap opts {:mandatory (select-keys (:types assoc-args-hmap) (set Assoc-keys))})
              
              ;; The target of the Assoc needs all the keys not explicitly Assoc'ed.
              expected-target-hmap 
              (let [types (select-keys (into {} entries)
                                       (set/difference (set Assoc-keys) (set Tkeys)))]
                (if (c/complete-hmap? T) 
                  (c/-complete-hmap types opts)
                  (c/-partial-hmap opts types absent-keys)))
              
              ;_ (prn assoc-args-hmap :< expected-assoc-args-hmap)
              ;_ (prn (:target S) :< expected-target-hmap)
              ]
          (cs-gen-list V X Y
                       [assoc-args-hmap 
                        (:target S)]
                       [expected-assoc-args-hmap
                        expected-target-hmap]
                       {} opts))

        (and (r/AssocType? S)
             (r/HeterogeneousVector? T))
        (let [elem-type (c/Un (concat
                                (:types T)
                                (some-> (:rest T) vector)
                                (when (:drest T)
                                  [r/-any]))
                              opts)
              vec-any (r/-hvec [] {:rest r/-any} opts)
              num-type (c/RClass-of 'java.lang.Number opts)
              target-cset (cs-gen V X Y (:target S) vec-any opts)
              entries-key (map first (:entries S))
              entries-val (map second (:entries S))
              key-cset (cs-gen-list V X Y entries-key (repeat (count entries-key)
                                                              num-type)
                                    {} opts)
              ;_ (println "key-cset" key-cset)
              val-cset (cs-gen-list V X Y entries-val (repeat (count entries-val)
                                                              elem-type)
                                    {} opts)
              ;_ (println "val-cset" val-cset)
              dentries-cset (when-some [{dty :pre-type dbound :name} (:dentries S)]
                              (when-not (Y dbound)
                                (fail! S T))
                              ;(println "passed when")
                              (let [merged-X (assoc X dbound (homogeneous-dbound->bound (Y dbound) opts))
                                    get-list-of-c (fn get-list-of-c [t-list]
                                                    (mapv #(get-c-from-cmap (cs-gen V merged-X Y dty % opts)
                                                                            dbound opts)
                                                          t-list))
                                    repeat-c (get-list-of-c [num-type elem-type])]
                                (assoc-in (cr/empty-cset X Y)
                                          [:maps 0 :dmap :map dbound]
                                          ; don't constrain on fixed, otherwise will fail
                                          ; on (assoc m x y)
                                          (cr/dcon-repeat-maker [] repeat-c))))]
          (cset-meet* (concat [target-cset key-cset val-cset]
                              (when dentries-cset [dentries-cset]))
                      opts))

        (and (r/PrimitiveArray? S)
             (r/PrimitiveArray? T)
             (impl/checking-clojure? opts))
        (cs-gen-list 
          V X Y
          ;input contravariant
          ;output covariant
          [(:input-type T) (:output-type S)]
          [(:input-type S) (:output-type T)]
          {} opts)

        ; some RClass's have heterogeneous vector ancestors (in "unchecked ancestors")
        ; It's useful to also trigger this case with HSequential, as that's more likely
        ; to be on the right.
        (and (r/RClass? S)
             (r/HSequential? T))
        (if-let [[Sv] (seq
                        (filter r/HSequential? (map #(c/fully-resolve-type % opts) (c/RClass-supers* S opts))))]
          (cs-gen V X Y Sv T opts)
          (fail! S T))
        
        (and (r/FnIntersection? S)
             (r/FnIntersection? T))
        (cs-gen-FnIntersection V X Y S T opts)

        ;; extract IFn unchecked ancestor
        (and (r/RClass? S)
             (c/ifn-ancestor S opts) ;;FIXME don't recalculate in consequent
             (r/FnIntersection? T))
        (cs-gen V X Y (c/ifn-ancestor S opts) T opts)

        (and (r/Function? S)
             (r/Function? T))
        (cs-gen-Function V X Y S T opts)

        (and (r/Result? S)
             (r/Result? T))
        (cs-gen-Result V X Y S T opts)

        (and (r/Value? S)
             (r/AnyValue? T))
        (cr/empty-cset X Y)

        ; TODO add :repeat support
        (and (r/HSequential? S)
             (r/RClass? T))
        (cs-gen V X Y
                (let [ss (c/Un (concat
                                 (:types S)
                                 (when-let [rest (:rest S)]
                                   [rest])
                                 (when (:drest S)
                                   [r/-any]))
                               opts)]
                  (c/In [(impl/impl-case opts
                           :clojure (case (:kind S)
                                      :vector (c/RClass-of clojure.lang.APersistentVector [ss] opts)
                                      :seq (c/RClass-of clojure.lang.ISeq [ss] opts)
                                      :list (c/RClass-of clojure.lang.IPersistentList [ss] opts)
                                      :sequential (c/In [(c/RClass-of clojure.lang.IPersistentCollection [ss] opts)
                                                         (c/RClass-of clojure.lang.Sequential opts)]
                                                        opts))
                           :cljs (throw (Exception. "TODO CLJS HSequential cs-gen")))
                         ((if (or (:rest S) (:drest S)) r/make-CountRange r/make-ExactCountRange)
                          (count (:types S)))]
                        opts))
                T opts)


        (and ((some-fn r/RClass? r/Instance?) S)
             ((some-fn r/RClass? r/Instance?) T))
        (cs-gen-RClass V X Y S T opts)

        (and (r/Protocol? S)
             (r/Protocol? T))
        (cs-gen-Protocol V X Y S T opts)

        (r/HeterogeneousMap? S)
        (let [new-S (c/upcast-hmap S opts)]
          (cs-gen V X Y new-S T opts))

        (r/HSet? S)
        (let [new-S (c/upcast-hset S opts)]
          (cs-gen V X Y new-S T opts))

        (r/HSequential? S)
        (cs-gen V X Y (c/upcast-HSequential S opts) T opts)

        (and (r/AssocType? S)
             (r/Protocol? T))
        (cs-gen V X Y (:target S) T opts)

        :else (fail! S T)))))

(declare var-store-take move-vars-to-dmap)

(t/ann cs-gen-HSequential [NoMentions ConstrainVars ConstrainDVars HSequential HSequential t/Any
                           -> cset])
(defn cs-gen-HSequential
  [V X Y S T opts]
  {:pre [(r/HSequential? S)
         (r/HSequential? T)]
   :post [(cr/cset? %)]}
  (when-not (r/compatible-HSequential-kind? (:kind S) (:kind T))
    (fail! S T))
  (cset-meet* (concat
                (cond
                  ;simple case
                  (not-any? (some-fn :rest :drest :repeat) [S T])
                  [(cs-gen-list V X Y (:types S) (:types T) {} opts)]

                  ;rest on right, optionally on left
                  (and (:rest T)
                       (not-any? (some-fn :drest :repeat) [S]))
                  (concat [(cs-gen-list V X Y (:types S) (concat (:types T)
                                                                 (repeat (- (count (:types S))
                                                                            (count (:types T)))
                                                                         (:rest T)))
                                        {} opts)]
                          (when (:rest S)
                            [(cs-gen V X Y (:rest S) (:rest T) opts)]))

                  ; repeat on right, nothing on left
                  (and (:repeat T)
                       (not-any? (some-fn :rest :drest :repeat) [S]))
                  (let [s-types (:types S)
                        t-types (:types T)
                        s-types-count (count s-types)
                        t-types-count (count t-types)]
                    (if (and (>= s-types-count t-types-count)
                             (zero? (rem s-types-count t-types-count)))
                      [(cs-gen-list V X Y s-types (gen-repeat (/ s-types-count
                                                                 t-types-count)
                                                              t-types)
                                    {} opts)]
                      (fail! S T)))

                  ; repeat on left, rest on right
                  (and (:repeat S)
                       (:rest T))
                  (let [s-types (:types S)
                        t-types (:types T)
                        s-types-count (count s-types)
                        t-types-count (count t-types)]
                    (if (>= s-types-count t-types-count)
                      [(cs-gen-list V X Y s-types (concat t-types
                                                          (repeat (- s-types-count
                                                                     t-types-count)
                                                                  (:rest T)))
                                    {} opts)]
                      (err/nyi-error (pr-str "NYI HSequential inference " S T) opts)))

                  ; repeat on left, drest on right
                  (and (:repeat S)
                       (:drest T))
                  (let [{t-dty :pre-type dbound :name} (:drest T)
                        _ (when-not (Y dbound)
                            (fail! S T))
                        merged-X (assoc X dbound (homogeneous-dbound->bound (Y dbound) opts))
                        get-list-of-c (fn get-list-of-c [S-list]
                                        (mapv #(get-c-from-cmap (cs-gen V merged-X Y % t-dty opts) dbound opts)
                                              S-list))
                        repeat-c (get-list-of-c (:types S))]
                    [(assoc-in (cr/empty-cset X Y) [:maps 0 :dmap :map dbound] (cr/dcon-repeat-maker [] repeat-c))])

                  ;; dotted on the left, nothing on the right
                  (and (:drest S)
                       (not ((some-fn :rest :drest :repeat) T)))
                  (let [{dty :pre-type dbound :name} (:drest S)]
                    (when-not (Y dbound)
                      (fail! S T))
                    (when-not (<= (count (:types S)) (count (:types T)))
                      (fail! S T))
                    (let [vars (var-store-take dbound dty (- (count (:types T))
                                                             (count (:types S)))
                                               opts)
                          new-tys (doall (for [var vars]
                                           (subst/substitute (r/make-F var) dbound dty opts)))
                          new-s-hsequential (r/-hsequential (concat (:types S) new-tys) {} opts)
                          new-cset (cs-gen-HSequential V 
                                                       ;move dotted lower/upper bounds to vars
                                                       (merge X (zipmap vars (repeat (homogeneous-dbound->bound (Y dbound) opts)))) Y new-s-hsequential T opts)]
                      [(move-vars-to-dmap new-cset dbound vars opts)]))

                  ;; dotted on the right, nothing on the left
                  (and (not ((some-fn :rest :drest :repeat) S))
                       (:drest T))
                  (let [{dty :pre-type dbound :name} (:drest T)]
                    (when-not (Y dbound)
                      (fail! S T))
                    (when-not (<= (count (:types T)) (count (:types S)))
                      (fail! S T))
                    (let [vars (var-store-take dbound dty (- (count (:types S)) (count (:types T))) opts)
                          new-tys (doall
                                    (for [var vars]
                                      (subst/substitute (r/make-F var) dbound dty opts)))
                          new-t-hsequential (r/-hsequential (concat (:types T) new-tys) {} opts)
                          new-cset (cs-gen-HSequential V 
                                                       ;move dotted lower/upper bounds to vars
                                                       (merge X (zipmap vars (repeat (homogeneous-dbound->bound (Y dbound) opts)))) Y S new-t-hsequential opts)]
                      [(move-vars-to-dmap new-cset dbound vars opts)]))

                  ;TODO cases
                  :else (err/nyi-error (pr-str "NYI HSequential inference " S T) opts))
                (map (fn [fs1 fs2]
                       (cs-gen-filter-set V X Y fs1 fs2 opts))
                     (:fs S) (:fs T))
                (map (fn [o1 o2]
                       (cs-gen-object V X Y o1 o2))
                     (:objects S) (:objects T)))
              opts))

;; FIXME - anything else to say about And and OrFilters?
(t/ann cs-gen-filter [NoMentions ConstrainVars ConstrainDVars fr/Filter fr/Filter t/Any
                      -> cset])
(defn cs-gen-filter [V X Y s t opts]
  {:pre [((con/set-c? symbol?) V)
         (X? X)
         (Y? Y)
         (fr/Filter? s)
         (fr/Filter? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    (fr/TopFilter? t) (cr/empty-cset X Y)

    (and (fr/TypeFilter? s)
         (fr/TypeFilter? t)
         (and (= (:path s) (:path t))
              (= (:id s) (:id t))))
    (cset-meet (cs-gen V X Y (:type s) (:type t) opts)
               (cs-gen V X Y (:type t) (:type s) opts)
               opts)

    (and (fr/NotTypeFilter? s)
         (fr/NotTypeFilter? t)
         (and (= (:path s) (:path t))
              (= (:id s) (:id t))))
    (cset-meet (cs-gen V X Y (:type s) (:type t) opts)
               (cs-gen V X Y (:type t) (:type s) opts)
               opts)

    ; simple case for unifying x and y in (& (is x sym) ...) (is y sym)
;    (and (fr/AndFilter? s)
;         (fr/TypeFilter? t)
;         (every? fo/atomic-filter? (:fs s))
;         (= 1 (count (filter fr/TypeFilter? (:fs s)))))
;    (let [tf (first (filter fr/TypeFilter? (:fs s)))]
;      (cs-gen-filter V X Y tf t opts))
    :else (fail! s t)))

;must be *latent* filter sets
(t/ann cs-gen-filter-set [NoMentions ConstrainVars ConstrainDVars fr/Filter fr/Filter t/Any
                          -> cset])
(defn cs-gen-filter-set [V X Y s t opts]
  {:pre [((con/set-c? symbol?) V)
         (X? X)
         (Y? Y)
         (fr/FilterSet? s)
         (fr/FilterSet? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    :else
    (let [{s+ :then s- :else} s
          {t+ :then t- :else} t]
      (cset-meet (cs-gen-filter V X Y s+ t+ opts)
                 (cs-gen-filter V X Y s- t- opts)
                 opts))))

(t/ann cs-gen-object [NoMentions ConstrainVars ConstrainDVars
                      or/RObject or/RObject -> cset])
(defn cs-gen-object [V X Y s t]
  {:pre [((con/set-c? symbol?) V)
         (X? X)
         (Y? Y)
         (or/RObject? s)
         (or/RObject? t)]
   :post [(cr/cset? %)]}
  (cond
    (= s t) (cr/empty-cset X Y)
    (or/EmptyObject? t) (cr/empty-cset X Y)
    ;;FIXME do something here
    :else (fail! s t)))

(declare cs-gen-list-with-variances)

(defn cs-gen-TApp
  [V X Y S T opts]
  {:pre [(r/TApp? S)
         (r/TApp? T)
         (= (:rator S) (:rator T))]}
  (let [tfn (-> T :rator (c/fully-resolve-type opts))]
    (assert (r/TypeFn? tfn) "Found something other than a TFn in a TApp")
    (cs-gen-list-with-variances 
      V X Y
      (:variances tfn)
      (:rands S) (:rands T)
      opts)))

(defn cs-gen-FnIntersection
  [V X Y S T opts]
  {:pre [(r/FnIntersection? S)
         (r/FnIntersection? T)]}
  ;(prn "cs-gen FnIntersections")
  (cset-meet*
   (mapv
    (fn [t-arr]
      ;; for each t-arr, we need to get at least s-arr that works
      (let [results
            (into [] (keep #(handle-failure (cs-gen-Function V X Y % t-arr opts)))
                  (:types S))
            ;;_ (prn "results" (count results))
            ;;_ (clojure.pprint/pprint results)
            ;;_ (flush)
            ;; ensure that something produces a constraint set
            _ (when (empty? results)
                (fail! S T))
            comb (cset-combine results)]
        ;; (prn "combined" comb)
        comb))
    (:types T))
   opts))

(defn cs-gen-Result
  [V X Y S T opts]
  {:pre [(r/Result? S)
         (r/Result? T)]}
  (cset-meet* [(cs-gen V X Y (r/Result-type* S) (r/Result-type* T) opts)
               (cs-gen-filter-set V X Y (r/Result-filter* S) (r/Result-filter* T) opts)
               (cs-gen-object V X Y (r/Result-object* S) (r/Result-object* T))]
              opts))

(t/ann cs-gen-datatypes-or-records
       [NoMentions ConstrainVars ConstrainDVars DataType DataType t/Any -> cset])
(defn cs-gen-datatypes-or-records 
  [V X Y S T opts]
  {:pre [(every? r/DataType? [S T])]}
  (when-not (= (:the-class S) (:the-class T)) 
    (fail! S T))
  (if (seq (:poly? S))
    (cs-gen-list-with-variances V X Y (:variances T) (:poly? S) (:poly? T) opts)
    (cr/empty-cset X Y)))

; constrain si and ti according to variance
(t/ann cs-gen-with-variance [NoMentions ConstrainVars ConstrainDVars r/Variance
                             r/AnyType r/AnyType t/Any -> cset])
(defn cs-gen-with-variance
  [V X Y variance si ti opts]
  {:pre [(r/variance? variance)
         (r/AnyType? si)
         (r/AnyType? ti)]
   :post [(cr/cset? %)]}
  (case variance
    (:covariant :constant) (cs-gen V X Y si ti opts)
    :contravariant (cs-gen V X Y ti si opts)
    :invariant (cset-meet (cs-gen V X Y si ti opts)
                          (cs-gen V X Y ti si opts)
                          opts)))

;constrain lists of types ss and ts according to variances
(t/ann cs-gen-list-with-variances 
       [NoMentions ConstrainVars ConstrainDVars (t/Seqable r/Variance)
        (t/Seqable r/AnyType) (t/Seqable r/AnyType) t/Any -> cset])
(defn cs-gen-list-with-variances
  [V X Y variances ss ts opts]
  {:pre [(every? r/variance? variances)
         (every? r/AnyType? ss)
         (every? r/AnyType? ts)
         (apply = (count variances) (map count [ss ts]))]
   :post [(cr/cset? %)]}
  ;(prn :cs-gen-list-with-variances ss ts)
  (cset-meet*
    (cons (cr/empty-cset X Y)
          (map (t/fn [variance :- r/Variance
                      si :- r/AnyType 
                      ti :- r/AnyType]
                 (cs-gen-with-variance V X Y variance si ti opts))
               variances ss ts))
    opts))

(defn cs-gen-RClass
  [V X Y S T opts]
  {:pre [((some-fn r/RClass? r/Instance?) S)
         ((some-fn r/RClass? r/Instance?) T)]}
  (let [relevant-S (or (when (= (:the-class S)
                                (:the-class T))
                         S)
                       (some #(when (and ((some-fn r/RClass? r/Instance?) %)
                                         (= (:the-class %) (:the-class T)))
                                %)
                             (map #(c/fully-resolve-type % opts) (c/RClass-supers* S opts))))]
    ;(prn "relevant-S" relevant-S)
    ;(prn "T" T)
    ;(when relevant-S
    ; (prn "relevant-S" (prs/unparse-type relevant-S opts)))
    (cond
      ((every-pred r/RClass? r/RClass?) relevant-S T)
      (cs-gen-list-with-variances 
        V X Y
        (:variances T)
        (:poly? relevant-S)
        (:poly? T)
        opts)

      ((every-pred r/Instance?) relevant-S T)
      (cr/empty-cset X Y)

      :else (fail! S T))))

(defn cs-gen-Protocol
  [V X Y S T opts]
  {:pre [(r/Protocol? S)
         (r/Protocol? T)]}
  (t/ann-form [S T] (t/Seqable Protocol))
  (if (= (:the-var S)
         (:the-var T))
    (cset-meet*
      (cons (cr/empty-cset X Y)
            (map (fn [vari si ti]
                   (case vari
                     (:covariant :constant) (cs-gen V X Y si ti opts)
                     :contravariant (cs-gen V X Y ti si opts)
                     :invariant (cset-meet (cs-gen V X Y si ti opts)
                                           (cs-gen V X Y ti si opts)
                                           opts)))
                 (:variances T)
                 (:poly? S)
                 (:poly? T)))
      opts)
    (fail! S T)))

(t/ann demote-F [NoMentions ConstrainVars ConstrainDVars F r/Type t/Any -> cset])
(defn demote-F [V X Y {:keys [name] :as S} T opts]
  {:pre [(r/F? S)]}
  ;constrain T to be below S (but don't mention V)
  (assert (contains? X name) (str X name))
  (when (and (r/F? T)
             (free-ops/free-in-scope (:name T)))
    (fail! S T))
  (let [dt (prmt/demote-var T V opts)
        bnd (X name)
        _ (assert bnd)]
    (-> (cr/empty-cset X Y)
      (insert-constraint name (r/Bottom) dt bnd))))

(t/ann promote-F [NoMentions ConstrainVars ConstrainDVars r/Type F -> cset])
(defn promote-F [V X Y S {:keys [name] :as T} opts]
  {:pre [(r/F? T)]}
  ;T is an F
  ;S is any Type
  ;constrain T to be above S (but don't mention V)
  (assert (contains? X name) (str X T))
  (when (and (r/F? S)
             (free-ops/free-in-scope (:name S)))
    (fail! S T))
  (let [ps (prmt/promote-var S V opts)
        bnd (X name)
        _ (assert bnd)]
    (-> (cr/empty-cset X Y)
      (insert-constraint name ps r/-any bnd))))

(t/ann cs-gen-left-F [NoMentions ConstrainVars ConstrainDVars F r/Type t/Any -> cset])
(defn cs-gen-left-F [V X Y S T opts]
  {:pre [(r/F? S)]}
  (cond
    (contains? X (:name S))
    (demote-F V X Y S T opts)

    (and (r/F? T)
         (contains? X (:name T)))
    (promote-F V X Y S T opts)

    :else (fail! S T)))

(t/ann cs-gen-right-F [NoMentions ConstrainVars ConstrainDVars r/Type F t/Any -> cset])
(defn cs-gen-right-F [V X Y S T opts]
  {:pre [(r/F? T)]}
  (cond
    (contains? X (:name T))
    (promote-F V X Y S T opts)

    (and (r/F? S)
         (contains? X (:name S)))
    (demote-F V X Y S T opts)

    :else (fail! S T)))

(t/ann singleton-dmap [t/Sym cr/DCon -> dmap])
(defn singleton-dmap [dbound dcon]
  (cr/dmap-maker {dbound dcon}))

(t/ann mover [cset t/Sym (t/Seqable t/Sym) [cr/CMap cr/DMap -> cr/DCon] -> cset])
(defn mover [cset dbound vars f opts]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cr/cset? %)]}
  (cr/cset-maker
    (map
      (fn [{cmap :fixed dmap :dmap}]
        (cr/cset-entry-maker
          (apply dissoc cmap dbound vars)
          (dmap-meet 
            (singleton-dmap 
              dbound
              (f cmap dmap))
            (cr/dmap-maker (dissoc (:map dmap) dbound))
            opts)))
      (:maps cset))))

;; dbound : index variable
;; cset : the constraints being manipulated
;FIXME needs no-check for unreachable flow filter error
(t/ann ^:no-check move-rest-to-dmap [cset t/Sym (t/HMap :optional {:exact (t/U nil true)}) t/Any -> cset])
(defn move-rest-to-dmap [cset dbound {:keys [exact]} opts]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         ((some-fn nil? true?) exact)]
   :post [(cr/cset? %)]}
  (mover cset dbound nil
         (fn [cmap dmap]
           ((if exact cr/dcon-exact-maker cr/dcon-maker)
              nil
              (or (cmap dbound)
                  (err/int-error (str "No constraint for bound " dbound) opts))))
         opts))

; FIXME why we need a list of cset-entry?
(t/ann get-c-from-cmap [cset t/Sym t/Any -> c])
(defn get-c-from-cmap [cset dbound opts]
  {:pre [(cr/cset? cset)
         (symbol? dbound)]
   :post [(cr/c? %)]}
  (or ((-> cset :maps first :fixed) dbound)
      (err/int-error (str "No constraint for bound " dbound) opts)))

;; dbound : index variable
;; vars : listof[type variable] - temporary variables
;; cset : the constraints being manipulated
;; takes the constraints on vars and creates a dmap entry constraining dbound to be |vars|
;; with the constraints that cset places on vars
(t/ann move-vars-to-dmap [cset t/Sym (t/Seqable t/Sym) t/Any -> cset])
;FIXME no-check, flow error
(defn ^:no-check move-vars-to-dmap [cset dbound vars opts]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cr/cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           (cr/dcon-maker (mapv (fn [v]
                                  (or (cmap v)
                                      (err/int-error (str "No constraint for new var " v) opts)))
                                vars)
                          nil))
         opts))

;; This one's weird, because the way we set it up, the rest is already in the dmap.
;; This is because we create all the vars, then recall cgen/arr with the new vars
;; in place, and the "simple" case will then call move-rest-to-dmap.  This means
;; we need to extract that result from the dmap and merge it with the fixed vars
;; we now handled.  So I've extended the mover to give access to the dmap, which we use here.
;FIXME no-check because of unreachable flow 
(t/ann ^:no-check move-vars+rest-to-dmap
       [cset t/Sym (t/Seqable t/Sym) (t/HMap :optional {:exact (t/U nil true)}) t/Any -> cset])
(defn move-vars+rest-to-dmap [cset dbound vars {:keys [exact]} opts]
  {:pre [(cr/cset? cset)
         (symbol? dbound)
         (every? symbol? vars)
         ((some-fn nil? true?) exact)]
   :post [(cr/cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           ((if exact cr/dcon-exact-maker cr/dcon-maker)
            (mapv (fn [v]
                    (or (cmap v)
                        (err/int-error (str "No constraint for new var " v) opts)))
                  vars)
            (if-some [c ((:map dmap) dbound)]
              (cond
                (and (cr/dcon? c)
                     (not (:fixed c))) (:rest c)
                (and (cr/dcon-exact? c)
                     (not (:fixed c))) (:rest c)
                :else (err/int-error (str "did not a get a rest-only dcon when moving to the dmap") opts))
              (err/int-error (str "No constraint for bound " dbound) opts))))
         opts))

;; Maps dotted vars (combined with dotted types, to ensure global uniqueness)
;; to "fresh" symbols.
;; That way, we can share the same "fresh" variables between the elements of a
;; cset if they're talking about the same dotted variable.
;; This makes it possible to reduce the size of the csets, since we can detect
;; identical elements that would otherwise differ only by these fresh vars.
;; The domain of this map is pairs (var . dotted-type).
;; The range is this map is a list of symbols generated on demand, as we need
;; more dots.

;; Take (generate as needed) n symbols that correspond to variable var used in
;; the context of type t.
;FIXME no-check, trans-dots needs to be generalised
(t/ann ^:no-check var-store-take [t/Sym r/Type t/Int t/Any -> (t/Seqable t/Sym)])
(defn- var-store-take [var t n {::keys [dotted-var-store] :as opts}]
  (assert dotted-var-store (keys opts))
  ;(t/ann-form dotted-var-store (t/Atom (t/Map '[r/Type t/Sym] (t/Seq t/Sym))))
  (let [key [t n]
        res (@dotted-var-store key)]
    (if (>= (count res) n)
      ;; there are enough symbols already, take n
      (take n res)
      ;; we need to generate more
      (let [new (repeatedly (- n (count res)) #(gensym var))
            all (concat res new)]
        (swap! dotted-var-store assoc key all)
        all))))

(defn cs-gen-Function-just-rests [V X Y S T opts]
  {:pre [(every? #(#{:fixed :rest} (:kind %)) [S T])]}
  (let [arg-mapping (cond
                      ;both rest args are present, so make them the same length
                      (and (:rest S) (:rest T))
                      (let [max-fixed (max (count (:dom S))
                                           (count (:dom T)))]
                        (cs-gen-list V X Y 
                                     (cons (:rest T) (u/pad-right max-fixed (:dom T) (:rest T)))
                                     (cons (:rest S) (u/pad-right max-fixed (:dom S) (:rest S)))
                                     {} opts))
                      ;rest arg only on left, and left fixed args can be padded to fit right fixed
                      (and (:rest S)
                           (not (:rest T))
                           (<= (count (:dom S))
                               (count (:dom T))))
                      (let [new-S (u/pad-right (count (:dom T)) (:dom S) (:rest S))]
                        ;(prn "infer rest arg on left")
                        ;(prn "left dom" (map #(prs/unparse-type % opts) (:dom S)))
                        ;(prn "right dom" (map #(prs/unparse-type % opts) (:dom T)))
                        ;(prn "new left dom" (map #(prs/unparse-type % opts) new-S))
                        (cs-gen-list V X Y (:dom T) new-S
                                     {} opts))
                      ;no rest arg on left, or wrong number = fail
                      :else (fail! S T))
        ret-mapping (cs-gen V X Y (:rng S) (:rng T) opts)]
    (cset-meet* [arg-mapping ret-mapping] opts)))

;;FIXME unused
#_
(defn cs-gen-Function-rest-prest [cg V X Y S T opts]
  {:pre [(:rest S)
         (:prest T)]}
  (let [S-dom (:dom S)
        S-dom-count (count S-dom)
        T-dom (:dom T)
        T-dom-count (count T-dom)
        S-rest (:rest S)
        T-prest-types (-> T :prest :types)
        T-prest-types-count (count T-prest-types)
        ret-mapping (cg (:rng S) (:rng T))
        rest-prest-mapping (cs-gen-list V X Y T-prest-types (repeat T-prest-types-count S-rest)
                                        {} opts)]
    (if (> S-dom-count T-dom-count)
      ; hard mode
      (let [[S-dom-short S-dom-rest] (split-at T-dom-count S-dom)
            ;_ (println "in hard-mode of rest prest")
            arg-mapping (cs-gen-list V X Y T-dom S-dom-short {} opts)
            remain-repeat-count (rem (count S-dom-rest) T-prest-types-count)
            ceiling (fn [up low]
                      {:pre [(every? integer? [up low])]}
                      (let [result (quot up low)]
                        (if (zero? (rem up low))
                          result
                          (inc result))))
            repeat-times (if (empty? S-dom-rest)
                           0
                           (ceiling (count S-dom-rest) T-prest-types-count))
            repeat-mapping (cs-gen-list V X Y
                                        (concat S-dom-rest
                                                (repeat remain-repeat-count S-rest))
                                        (gen-repeat repeat-times T-prest-types)
                                        {} opts)]
        (cset-meet* [arg-mapping repeat-mapping rest-prest-mapping ret-mapping] opts))
      ; easy mode
      (let [[T-dom-short T-dom-rest] (split-at S-dom-count T-dom)
            arg-mapping (cs-gen-list V X Y T-dom-short S-dom {} opts)
            rest-mapping (cs-gen-list V X Y T-dom-rest (repeat (count T-dom-rest) S-rest) {} opts)]
        (cset-meet* [arg-mapping rest-mapping rest-prest-mapping ret-mapping] opts)))))

(defn cs-gen-Function-prest-on-left [V X Y S T opts]
  ; prest on left, nothing on right
  {:pre [(:prest S)
         (= :fixed (:kind T))]}
  (let [s-dom (:dom S)
        t-dom (:dom T)
        s-dom-count (count s-dom)
        t-dom-count (count t-dom)]
    (if (and (<= t-dom-count s-dom-count)
             (not (zero? (rem (- t-dom-count s-dom-count)
                              (count (-> S :prest :types))))))
      (fail! S T)
      (let [[short-T rest-T] (split-at s-dom-count t-dom)
            short-cs (cs-gen-list V X Y short-T s-dom {} opts)
            s-prest-types (-> S :prest :types)
            rest-S (gen-repeat (/ (count rest-T) (count s-prest-types)) s-prest-types)
            rest-cs (cs-gen-list V X Y rest-T rest-S {} opts)
            ret-mapping (cs-gen V X Y (:rng S) (:rng T) opts)]
        (cset-meet* [short-cs rest-cs ret-mapping] opts)))))

(defn cs-gen-Function-prest-drest [cg V X Y S T opts]
  {:pre [(:prest S)
         (:drest T)]}
  (let [{t-dty :pre-type dbound :name} (:drest T)
        _ (when-not (Y dbound)
            (fail! S T))
        S-dom (:dom S)
        S-dom-count (count S-dom)
        T-dom (:dom T)
        T-dom-count (count T-dom)
        S-prest-types (-> S :prest :types)
        S-prest-types-count (count S-prest-types)
        merged-X (assoc X dbound (homogeneous-dbound->bound (Y dbound) opts))
        get-list-of-c (fn [S-list]
                        (mapv #(get-c-from-cmap (cs-gen V merged-X Y t-dty % opts) dbound opts)
                              S-list))
        repeat-c (get-list-of-c S-prest-types)
        ret-mapping (cg (:rng S) (:rng T))]
    (if (<= S-dom-count T-dom-count)
      ; hard mode
      (let [T-rest-count (- T-dom-count S-dom-count)
            [arg-S-prest remain-S-prest] (split-at (rem T-rest-count
                                                        S-prest-types-count) S-prest-types)
            new-S (concat S-dom
                          (gen-repeat (quot T-rest-count S-prest-types-count) S-prest-types)
                          arg-S-prest)
            arg-mapping (cs-gen-list V X Y T-dom new-S {} opts)
            fixed-c (if (zero? (count arg-S-prest))
                      []
                      (get-list-of-c remain-S-prest))
            darg-mapping (assoc-in (cr/empty-cset X Y)
                                   [:maps 0 :dmap :map dbound]
                                   (cr/dcon-repeat-maker fixed-c repeat-c))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping] opts))
      ; easy mode
      (let [[arg-S rest-S] (split-at T-dom-count S-dom)
            arg-mapping (cs-gen-list V X Y T-dom arg-S {} opts)
            fixed-c (get-list-of-c rest-S)
            darg-mapping (assoc-in (cr/empty-cset X Y)
                                   [:maps 0 :dmap :map dbound]
                                   (cr/dcon-repeat-maker fixed-c repeat-c))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping] opts)))))

(defn cs-gen-Function-dotted-left-nothing-right [V X Y S T opts]
  {:pre [(:drest S)
         (= :fixed (:kind T))]}
  (let [{dty :pre-type dbound :name} (:drest S)]
    (when-not (Y dbound)
      (fail! S T))
    (when-not (<= (count (:dom S)) (count (:dom T)))
      (fail! S T))
    (let [vars (var-store-take dbound dty (- (count (:dom T))
                                             (count (:dom S)))
                               opts)
          new-tys (mapv (fn [var]
                          (subst/substitute (r/make-F var) dbound dty opts))
                        vars)
          new-s-arr (r/make-Function (into (:dom S) new-tys) (:rng S))
          new-cset (cs-gen-Function V
                                    ;move dotted lower/upper bounds to vars
                                    (merge X (zipmap vars (repeat (homogeneous-dbound->bound (Y dbound) opts)))) Y new-s-arr T opts)]
      (move-vars-to-dmap new-cset dbound vars opts))))

(defn cs-gen-Function-dotted-right-nothing-left [V X Y S T opts]
  {:pre [(= :fixed (:kind S))
         (:drest T)]}
  (let [{dty :pre-type dbound :name} (:drest T)]
    (when-not (Y dbound)
      (fail! S T))
    (when-not (<= (count (:dom T)) (count (:dom S)))
      (fail! S T))
    (let [vars (var-store-take dbound dty (- (count (:dom S)) (count (:dom T))) opts)
          new-tys (mapv
                    (fn [var]
                      (subst/substitute (r/make-F var) dbound dty opts))
                    vars)
          ;_ (prn "dotted on the right, nothing on the left")
          ;_ (prn "vars" vars)
          new-t-arr (r/make-Function (into (:dom T) new-tys) (:rng T))
          ;_ (prn "S" (prs/unparse-type S opts))
          ;_ (prn "new-t-arr" (prs/unparse-type new-t-arr opts))
          new-cset (cs-gen-Function V
                                    ;move dotted lower/upper bounds to vars
                                    (merge X (zipmap vars (repeat (homogeneous-dbound->bound (Y dbound) opts)))) Y S new-t-arr opts)]
      (move-vars-to-dmap new-cset dbound vars opts))))

;; * <: ...
(defn cs-gen-Function-star-<-dots [cg V X Y S T opts]
  {:pre [(:rest S)
         (:drest T)]}
  (let [{t-dty :pre-type dbound :name} (-> T :drest)]
    (when-not (Y dbound)
      (fail! S T))
    (if (<= (count (:dom S)) (count (:dom T)))
      ;; the simple case
      (let [arg-mapping (cs-gen-list V X Y (:dom T) (u/pad-right (count (:dom T)) (:dom S) (:rest S)) {} opts)
            darg-mapping (move-rest-to-dmap (cs-gen V (assoc X dbound (homogeneous-dbound->bound (Y dbound) opts)) Y t-dty (:rest S) opts) dbound {} opts)
            ret-mapping (cg (:rng S) (:rng T))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping] opts))
      ;; the hard case
      (let [vars (var-store-take dbound t-dty (- (count (:dom S)) (count (:dom T))) opts)
            new-tys (mapv (fn [var]
                            (subst/substitute (r/make-F var) dbound t-dty opts))
                          vars)
            new-t-arr (r/make-Function (into (:dom T) new-tys) (:rng T) :drest (r/DottedPretype1-maker t-dty dbound))
            new-cset (cs-gen-Function V (merge X (zipmap vars (repeat (homogeneous-dbound->bound (Y dbound) opts))) X) Y S new-t-arr opts)]
        (move-vars+rest-to-dmap new-cset dbound vars {} opts)))))

;; ... <: *
; Typed Racket notes that this might not be a correct subtyping case?
(defn cs-gen-Function-dots-<-star [cg V X Y S T opts]
  {:pre [(:drest S)
         (:rest T)]}
  (let [{s-dty :pre-type dbound :name} (-> S :drest)]
    (when-not (Y dbound)
      (fail! S T))
    (cond 
      (< (count (:dom S)) (count (:dom T)))
      ;; the hard case
      (let [vars (var-store-take dbound s-dty (- (count (:dom T)) (count (:dom S))) opts)
            new-tys (mapv (fn [var]
                            (subst/substitute (r/make-F var) dbound s-dty opts))
                          vars)
            new-s-arr (r/make-Function (into (:dom S) new-tys) (:rng S) :drest (r/DottedPretype1-maker s-dty dbound))
            new-cset (cs-gen-Function V (merge X (zipmap vars (repeat (homogeneous-dbound->bound (Y dbound) opts))) X) Y new-s-arr T opts)]
        (move-vars+rest-to-dmap new-cset dbound vars {:exact true} opts))

      (= (count (:dom S)) (count (:dom T)))
      ;the simple case
      (let [arg-mapping (cs-gen-list V X Y (u/pad-right (count (:dom S)) (:dom T) (:rest T)) (:dom S) {} opts)
            darg-mapping (move-rest-to-dmap (cs-gen V (assoc X dbound (homogeneous-dbound->bound (Y dbound) opts)) Y (:rest T) s-dty opts) dbound {:exact true} opts)
            ret-mapping (cg (:rng S) (:rng T))]
        (cset-meet* [arg-mapping darg-mapping ret-mapping] opts))

      :else (fail! S T))))

(t/ann ^:no-check cs-gen-Function
       [NoMentions ConstrainVars ConstrainDVars Function Function t/Any -> cset])
(defn cs-gen-Function
  [V X Y S T opts]
  {:pre [((con/set-c? simple-symbol?) V)
         (X? X)
         (Y? Y)
         (r/Function? S)
         (r/Function? T)]
   :post [(cr/cset? %)]}
  ;(prn "cs-gen-Function" (prs/unparse-type S opts) (prs/unparse-type T opts))
  (letfn> [cg :- [r/AnyType r/AnyType -> cset]
           (cg [S T] (cs-gen V X Y S T opts))]
    (cond
      ;easy case - no rests, drests, kws, prest
      ((every-pred #(= :fixed (:kind %))) S T)
      ; contravariant
      (cset-meet* [(cs-gen-list V X Y (:dom T) (:dom S) {} opts)
                   ; covariant
                   (cg (:rng S) (:rng T))]
                  opts)

      ;just a rest args on one or more sides
      ((every-pred #(#{:fixed :rest} (:kind %))) S T)
      (cs-gen-Function-just-rests V X Y S T opts)

      ; :rest is less restricted than :prest
      (and (:prest S)
           (:rest T)
           (> (-> S :prest :types count) 1))
      (fail! S T)

      (and (:rest S)
           (:prest T))
      (cs-gen-Function-just-rests V X Y S T opts)

      ; prest on left, nothing on right
      (and (:prest S)
           (= :fixed (:kind T)))
      (cs-gen-Function-prest-on-left V X Y S T opts)

      ; prest on left, drest on right
      (and (:prest S)
           (:drest T))
      (cs-gen-Function-prest-drest cg V X Y S T opts)

      ;; dotted on the left, nothing on the right
      (and (:drest S)
           (= :fixed (:kind T)))
      (cs-gen-Function-dotted-left-nothing-right V X Y S T opts)

      ;; dotted on the right, nothing on the left
      (and (= :fixed (:kind S))
           (:drest T))
      (cs-gen-Function-dotted-right-nothing-left V X Y S T opts)

      ;; * <: ...
      (and (:rest S)
           (:drest T))
      (cs-gen-Function-star-<-dots cg V X Y S T opts)

      ;; ... <: *
      ; Typed Racket notes that this might not be a correct subtyping case?
      (and (:drest S)
           (:rest T))
      (cs-gen-Function-dots-<-star cg V X Y S T opts)

      :else 
      (err/nyi-error (str "NYI Function inference " (pr-str (prs/unparse-type S opts)) " " (pr-str (prs/unparse-type T opts))) opts))))

;; C : cset? - set of constraints found by the inference engine
;; Y : (setof symbol?) - index variables that must have entries
;; R : Type? - result type into which we will be substituting
;TODO no-check, very slow!
(t/ann ^:no-check subst-gen [cset (t/Set t/Sym) r/AnyType (t/HMap :optional {:T (t/Seqable r/Type) :flip-T-variances? t/Bool}) t/Any -> (t/U nil cr/SubstMap)])
(defn subst-gen [C Y R {:keys [T flip-T-variances?]} opts]
  {:pre [(cr/cset? C)
         ((con/set-c? symbol?) Y)
         (r/AnyType? R)
         (every? r/Type? T)]
   :post [((some-fn nil? cr/substitution-c?) %)]}
  (let [var-hash (apply frees/combine-frees
                        (frees/fv-variances R opts)
                        (map (cond->> #(frees/fv-variances % opts)
                               ;; FIXME this is necessary for symbolic closure inference but
                               ;; breaks some tests. should be default behavior.
                               flip-T-variances? (comp frees/flip-variance-map))
                             T))
        ;_ (prn :subst-gen :var-hash var-hash)
        ;;FIXME consider (flipped) variances in T
        idx-hash (frees/idx-variances R opts)]
    (letfn> 
           [;; v : Symbol - variable for which to check variance
            ;; h : (Hash F Variance) - hash to check variance in (either var or idx hash)
            ;; variable: Symbol - variable to use instead, if v was a temp var for idx extension
            constraint->type :- [c frees/VarianceMap & :optional {:variable (t/U nil t/Sym)} -> r/Variance]
            (constraint->type [{{:keys [upper-bound lower-bound]} :bnds :keys [S X T] :as v} h & {:keys [variable]}]
              {:pre [(cr/c? v)
                     (frees/variance-map? h)
                     ((some-fn nil? symbol?) variable)]}
              (when-not (subtype? S T opts) (fail! S T))
              (when (some r/TypeFn? [upper-bound lower-bound]) (err/nyi-error "Higher kinds" opts))
              (let [X (or variable X)
                    var (h X :constant)
                    ;_ (prn "variance" X var)
                    ;; Pierce+Turner section 3.5
                    inferred (case var
                               (:constant :covariant) S
                               :contravariant T
                               ;; Colored LTI suggests S
                               :invariant (c/In [S T] opts))]
                ;(prn "inferred" inferred)
                inferred))
            ;TODO implement generalize
            ;                  (let [gS (generalize S)]
            ;                    (if (subtype? gS T opts)
            ;                      gS
            ;                      S))

            ;; Since we don't add entries to the empty cset for index variables (since there is no
            ;; widest constraint, due to dcon-exacts), we must add substitutions here if no constraint
            ;; was found.  If we're at this point and had no other constraints, then adding the
            ;; equivalent of the constraint (dcon null (c Bot X Top)) is okay.
            extend-idxs :- [cr/SubstMap -> (t/U nil cr/SubstMap)]
            (extend-idxs [S]
              {:pre [(cr/substitution-c? S)]}
              (let [fi-R (frees/fi R opts)] ;free indices in R
                ;; If the index variable v is not used in the type, then
                ;; we allow it to be replaced with the empty list of types;
                ;; otherwise we error, as we do not yet know what an appropriate
                ;; lower bound is.
                (letfn> [demote-check-free :- [t/Sym -> cr/SubstRHS]
                         (demote-check-free [v]
                           {:pre [(symbol? v)]}
                           (if (fi-R v)
                             (err/int-error "attempted to demote dotted variable" opts)
                             (cr/i-subst-maker nil)))]
                  ;; absent-entries is false if there's an error in the substitution, otherwise
                  ;; it's a list of variables that don't appear in the substitution
                  (let [absent-entries
                        (reduce (t/fn [no-entry :- t/Any
                                       v :- t/Sym]
                                  {:pre [(symbol? v)]}
                                  (let [entry (S v)]
                                    ;; Make sure we got a subst entry for an index var
                                    ;; (i.e. a list of types for the fixed portion
                                    ;;  and a type for the starred portion)
                                    (cond
                                      (not entry) (cons v no-entry)
                                      (or (cr/i-subst? entry)
                                          (cr/i-subst-starred? entry)
                                          (cr/i-subst-dotted? entry)) no-entry
                                      :else (reduced false))))
                                [] Y)]
                    (and absent-entries
                         (-> (into {}
                                   (map
                                     (fn [missing]
                                       (let [var (idx-hash missing :constant)]
                                         [missing
                                          (case var
                                            (:constant :covariant :invariant) (demote-check-free missing)
                                            :contravariant (cr/i-subst-starred-maker nil r/-any))])))
                                   absent-entries)
                             (into S)))))))]

      (let [{cmap :fixed dmap* :dmap} (or (-> C :maps first)
                                          (err/int-error "No constraints found" opts))
            ; Typed Racket arbitrarily picks the first constraint here, we follow.
            ;
            ;_ (when-not (= 1 (count (:maps C))) 
            ;    (err/int-error "More than one constraint set found" opts))
            dm (:map dmap*)
            subst (as-> {} subst
                    (reduce-kv (fn [subst k dc]
                                 (let [fixed (mapv #(constraint->type % idx-hash :variable k)
                                                   (:fixed dc))]
                                   (assoc subst k
                                          (cond
                                            (and (cr/dcon? dc) (not (:rest dc)))
                                            (cr/i-subst-maker fixed)

                                            (and (cr/dcon? dc) (:rest dc))
                                            (cr/i-subst-starred-maker fixed (constraint->type (:rest dc) idx-hash))

                                            (cr/dcon-exact? dc)
                                            (cr/i-subst-starred-maker fixed (constraint->type (:rest dc) idx-hash))

                                            (cr/dcon-dotted? dc)
                                            (cr/i-subst-dotted-maker
                                              fixed
                                              (constraint->type (:dc dc) idx-hash :variable k)
                                              (:dbound dc))

                                            (cr/dcon-repeat? dc)
                                            (cr/i-subst-maker fixed)

                                            :else (err/int-error (prn-str "What is this? " dc) opts)))))
                               subst dm)
                    (reduce-kv (fn [subst k v]
                                 (assoc subst k (cr/t-subst-maker
                                                  (constraint->type v var-hash)
                                                  (:bnds v))))
                               subst cmap))
            ;check delayed constraints and type variable bounds
            _ (let [t-substs (into {} (filter (t/fn [[_ v] :- '[t/Sym cr/SubstRHS]]
                                                (cr/t-subst? v)))
                                   subst)
                    s (seq t-substs)
                    names (map first s)
                    images (map (comp :type second) s)]
                (doseq [[nme {inferred :type :keys [bnds]}] t-substs]
                  (when (some r/TypeFn? [(:upper-bound bnds) (:lower-bound bnds)]) (err/nyi-error "Higher kinds" opts))
                  (let [lower-bound (subst/substitute-many (:lower-bound bnds) images names opts)
                        upper-bound (subst/substitute-many (:upper-bound bnds) images names opts)]
                    (cond
                      (not (subtype? lower-bound upper-bound opts))
                      (fail! lower-bound upper-bound)

                      (not (subtype? inferred upper-bound opts))
                      (fail! inferred upper-bound)

                      (not (subtype? lower-bound inferred opts))
                      (fail! lower-bound inferred)))))]
        ;; verify that we got all the important variables
        (when (every? #(some-> (subst %) cr/t-subst?)
                      (frees/fv R opts))
          (extend-idxs subst))))))

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained mapped to their bounds
;; Y : the set of index variables to be constrained mapped to their bounds
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; expected-cset : a cset representing the expected type, to meet early and
;;  keep the number of constraints in check. (empty by default)
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(t/ann cs-gen-list
       [NoMentions ConstrainVars ConstrainDVars 
        (t/Seqable r/Type) (t/Seqable r/Type)
        (t/HMap :optional {:expected-cset (t/U nil cset)})
        t/Any
        -> cset])
(defn cs-gen-list [V X Y S T {:keys [expected-cset] :or {expected-cset (cr/empty-cset {} {})}} opts]
  {:pre [((con/set-c? symbol?) V)
         (X? X)
         (Y? Y)
         (every? r/Type? (concat S T))
         (cr/cset? expected-cset)]
   :post [(cr/cset? %)]}
  ;(prn "cs-gen-list" 
  ;     V X Y
  ;     (map #(prs/unparse-type % opts) S)
  ;     (map #(prs/unparse-type % opts) T))
  (when-not (= (count S) (count T))
    (fail! S T))
  (cset-meet*
    ;; We meet early to prune the csets to a reasonable size.
    ;; This weakens the inference a bit, but sometimes avoids
    ;; constraint explosion.
    (cons
      (cr/empty-cset X Y)
      (map (fn [s t]
             (let [c (cs-gen V X Y s t opts)
                   ;_ (prn "csgen-list 1")
                   ;_ (prn "V" V)
                   ;_ (prn "X" X)
                   ;_ (prn "Y" Y)
                   ;_ (prn "s" (prs/unparse-type s opts))
                   ;_ (prn "t" (prs/unparse-type t opts))
                   ;_ (prn "c" c)
                   ;_ (prn "expected cset" expected-cset)
                   m (cset-meet c expected-cset opts)]
               ;(prn "meet:")
               ;(prn m)
               ;(flush)
               m))
           S T))
    opts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Infer

(f/def-derived-fold IHardRange
  hard-range?*
  [hard?])

(f/add-fold-case IHardRange hard-range?* GetType (fn [t hard?] (vreset! hard? true) t))
(f/add-fold-case IHardRange hard-range?* MergeType (fn [t hard?] (vreset! hard? true) t))
(f/add-fold-case IHardRange hard-range?* AssocType (fn [t hard?] (vreset! hard? true) t))
(f/add-fold-case IHardRange hard-range?* MatchType (fn [t hard?] (vreset! hard? true) t))

(defn hard-range? [t opts]
  (let [hard? (volatile! false)]
    (call-hard-range?* t opts {:hard? hard?})
    @hard?))

(f/def-derived-fold IHardFnIntersection
  hard-FnIntersection?*
  [hard?])

(f/add-fold-case IHardFnIntersection hard-FnIntersection?* GetType (fn [t hard?] (vreset! hard? true) t))
(f/add-fold-case IHardFnIntersection hard-FnIntersection?* MergeType (fn [t hard?] (vreset! hard? true) t))
(f/add-fold-case IHardFnIntersection hard-FnIntersection?* AssocType (fn [t hard?] (vreset! hard? true) t))

(defn hard-FnIntersection? [t opts]
  (and (r/FnIntersection? t)
       (let [hard? (volatile! false)]
         (call-hard-FnIntersection?* t opts {:hard? hard?})
         @hard?)))

;; returns a non-empty set of argument positions whose checking should be deferred (symbolic closures and polymorphic functions).
;; if none, returns nil. :rng is the range position.
(defn get-deferred-fixed-args [arg-types dom R expected opts]
  {:pre [(r/AnyType? R)
         ((some-fn nil? r/AnyType?) expected)]}
  (let [defer? (fn [mode s t]
                 (let [s (c/fully-resolve-type s opts)
                       t (c/fully-resolve-type t opts)]
                   (case mode
                     :arg (or (and r/enable-symbolic-closures?
                                   (r/SymbolicClosure? s)
                                   (inferrable-symbolic-closure-expected-type? t opts))
                              (or (and ((some-fn r/Poly? r/PolyDots? r/Value?) s)
                                       (or (and (r/FnIntersection? t)
                                                (= 1 (count (:types t)))
                                                ;; something can potentially be run through check-funapp
                                                ;; after instantiating dotted variables, i.e., turns into :fixed
                                                (not-any? (comp #{:rest :kws :prest :regex} :kind) (:types t)))
                                           #_(printf "get-deferred-fixed-args not deferring poly %s because expected is: %s\n" (pr-str s) (pr-str t))))
                                   (hard-FnIntersection? t opts)))
                     :rng (hard-range? s opts))))]
    (-> #{}
        (cond-> (some->> expected (defer? :rng R)) (conj :rng))
        (into (keep-indexed (fn [i arg-t]
                              (when (defer? :arg arg-t (nth dom i))
                                i)))
              arg-types)
        not-empty)))

(defn prep-symbolic-closure-expected-type [substitution-without-symb dom-t opts]
  {:pre [(cr/substitution-c? substitution-without-symb)
         (r/Type? #_inferrable-symbolic-closure-expected-type? dom-t)]
   :post [(r/Type? #_inferrable-symbolic-closure-expected-type? %)]}
  ;(prn :prep-symbolic-closure-expected-type dom-t)
  (let [var-hash (frees/fv+idx-variances dom-t opts)
        subst (reduce-kv (fn [subst k v]
                           (let [variance (var-hash k)]
                             (cond-> subst
                               variance (assoc k (case variance
                                                   :covariant (cond
                                                                (cr/t-subst? v) (assoc v :type r/-wild)
                                                                :else v)
                                                   (:invariant :contravariant) v)))))
                         {} substitution-without-symb)]
    (subst/subst-all subst dom-t opts)))

(f/def-derived-fold IRenameDots rename-dots* [rename])

(f/add-fold-case
  IRenameDots rename-dots*
  DottedPretype
  (fn [t rename]
    (-> t
        (update :name #(rename % %)))))

(defn rename-dots [t rename opts]
  (call-rename-dots* t opts {:rename rename}))

(f/def-derived-fold ISeparateF separate-F* [replace-fs remap-atom idx-context in-idx-context])

(f/add-fold-case
  ISeparateF separate-F*
  F
  (fn [t replace-fs remap-atom idx-context _]
    (if-not (contains? (:fv replace-fs) (:name t))
      t
      (let [t' (-> t
                   (update :name gensym))]
        (swap! remap-atom update-in
               (-> (if idx-context
                     [:idx-context idx-context]
                     [:fv])
                   (conj (:name t)))
               (fnil conj []) (:name t'))
        t'))))

(f/add-fold-case
  ISeparateF separate-F*
  DottedPretype
  (fn [{:keys [name] :as t} replace-fs remap-atom _ in-idx-context]
    (if-not (contains? (:idx replace-fs) (:name t))
      t
      (let [name' (gensym name)
            t' (-> t
                   (update :pre-type in-idx-context name name')
                   (assoc :name name'))]
        (swap! remap-atom update-in [:idx name] (fnil conj []) name')
        t'))))

(defn separate-F [t replace-fs opts]
  {:pre [(r/AnyType? t)
         ((con/hmap-c? (con/optional :fv) (con/set-c? simple-symbol?)
                       (con/optional :idx) (con/set-c? simple-symbol?))
          replace-fs)]
   :post [(r/AnyType? (:separated-t %))
          (map? (:remap %))]}
  (let [remap-atom (atom {})
        separated-t (letfn [(rec [t replace-fs idx-context]
                              (call-separate-F* t opts
                                                {:in-idx-context (partial in-idx-context replace-fs idx-context)
                                                 :idx-context idx-context
                                                 :replace-fs replace-fs
                                                 :remap-atom remap-atom}))
                            (in-idx-context [replace-fs idx-context t idx idx']
                              (rec t
                                   (-> replace-fs
                                       ;; scope dotted as normal var
                                       (update :fv (fnil conj #{}) idx))
                                   (conj (or idx-context []) idx')))]
                      (rec t replace-fs nil))
        remap @remap-atom
        separated-fv->original (reduce (fn [separated-fv->original [original separateds]]
                                         (reduce #(assoc %1 %2 original) separated-fv->original separateds))
                                       {} (apply concat (:fv remap) (:idx remap)
                                                 (vals (:idx-context remap))))
        separated-fv->original-idx-context (reduce-kv (fn [separated-fv->original-idx-context idx-context original->separateds]
                                                        (reduce #(assoc %1 %2 (mapv separated-fv->original idx-context)) separated-fv->original-idx-context
                                                                (mapcat identity (vals original->separateds))))
                                                      {} (:idx-context remap))
        {free-variances :frees idx-variances :idxs} (frees/free-variances separated-t opts)
        ;_ (prn "remap" remap)
        ;_ (prn "separated-t" separated-t)
        ;_ (prn "separated-fv->original" separated-fv->original)
        ;_ (prn "separated-fv->original-idx-context" separated-fv->original-idx-context)
        ;_ (prn "free-variances" free-variances)
        ;_ (prn "idx-variances" idx-variances)
        ]
    {:separated-t separated-t :remap remap
     :separated-fv->original separated-fv->original
     :separated-fv->original-idx-context separated-fv->original-idx-context
     :free-variances free-variances
     :idx-variances idx-variances}))

;; apply substitution in argument positions, replace range positions with r/-infer-top
;; TODO what if range is [x :-> x], probably want to subst the lhs with the actual x rather than r/-infer-top
(defn prep-symbolic-closure-expected-type2 [subst t opts]
  {:pre [(cr/substitution-c? subst)
         (inferrable-symbolic-closure-expected-type? t opts)]
   :post [(r/Type? %)]}
  ;(prn "prep-symbolic-closure-expected-type2" t)
  ;(prn "subst" subst)
  (let [t (c/fully-resolve-type t opts)]
    (cond
      (r/FnIntersection? t) (-> t
                                (update :types (fn [types]
                                                 (mapv (fn [{:keys [rng] :as f}]
                                                         {:pre [(r/Function? f)]
                                                          :post (r/Function? %)}
                                                         (let [rng-var-hash (frees/fv+idx-variances rng opts)
                                                               ;_ (prn "rng-var-hash" rng-var-hash rng)
                                                               rng-subst (reduce-kv (fn [subst k v]
                                                                                      (let [variance (rng-var-hash k)]
                                                                                        ;(prn "variance" k variance)
                                                                                        (cond-> subst
                                                                                          variance (assoc k (case variance
                                                                                                              :covariant (cond
                                                                                                                           (cr/t-subst? v) (assoc v :type r/-wild)
                                                                                                                           :else v)
                                                                                                              (:invariant :contravariant) v)))))
                                                                                    {} subst)]
                                                           (-> f
                                                               (assoc :rng (r/make-Result r/-any))
                                                               (as-> t (subst/subst-all subst t opts))
                                                               (assoc :rng (subst/subst-all rng-subst rng opts)))))
                                                       types))))
      (r/Poly? t) (let [names (c/Poly-fresh-symbols* t)
                        bbnds (c/Poly-bbnds* names t opts)
                        body (c/Poly-body* names t opts)]
                   (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
                     (c/Poly* names bbnds (prep-symbolic-closure-expected-type2 subst body opts) opts)))
      (r/PolyDots? t) (let [names (c/PolyDots-fresh-symbols* t)
                            bbnds (c/PolyDots-bbnds* names t opts)
                            body  (c/PolyDots-body* names t opts)]
                        (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
                          (c/PolyDots* names bbnds (prep-symbolic-closure-expected-type2 subst body opts) opts)))
      :else (do ;(prn "unsupported type in prep-symbolic-closure-expected-type: " t)
                (fail! nil nil)))))

;; replaces covariant type variables with r/-wild and then applies subst
(defn prep-symbolic-closure-expected-type3 [subst t opts]
  ;(prn :prep-symbolic-closure-expected-type3 subst t)
  (let [replace-fs (reduce-kv (fn [replace-fs sym sbst]
                                (update replace-fs (if (cr/t-subst? sbst) :fv :idx) (fnil conj #{}) sym))
                              {} subst)
        {:keys [separated-t remap separated-fv->original separated-fv->original-idx-context
                free-variances idx-variances]} (separate-F t replace-fs opts)

        subst-infer-covariant (reduce-kv (fn [subst-infer-covariant sym variance]
                                           (cond-> subst-infer-covariant
                                             (= :covariant variance)
                                             (assoc sym r/-wild)))
                                         {} free-variances)]
    ;(prn "subst-infer-covariant" subst-infer-covariant)
    ;(prn "separated-t" separated-t)
    (-> separated-t
        ;; replace covariant occurrences of variables with wildcards
        ;; TODO what do we do with covariant dotted variables?
        (subst/substitute-many (vals subst-infer-covariant) (keys subst-infer-covariant) opts)
        ;; rename all other variables back to original
        (subst/substitute-many (mapv r/make-F (vals separated-fv->original)) (keys separated-fv->original) opts)
        (rename-dots separated-fv->original opts)
        ;; perform original substitution
        ;(doto (->> (println "before subst" (with-out-str (clojure.pprint/pprint subst)))))
        (as-> t (subst/subst-all subst t opts))
        #_(doto (->> (prn "after subst"))))))

;; apply symbolic closure arg-t using function type dom-t as expected type, which is selectively
;; instantiated using substitution-without-symb, a substitution yielded from constraint
;; generation on all arguments except the symbolic closure argument.
(defn app-symbolic-closure [substitution-without-symb arg-t dom-t {::check/keys [check-expr] :as opts}]
  {:pre [(cr/substitution-c? substitution-without-symb)
         (r/SymbolicClosure? arg-t)
         (r/Type? #_inferrable-symbolic-closure-expected-type? dom-t)]
   :post [(r/AnyType? %)]}
  (with-bindings (:bindings arg-t)
    (let [delayed-errors (err/-init-delayed-errors)
          expected (r/ret (prep-symbolic-closure-expected-type3 substitution-without-symb dom-t opts))
          ;_ (prn :app-symbolic-closure expected)
          res (-> (check-expr
                    (:fexpr arg-t)
                    expected
                    ;;TODO add symbolic closure's lexical scope from (:opts arg-t)
                    (assoc opts ::vs/delayed-errors delayed-errors))
                  u/expr-type r/ret-t)]
      (when-some [errs (seq @delayed-errors)]
        #_
        (prn "symbolic closure failed to check"
             errs)
        ;; move to next arity, symbolic closure failed to check
        (fail! nil nil))
      ;(prn :arg-t arg-t)
      ;(prn :res res)
      res)))

;; substitute all variables in non-covariant positions (and all index variables for now)
(defn subst-non-covariant [subst t opts]
  {:pre [(cr/substitution-c? subst)
         (r/AnyType? t)]
   :post (r/AnyType? %)}
  (let [replace-fs (reduce-kv (fn [replace-fs sym sbst]
                                (update replace-fs (if (cr/t-subst? sbst) :fv :idx) (fnil conj #{}) sym))
                              {} subst)
        {:keys [separated-t remap free-variances idx-variances separated-fv->original]} (separate-F t replace-fs opts)
        non-covariant-frees (into {} (remove (comp #{:covariant} val)) free-variances)
        non-covariant-idxs (into {} (remove (comp #{:covariant} val)) idx-variances)]
    (-> separated-t
        ;; rename non-covariant variables back to original. ensures i-subst's substitute correctly, since
        ;; the scoped variable in the pretype is correctly scopes (a :.. a instead of a1 :.. a2).
        (subst/substitute-many (mapv (comp r/make-F separated-fv->original) (keys non-covariant-frees))
                               (keys non-covariant-frees) opts)
        (rename-dots (select-keys separated-fv->original (keys non-covariant-idxs)) opts)
        ;; substitute non-covariant variables. covariant variables are effectively fresh, thus ignored here.
        (as-> t (subst/subst-all subst t opts))
        ;; rename all other variables back to original
        (subst/substitute-many (mapv r/make-F (vals separated-fv->original)) (keys separated-fv->original) opts)
        (rename-dots separated-fv->original opts))))

(declare cs-gen-app)

(t/ann cs-gen-list+symbolic
       [(t/Set t/Int) ConstrainVars ConstrainDVars 
        (t/Vec r/Type) (t/Vec r/Type)
        cset t/Any
        -> (t/U cset SymbolicClosure)])
(defn cs-gen-list+symbolic [deferred-fixed-args X Y S T R expected expected-cset expr opts]
  {:pre [((some-fn nil? (con/set-c? (some-fn nat-int? #{:rng}))) deferred-fixed-args)
         (if (:rng deferred-fixed-args)
           expected
           true)
         (X? X)
         (Y? Y)
         ((every-pred (con/vec-c? r/AnyType?)) S T)
         ((some-fn nil? r/AnyType?) expected)
         (cr/cset? expected-cset)
         (r/AnyType? R)
         ((some-fn nil? :op) expr)]
   :post [((some-fn cr/cset? r/SymbolicClosure?) %)]}
  ;(prn :cs-gen-list+symbolic deferred-fixed-args)
  (when-not (= (count S) (count T))
    (fail! S T))
  (let [cs-gen-args #(cs-gen-list #{} X Y % T {:expected-cset expected-cset} opts)]
    (if-not deferred-fixed-args
      (cs-gen-args S)
      ;we have symbolic closures provided as args. infer constraints from other args
      ;then use them to check the symbolic closures (and infer more constraints from their return types).
      (let [;_ (prn :cs-gen-list+symbolic)
            S-no-symb (keep-indexed (fn [i t] (when-not (deferred-fixed-args i) t)) S)
            T-no-symb (keep-indexed (fn [i t] (when-not (deferred-fixed-args i) t)) T)
            T-symb (keep-indexed (fn [i t] (when (deferred-fixed-args i) t)) T)
            cs-no-symb (-> (cs-gen-app X Y S-no-symb T-no-symb R (when-not (deferred-fixed-args :rng) expected) opts)
                           (cset-meet expected-cset opts))
            symb-fv-variances (cond-> (frees/flip-variance-map (apply frees/combine-frees (map #(frees/fv-variances % opts) T-symb)))
                                (deferred-fixed-args :rng) (frees/combine-frees (frees/fv-variances R opts)))
            ;; a tvar must appear both co+contravariantly in order for iterating to potentially give new information:
            ;; previous iteration can learn input information from a contravariant position that is useful for an output covariant
            ;; position next iteration.
            iterate? (some #{:invariant} (vals symb-fv-variances))
            ;_ (prn :expected-cset expected-cset)
            ;_ (prn :cs-no-symb cs-no-symb)
            substitution-without-symb (subst-gen cs-no-symb (set (keys Y)) R {:T T :flip-T-variances? true} opts)
            ;_ (prn "substitution-without-symb" substitution-without-symb)
            add-symb-to-S (fn [inferred-deferred-arg-types] (reduce-kv assoc S inferred-deferred-arg-types))
            infer-deferred-arg-types (fn [subst]
                                       (reduce (fn [ts i]
                                                 (assoc ts i
                                                        (let [s (c/fully-resolve-type
                                                                  (case i
                                                                    :rng R
                                                                    (nth S i))
                                                                  opts)
                                                              t (c/fully-resolve-type
                                                                  (case i
                                                                    :rng expected
                                                                    (nth T i))
                                                                  opts)]
                                                          ;(prn "infer-deferred-arg-types" i s t)
                                                          (cond
                                                            (and (not= :rng i)
                                                                 (r/SymbolicClosure? s))
                                                            (app-symbolic-closure subst s t opts)

                                                            (and (not= :rng i)
                                                                 (r/FnIntersection? t)
                                                                 (= 1 (count (:types t))))
                                                            (let [delayed-errors (err/-init-delayed-errors)
                                                                  opts (assoc opts ::vs/delayed-errors delayed-errors)]
                                                              ;(prn "Deferred Poly <: " s t)
                                                              (let [t (prep-symbolic-closure-expected-type3 subst t opts)
                                                                    ;_ (prn "t" t)
                                                                    _ (when-not (and (r/FnIntersection? t)
                                                                                     (= 1 (count (:types t)))
                                                                                     ;; this might be too restrictive
                                                                                     (every? #(= :fixed (:kind %)) (:types t)))
                                                                        ;(prn "need :fixed" t)
                                                                        (fail! nil nil))
                                                                    inferred-rng (r/TCResult->Result
                                                                                   ((requiring-resolve 'typed.cljc.checker.check.funapp/check-funapp)
                                                                                    nil nil
                                                                                    (r/ret s)
                                                                                    (mapv r/ret (-> t :types first :dom))
                                                                                    (-> t :types first :rng r/Result->TCResult)
                                                                                    {} opts))
                                                                    ;_ (prn "after check-funapp" inferred-rng)
                                                                    _ (when-some [errs (seq @delayed-errors)]
                                                                        #_
                                                                        (prn "deferred Poly argument failed to check"
                                                                             errs)
                                                                        ;; move to next arity, symbolic closure failed to check
                                                                        (fail! nil nil))]
                                                                ;(prn "inferred-rng" inferred-rng)
                                                                (assoc-in t [:types 0 :rng] inferred-rng)))

                                                            (= :rng i)
                                                            (let [s (subst/subst-all subst s opts)]
                                                              ;(prn ":rng" s :< t)
                                                              (if (sub/subtype? s t opts)
                                                                s
                                                                (fail! s t)))

                                                            :else (err/int-error (str "Unsupported deferred argument type: " (prs/unparse-type s opts)
                                                                                      " " (prs/unparse-type t opts))
                                                                                 opts)))))
                                               {} deferred-fixed-args))
            inferred-deferred-arg-types (infer-deferred-arg-types substitution-without-symb)
            ;_ (prn "inferred-deferred-arg-types" inferred-deferred-arg-types iterate?)
            ;_ (prn "iterate?" (boolean iterate?))
            inferred-deferred-arg-types (if iterate?
                                          (loop [fuel 21
                                                 cs cs-no-symb
                                                 inferred-deferred-arg-types inferred-deferred-arg-types]
                                            ;(prn "fuel" fuel)
                                            (let [subst (subst-gen cs (set (keys Y)) R {:T T :flip-T-variances? true} opts)
                                                  ;_ (prn "subst" subst)
                                                  S-symb' (vals (into (sorted-map) (dissoc inferred-deferred-arg-types :rng)))
                                                  T-symb' (mapv #(subst-non-covariant subst % opts) T-symb)
                                                  ;_ (prn "S-symb'" S-symb')
                                                  ;_ (binding [vs/*verbose-types* true] (prn "T-symb" T-symb))
                                                  ;_ (binding [vs/*verbose-types* true] (prn "T-symb'" T-symb'))
                                                  ;_ (prn "deferred-fixed-args" deferred-fixed-args)
                                                  cs (-> (cs-gen-list #{} X Y S-symb' T-symb' {} opts)
                                                         (cond->
                                                           (deferred-fixed-args :rng)
                                                           ;; perhaps substituting non-contravariant occurrences instead?
                                                           (cset-meet (let [inst-rng (subst/subst-all subst R opts)]
                                                                        ;(prn "inst-rng" inst-rng)
                                                                        (cs-gen #{} X Y inst-rng expected opts))
                                                                      opts))
                                                         (cset-meet cs opts))
                                                  ;_ (prn "cs" cs)
                                                  substitution-with-symb (subst-gen cs (set (keys Y)) R {:T T :flip-T-variances? true} opts)
                                                  ;_ (prn "substitution-with-symb" substitution-with-symb)
                                                  inferred-deferred-arg-types' (infer-deferred-arg-types substitution-with-symb)
                                                  ;_ (prn "inferred-deferred-arg-types" inferred-deferred-arg-types)
                                                  ;_ (prn "inferred-deferred-arg-types'" inferred-deferred-arg-types')
                                                  ;;TODO could save one extra iteration by instead checking the assignments of covariant variables.
                                                  no-new-information? (every? (fn [[i s]]
                                                                                (let [t (inferred-deferred-arg-types' i)]
                                                                                  (and (sub/subtype? s t opts)
                                                                                       (sub/subtype? t s opts))))
                                                                              inferred-deferred-arg-types)]
                                              (if no-new-information?
                                                inferred-deferred-arg-types'
                                                (if (zero? fuel)
                                                  (do ;(prn "fuel zero")
                                                      (fail! nil nil))
                                                  (recur (dec fuel) cs inferred-deferred-arg-types')))))
                                          inferred-deferred-arg-types)
            arg-types-with-inferred-symb (add-symb-to-S (dissoc inferred-deferred-arg-types :rng))
            ;;seems important to recompute all constraints for all arguments again here, not sure why.
            ;; I would think only the symbolic closure args are needed.
            cs (cs-gen-args arg-types-with-inferred-symb)]
        ;(prn :arg-types-with-inferred-symb arg-types-with-inferred-symb)
        (if (and expr
                 ;; TODO if R has no type variables in contravariant position, I don't
                 ;; think we need to suspend the type check
                 ((some-fn r/FnIntersection? r/Poly? r/PolyDots?) (c/fully-resolve-type R opts))
                 (or (not expected)
                     (r/wild? (c/fully-resolve-type expected opts))))
          ;; wait for an expected type to help inference
          (let [subst (subst-gen cs (set (keys Y)) R {:T T} opts)
                smallest (subst/subst-all subst R opts)]
            ;;TODO record and reuse constraints
            (r/symbolic-closure expr smallest opts))
          cs)))))

;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(t/ann infer-dots
       [ConstrainVars 
        t/Sym 
        Regex
        (t/Seqable r/Type) 
        (t/Seqable r/Type)
        r/Type 
        (t/U nil r/AnyType) 
        (t/Set t/Sym)
        (t/HMap :optional {:expected (t/U nil r/Type)
                           :expr (t/U nil (t/Map t/Any t/Any))})
        t/Any
        -> cr/SubstMap])
(defn infer-dots [X dotted-var dotted-bnd S T T-dotted R must-vars {:keys [expected expr]} opts]
  {:pre [(X? X)
         (symbol? dotted-var)
         (r/Regex? dotted-bnd)
         (every? (con/every-c? r/Type?) [S T])
         (r/Type? T-dotted) 
         (r/AnyType? R)
         ((con/set-c? symbol?) must-vars)
         ((some-fn nil? r/Type?) expected)
         ((some-fn nil? :op) expr)]
   :post [(or (cr/substitution-c? %)
              (r/SymbolicClosure? %))]}
  ;(prn :infer-dots S T)
  (let [T (vec T)
        [short-S rest-S] (map vec (split-at (count T) S))
        new-vars (var-store-take dotted-var T-dotted (count rest-S) opts)
        new-Ts (mapv (fn [v]
                       (let [target (subst/substitute-dots (map r/make-F new-vars) nil dotted-var T-dotted opts)]
                         #_(prn "replace" v "with" dotted-var "in" (prs/unparse-type target opts))
                         (subst/substitute (r/make-F v) dotted-var target opts)))
                     new-vars)
        ;; allow 1 symbolic closure in non-dotted arg
        ;; strategy: infer all other non-dotted args plus the dotted args, then use the
        ;; resulting substitution to instantiate any dotted types in the symbolic closure's
        ;; expected type. finally, add any constraints inferred from checking the symbolic closure
        ;; the final substitution.
        ;; e.g., (map #(+ %1 %2) [1 2 3] [4 5 6])
        ;; map : (All [a c b :..] [[a b :.. b :-> c] (Seqable a) (Seqable b) :.. b :-> (Seq c)])
        ;; (infer [1 2 3] (Seqable a)) => {a Int}
        ;; (infer [4 5 6] (Seqable b0)) => {b0 Int} :dotted {b [b0]}
        ;; (infer #(+ %1 %2) [Int Int :-> ^:infer Any]) => {c Int}
        short-deferred-fixed-args (when-not (get-deferred-fixed-args rest-S new-Ts R nil opts)
                                    (get-deferred-fixed-args short-S T R expected opts))
        expected-cset (if (and expected (not (:rng short-deferred-fixed-args)))
                        (cs-gen #{} X {dotted-var dotted-bnd} R expected opts)
                        (cr/empty-cset {} {}))
        cs-dotted (-> (cs-gen-list #{} (reduce #(assoc %1 %2 (homogeneous-dbound->bound dotted-bnd opts)) X new-vars)
                                   {dotted-var dotted-bnd} rest-S new-Ts
                                   {:expected-cset expected-cset} opts)
                      (move-vars-to-dmap dotted-var new-vars opts))
        expected-cset+dotted (cset-meet expected-cset cs-dotted opts)
        ;_ (prn :expected-cset+dotted expected-cset+dotted)
        cs (cs-gen-list+symbolic short-deferred-fixed-args
                                 X {dotted-var dotted-bnd} short-S T R expected
                                 expected-cset+dotted expr
                                 opts)]
    ;(prn :cs cs)
    ;; FIXME pass variances via :T
    (cond-> cs
      (not (r/SymbolicClosure? cs))
      (-> (cset-meet expected-cset opts)
          (subst-gen #{dotted-var} R {} opts)))))

(declare infer)

;; like infer-dot, but for infering pdot function
;; T-dotted is the one with `:repeat true`
;; FIXME dotted-bnd seems not that useful, cause it can't constrain on serveral pretypes
;; on pdot
;; TODO support symbolic closures, accept :expr
(t/ann infer-pdot
       [ConstrainVars
        t/Sym
        Regex
        (t/Seqable r/Type)
        (t/Seqable r/Type)
        r/Type
        (t/U nil r/AnyType)
        (t/Set t/Sym)
        (t/HMap :optional {:expected (t/U nil r/Type)})
        t/Any
        -> cr/SubstMap])
(defn infer-pdot [X dotted-var dotted-bnd S T T-dotted R must-vars {:keys [expected]} opts]
  {:pre [(X? X)
         (symbol? dotted-var)
         (r/Regex? dotted-bnd)
         (every? (con/every-c? r/Type?) [S T])
         (and (r/Type? T-dotted) (:repeat T-dotted) (:types T-dotted))
         (r/AnyType? R)
         ((con/set-c? symbol?) must-vars)
         ((some-fn nil? r/Type?) expected)]
   :post [(cr/substitution-c? %)]}
  ;(prn :infer-pdot)
  (let [[short-S rest-S] (split-at (count T) S)
        _ (when-not (zero? (rem (- (count S) (count T))
                                (-> T-dotted :types count)))
            (fail! S T))
        ;_ (prn "short-S" (map #(prs/unparse-type % opts) short-S))
        ;_ (prn "T" (map #(prs/unparse-type % opts) T))
        ;_ (prn "rest-S" (map #(prs/unparse-type % opts) rest-S))
        ;_ (prn "R" R)
        ;_ (prn "expected" expected)
        expected-cset (if expected
                        (cs-gen #{} X {dotted-var dotted-bnd} R expected opts)
                        (cr/empty-cset {} {}))
        ;_ (prn "expected-cset" expected-cset)
        cs-short (cs-gen-list #{} X {dotted-var dotted-bnd} short-S T
                              {:expected-cset expected-cset} opts)
        ;_ (prn "cs-short" cs-short)
        new-vars (var-store-take dotted-var T-dotted (count rest-S) opts)
        new-Ts (doall
                 (let [list-of-vars (partition-by-nth (-> T-dotted :types count) new-vars)
                       ;_ (println "list-of-vars" list-of-vars)
                       list-of-result (map (fn [vars pre-type]
                                             (for [v vars]
                                               (let [target (subst/substitute-dots
                                                              (map r/make-F vars)
                                                              nil dotted-var pre-type opts)]
                                                 (subst/substitute (r/make-F v) dotted-var target opts))))
                                           list-of-vars
                                           (:types T-dotted))]
                   (apply interleave list-of-result)))
        cs-dotted (cs-gen-list #{} (reduce #(assoc %1 %2 dotted-bnd) X new-vars)
                               {dotted-var dotted-bnd} rest-S new-Ts
                               {:expected-cset expected-cset} opts)
        ;_ (prn "cs-dotted" cs-dotted)
        cs-dotted (move-vars-to-dmap cs-dotted dotted-var new-vars opts)
        ;_ (prn "cs-dotted" cs-dotted)
        cs (cset-meet cs-short cs-dotted opts)
        ;_ (prn "cs" cs)
        ]
    ;; FIXME pass variances via :T
    (subst-gen (cset-meet cs expected-cset opts) #{dotted-var} R {} opts)))

;; like infer-vararg, but T-var is the prest type:
(t/ann infer-prest
  [ConstrainVars ConstrainDVars
   (t/Seqable r/Type) (t/Seqable r/Type)
   r/Type (t/U nil r/AnyType) (t/U nil TCResult)
   t/Any
   -> (t/U nil true false cr/SubstMap)])
(defn infer-prest
  [X Y S T T-var R expected {:keys [expr]} opts]
  {:pre [(X? X)
         (Y? Y)
         (every? r/Type? S)
         (every? r/Type? T)
         (r/Type? T-var)
         (r/AnyType? R)
         ((some-fn nil? r/AnyType?) expected)]
   :post [(or (nil? %)
              (cr/substitution-c? %))]}
  ;(prn :infer-prest)
  #_(println "infer-prest\n"
           "X" X "\n"
           "Y" Y "\n"
           "S" S "\n"
           "T" T "\n"
           "T-var" T-var "\n"
           )
  (and (>= (count S) (count T))
       (let [[short-S rest-S] (split-at (count T) S)
             ; wrap rest-S into HeterogeneousVector, this is semantic meaning of <*
             new-rest-S (r/-hvec (vec rest-S) {} opts)
             new-S (concat short-S [new-rest-S])
             new-T (concat T [T-var])]
         (infer X Y new-S new-T R expected {:expr expr} opts))))

;; like infer, but T-var is the vararg type:
(t/ann infer-vararg
       [ConstrainVars ConstrainDVars 
        (t/Seqable r/Type)
        (t/Seqable r/Type)
        (t/U nil r/Type)
        (t/U nil r/AnyType)
        (t/alt (t/? (t/U nil TCResult))
               (t/cat (t/U nil TCResult)
                      (t/HMap :optional {:expr (t/U nil (t/Map t/Any t/Any))})))
        t/Any
        :-> (t/U nil true false cr/SubstMap)])
(defn infer-vararg
  ([X Y S T T-var R opts]          (infer-vararg X Y S T T-var R nil      {} opts))
  ([X Y S T T-var R expected opts] (infer-vararg X Y S T T-var R expected {} opts))
  ([X Y S T T-var R expected {:keys [expr]} opts]
   {:pre [(X? X)
          (Y? Y)
          (every? r/Type? S)
          (every? r/Type? T)
          ((some-fn nil? r/Type?) T-var)
          (r/AnyType? R)
          ((some-fn nil? r/AnyType?) expected)]
    :post [(or (nil? %)
               (cr/substitution-c? %)
               (r/SymbolicClosure? %))]}
   ;(prn :infer-vararg S T R expected)
   ;(prn :expected R expected)
   (when (>= (count S) (count T))
     (let [T (cond-> T
               ;Pad out T
               T-var (concat (repeat (- (count S) (count T)) T-var)))]
       (infer X Y S T R expected {:expr expr} opts)))))

(defn cs-gen-app
  "Constraints for a function application.
  X : variables to infer mapped to their bounds
  Y : indices to infer mapped to their bounds
  S : actual argument types
  T : formal argument types
  R : result type
  expected : nil or the expected type"
  ([X Y S T R opts] (cs-gen-app X Y S T R nil {} opts))
  ([X Y S T R expected opts] (cs-gen-app X Y S T R expected {} opts))
  ([X Y S T R expected {:keys [expr]} opts]
   {:pre [(X? X)
          (Y? Y)
          (every? r/Type? S)
          (every? r/Type? T)
          (r/AnyType? R)
          ((some-fn nil? r/AnyType?) expected)
          ((some-fn nil? :op) expr)]
    :post [((some-fn cr/cset? r/SymbolicClosure?) %)]}
   (when-not (= (count S) (count T))
     (fail! S T))
   (let [deferred (get-deferred-fixed-args S T R expected opts)
         expected-cset (if (and expected (not (:rng deferred)))
                         (cs-gen #{} X Y R expected opts)
                         (cr/empty-cset {} {}))
         cs (cs-gen-list+symbolic deferred X Y (vec S) (vec T) R expected expected-cset expr opts)]
     (cond-> cs
       (not (r/SymbolicClosure? cs)) (cset-meet expected-cset opts)))))

;; X : variables to infer mapped to their bounds
;; Y : indices to infer mapped to their bounds
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : nil or the expected type
;; returns a substitution
;; if R is nil, we don't care about the substituion
;; just return a boolean result
;; if :expr is provided, then return may be SymbolicClosure
(t/ann infer
       (t/IFn [ConstrainVars ConstrainDVars (t/Seqable r/Type) (t/Seqable r/Type) (t/Nilable r/AnyType) t/Any -> (t/U nil true cr/SubstMap SymbolicClosure)]
              [ConstrainVars ConstrainDVars (t/Seqable r/Type) (t/Seqable r/Type) (t/Nilable r/AnyType) (t/Nilable TCResult) t/Any -> (t/U nil true cr/SubstMap SymbolicClosure)]
              [ConstrainVars ConstrainDVars (t/Seqable r/Type) (t/Seqable r/Type) (t/Nilable r/AnyType) (t/Nilable TCResult) (t/HMap :optional {:expr t/Any}) t/Any ->
               (t/U nil true cr/SubstMap SymbolicClosure)]))
(defn infer
  ([X Y S T R opts]          (infer X Y S T R nil      {} opts))
  ([X Y S T R expected opts] (infer X Y S T R expected {} opts))
  ([X Y S T R expected {:keys [expr]} opts]
   {:pre [(X? X)
          (Y? Y)
          (every? r/Type? S)
          (every? r/Type? T)
          (r/AnyType? R)
          ((some-fn nil? r/AnyType?) expected)]
    :post [((some-fn nil? true? cr/substitution-c? r/SymbolicClosure?) %)
           (if (r/SymbolicClosure? %) expr true)]}
   ;(prn :infer S T R expected)
   (let [cs* (cs-gen-app X Y S T R expected {:expr expr} opts)
         res (if (r/SymbolicClosure? cs*)
               cs*
               (if R
                 (subst-gen cs* (set (keys Y)) R {:T T} opts)
                 true))]
     ;(prn :infer res)
     res)))

(defmacro unify-or-nil [{:keys [fresh out opts]} s t]
  (assert opts)
  (assert (every? simple-symbol? fresh))
  (assert (apply distinct? fresh))
  (assert (some #{out} fresh))
  (let [fresh-names (map (juxt identity gensym) fresh)
        name-lookup (into {} fresh-names)]
    `(let [opts# ~opts
           [lhs# rhs#] (let ~(into []
                                   (mapcat (fn [[fresh name]]
                                             [fresh `(r/make-F '~name)]))
                                   fresh-names)
                         [~s ~t])
           _# (assert (r/Type? lhs#))
           _# (assert (r/Type? rhs#))
           substitution#
           (handle-failure
             (infer
               (zipmap '~(map second fresh-names) (repeat r/no-bounds))
               {}
               [lhs#]
               [rhs#]
               r/-any
               opts#))
           res# (when substitution#
                  (when-let [s# (get substitution# '~(name-lookup out))]
                    (:type s#)))
           _# (assert ((some-fn nil? r/Type?) res#))]
       res#)))

(defn solve [t query opts]
  {:pre [(r/TCResult? t)
         (r/Type? query)]
   :post [((some-fn nil? r/TCResult?) %)]}
  ;(prn "solve" t query)
  (let [;; atm only support query = (All [x+] [in :-> out]) or [in :-> out]
        poly? (r/Poly? query)
        names (when poly? (c/Poly-fresh-symbols* query))
        bbnds (when poly? (c/Poly-bbnds* names query opts))
        body (if poly? (c/Poly-body* names query opts) query)
        _ (assert (r/FnIntersection? body) (class body))
        _ (assert (= 1 (count (:types body))))
        arity (first (:types body))
        _ (assert (r/Function? arity))
        _ (assert (= 1 (count (:dom arity))))
        _ (assert (= :fixed (:kind arity)))
        _ (assert (= (fo/-simple-filter) (:fl (:rng arity))))
        _ (assert (= or/-empty (:o (:rng arity))))

        lhs (:t t)
        rhs (first (:dom arity))
        ;; TODO incorporate filter/object
        out (:t (:rng arity))
        ;_ (prn {:lhs lhs :rhs rhs :out out})
        substitution (handle-failure
                       (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
                         (infer
                           (zipmap names bbnds)
                           {}
                           [lhs]
                           [rhs]
                           out
                           opts)))]
    (when substitution
      (r/ret (subst/subst-all substitution out opts)))))

(f/def-derived-fold IInferToTV wild->tv* [tvs-atom])

(f/add-fold-case
  IInferToTV wild->tv*
  Wildcard
  (fn [t tvs-atom]
    (let [sym (gensym 'tv)]
      (swap! tvs-atom conj sym)
      (r/F-maker sym))))

(defn wild->tv [t opts]
  {:pre [(r/AnyType? t)]
   :post [(-> % :t r/AnyType?)
          ((con/vec-c? simple-symbol?) (:tvs %))]}
  (let [tvs-atom (atom [])
        t (call-wild->tv* t opts {:tvs-atom tvs-atom})]
    {:t t :tvs @tvs-atom}))

(defn eliminate-wild
  "Assumes s <: t. If t has no wild's or if t fails to unify, returns t.

  Since this is used only in check-below, we need to be conservative about extra
  subtyping checks."
  [s t opts]
  {:pre [(r/AnyType? s)
         (r/AnyType? t)]
   :post [((some-fn nil? r/AnyType?) t)]}
  (when (subtype? s t opts)
    (let [{t-replaced :t :keys [tvs]} (wild->tv t opts)]
      (if (empty? tvs)
        t
        (:t (solve (r/ret s) (c/Poly* tvs (repeat (count tvs) r/no-bounds)
                                      (r/make-FnIntersection
                                        (r/make-Function [t-replaced] t-replaced))
                                      opts)
                   opts))))))

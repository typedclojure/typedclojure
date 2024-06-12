;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; support for assoc/merge/conj
(ns typed.clj.checker.assoc-utils
  (:require [typed.clojure :as t]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.free-ops :as free-ops]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.tvar-bnds :as bnds]
            [clojure.set :as set]
            [clojure.core.typed.current-impl :as impl])
  (:import (typed.cljc.checker.type_rep HeterogeneousMap Value Intersection F RClass DataType HSequential)
           (clojure.lang IPersistentMap IPersistentVector)))

;supporting assoc functionality

(declare assoc-type-pairs)

(defprotocol AssocableType
  (-assoc-pair [left kv opts]))

(extend-protocol AssocableType
  Intersection
  (-assoc-pair
    [old-i assoc-entry opts]
    ; attempt to simplify the intersection before recursing. Parsing an
    ; intersection type does not simplify it.
    (let [new-i (c/In (:types old-i) opts)]
      (if (r/Intersection? new-i)
        (c/In (keep #(assoc-type-pairs % [assoc-entry] opts) (:types new-i)) opts)
        (assoc-type-pairs new-i [assoc-entry] opts))))

  ; use the upper bound if bounds below (Map t/Any t/Any)
  F
  (-assoc-pair
    [{:keys [name] :as f} assoc-entry opts]
    (let [bnd (free-ops/free-with-name-bnds name)
          _ (when-not bnd
              (err/int-error (str "No bounds for type variable: " name bnds/*current-tvar-bnds*) opts))]
      (when (ind/subtype? (:upper-bound bnd)
                          (c/-name `t/Map r/-any r/-any)
                          opts)
        (r/AssocType-maker f [(mapv r/ret-t assoc-entry)] nil))))

  Value
  (-assoc-pair
   [v [kt vt :as assoc-entry] opts]
   (when (ind/subtype? v r/-nil opts)
     (let [rkt (-> kt :t (c/fully-resolve-type opts))]
       (if (c/keyword-value? rkt)
         (c/-complete-hmap {rkt (:t vt)} opts)
         (if (r/F? rkt)
           (r/AssocType-maker v [(mapv r/ret-t assoc-entry)] nil)
           (c/-name `t/Map rkt (r/ret-t vt)))))))
  
  ;; FIXME we need another interface (or extra params on IPersistentMap) for keys allowed
  ;; to be conj'ed onto types. Vectors and sorted maps need specific keys and records
  ;; need specific vals for some keys. These cases are currently unsound when assoc'ing
  ;; type variable targets/keys/vals. e.g., (fn :forall [M K] [m :- M k :- K] (assoc m k 1))
  ;; These cases should just match on one interface (this code models Associative/assoc)
  ;; and extract contravariant types from somewhere.
  RClass
  (-assoc-pair
   [rc [kt vt] opts]
   (let [_ (impl/assert-clojure opts)
         rkt (-> kt r/ret-t (c/fully-resolve-type opts))]
     (case (:the-class rc)
       clojure.lang.IPersistentMap
       (c/-name `t/Map
                (c/Un [(r/ret-t kt) (nth (:poly? rc) 0)] opts)
                (c/Un [(r/ret-t vt) (nth (:poly? rc) 1)] opts))

       clojure.lang.IPersistentVector
       (if (and (r/Value? rkt)
                (integer? (:val rkt)))
         (c/-name `t/Vec (c/Un [(r/ret-t vt) (nth (:poly? rc) 0)] opts))
         (when (ind/subtype? rkt (c/-name `t/Int) opts)
           (c/-name `t/Vec (c/Un [(r/ret-t vt) (nth (:poly? rc) 0)] opts))))
       nil)))
  
  HeterogeneousMap
  (-assoc-pair
   [hmap [kt vt] opts]
   (let [rkt (-> kt :t (c/fully-resolve-type opts))]
     (if (c/keyword-value? rkt)
       (c/make-HMap opts
                    {:mandatory (-> (:types hmap)
                                    (assoc rkt (:t vt)))
                     :optional (-> (:optional hmap)
                                   (dissoc (:t vt)))
                     :absent-keys (-> (:absent-keys hmap) 
                                      (disj rkt))
                     :complete? (c/complete-hmap? hmap)})
       (if (r/F? rkt)
         (r/AssocType-maker hmap [[rkt (r/ret-t vt)]] nil)
         (c/upcast-hmap hmap {:visit-ks-type #(c/Un [rkt %] opts)
                              :visit-vs-type #(c/Un [(r/ret-t vt) %] opts)
                              :elide-upper-count true}
                        opts)))))
  
  HSequential
  (-assoc-pair
   [v [kt vt] opts]
   (when (r/HeterogeneousVector? v)
     (let [rkt (-> kt :t (c/fully-resolve-type opts))]
       (when (ind/subtype? rkt (c/-name `t/Int) opts)
         (if (r/Value? rkt)
           (let [kt rkt
                 k (:val kt)]
             (when (and (integer? k) (<= k (count (:types v))))
               (r/-hvec (assoc (:types v) k (:t vt))
                        {:filters (assoc (:fs v) k (:fl vt))
                         :objects (assoc (:objects v) k (:o vt))}
                        opts)))
           (if (r/F? rkt)
             (r/AssocType-maker v [[rkt (r/ret-t vt)]] nil)
             (c/upcast-HSequential v {:visit-elem-type #(c/Un [% (r/ret-t vt)] opts)
                                      :elide-count true}
                                   opts)))))))
  
  DataType
  (-assoc-pair
   [dt [kt vt] opts]
   (let [rkt (-> kt :t (c/fully-resolve-type opts))]
     (when (and (r/Record? dt) (c/keyword-value? rkt))
       (let [kt rkt
             field-type (when (c/keyword-value? kt)
                          (get (:fields dt) (symbol (name (:val kt)))))]
         (when (and field-type (ind/subtype? (:t vt) field-type opts))
           dt))))))

(defn assoc-type-pairs [t pairs opts]
  {:pre [(r/Type? t)
         (every? (fn [[k v :as kv]]
                   (and (= 2 (count kv))
                        (r/TCResult? k)
                        (r/TCResult? v)))
                 pairs)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform #(-assoc-pair %1 %2 opts) t pairs
                           {:when #(and (ind/subtype? % (c/Un [r/-nil (c/-name `t/Associative r/-any r/-any)] opts) opts)
                                        (satisfies? AssocableType %))}
                           opts))

(defn assoc-pairs-noret [t pairs opts]
  {:pre [(r/Type? t)
         (every? (fn [[k v :as kv]]
                   (and (= 2 (count kv))
                        (r/Type? k)
                        (r/Type? v)))
                 pairs)]
   :post [((some-fn nil? r/Type?) %)]}
  (assoc-type-pairs t (map (fn [[k v]] [(r/ret k) (r/ret v)]) pairs) opts))

; dissoc support functions
(defn- -dissoc-key [t k opts]
  {:pre [(r/Type? t)
         (r/TCResult? k)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/union-or-nil
    (for [rtype (c/resolved-type-vector k opts)]
      (cond
        (ind/subtype? t r/-nil opts)
        t

        (r/HeterogeneousMap? t)
        (if (c/keyword-value? rtype)
          (c/make-HMap opts
                       {:mandatory
                        (dissoc (:types t) rtype)
                        :optional
                        (dissoc (:optional t) rtype)
                        :absent-keys
                        (conj (:absent-keys t) rtype)
                        :complete? (c/complete-hmap? t)})
          (c/make-HMap opts
                       {:optional
                        (into (:types t) (:optional t))
                        :absent-keys (:absent-keys t)
                        :complete? (c/complete-hmap? t)}))))
    opts))

(defn dissoc-keys [t ks opts]
  {:post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform #(-dissoc-key %1 %2 opts) t ks {} opts))

; merge support functions
(defn- merge-hmaps
  "Merges two HMaps into one, right into left.
  
  Preserves all key information where possible, missing keys in a right hand incomplete
  map will erase type information for those keys in the left.
  
  This strategy allows a merge of HMaps to always stay an HMap, without having to drop
  down to an IPersistentMap.
  
  For example:
  (merge '{:a 4 :b 6} '{:b 5}) -> '{:a t/Any :b 5}"
  [left right opts]
  {:pre [(r/HeterogeneousMap? left)
         (r/HeterogeneousMap? right)]}
  ;; want to know how often complete HMap's help with merging.
  (u/trace-when (c/complete-hmap? right)
    "Merge: complete used on the right")
  (c/make-HMap opts
   {:mandatory
      (let [m (:types left)
            ; optional keys on the right may or may not overwrite mandatory
            ; entries, so we union the common mandatory and optional val types together.
            ;
            ; eg. (merge (HMap :mandatory {:a Number}) (HMap :optional {:a t/Sym}))
            ;     => (HMap :mandatory {:a (t/U Number t/Sym)})
            m (merge-with #(c/Un [%1 %2] opts) 
                          m 
                          (select-keys (:optional right) (keys (:types left))))
            ;_ (prn "after first mandatory pass" m)

            ; combine left+right mandatory entries. 
            ; If right is partial, we can only update the entries common to both
            ; and give any entries type Any.
            ;
            ; eg. (merge (HMap :mandatory {:a Number}) (HMap :mandatory {:b Number}))
            ;     ;=> (HMap :mandatory {:a t/Any :b Number})
            ;
            ; If right is complete, it's safe to merge both mandatory maps.
            ; right-most wins on duplicates.
            m (merge m 
                     (cond
                       (c/partial-hmap? right)
                         (merge (:types right)
                                (zipmap (set/difference 
                                          (set (keys (:types left)))
                                          (set (keys (:types right)))
                                          (set (keys (:optional right)))
                                          (:absent-keys right))
                                        (repeat r/-any)))
                       :else
                        (:types right)))]
        ;(prn "after final mandatory pass" m)
        m)
    :optional
      (let [o (:optional left)
            ;_ (prn "before first optional pass" o)
            ; dissoc keys that end up in the mandatory map
            o (apply dissoc o 
                     (concat (keys (:types right))
                             ; entries mandatory on the left and optional
                             ; on the right are always in the mandatory map
                             (set/intersection 
                               (set (keys (:optional right)))
                               (set (keys (:types left))))))
            ;_ (prn "after first optional pass" o)
            ; now we merge any new :optional entries
            o (merge-with #(c/Un [%1 %2] opts) 
                          o
                          ; if the left is partial then we only add optional entries
                          ; common to both maps.
                          ; if left is complete, we are safe to merge both maps.
                          ;
                          ; (merge (HMap :optional {:a Number}) 
                          ;        (HMap :optional {:b Number}))
                          ; => (HMap)
                          ;
                          ; (merge (HMap :mandatory {:a '5})
                          ;        (HMap :optional {:a '10}))
                          ; => (HMap :mandatory {:a (t/U '5 '10)})
                          ;
                          ; (merge (HMap :optional {:a Number}) 
                          ;        (HMap :optional {:a t/Sym}))
                          ; => (HMap :optional {:a (t/U Number t/Sym)})
                          ;
                          ; (merge (HMap :optional {:a Number}) 
                          ;        (HMap :optional {:b Number} :complete? true))
                          ; => (HMap :optional {:a Number :b Number})
                          ;
                          ; (merge (HMap :optional {:a Number} :complete? true) 
                          ;        (HMap :optional {:b Number}))
                          ; => (HMap :optional {:a Number :b Number})
                          ;
                          ; (merge (HMap :optional {:a Number} :complete? true) 
                          ;        (HMap :optional {:b Number} :complete? true))
                          ; => (HMap :optional {:a Number :b Number})
                          (select-keys (:optional right) 
                                       (set/difference 
                                         (set (keys (:optional right)))
                                         ;remove keys that will be mandatory in the result
                                         (set (keys (:types left)))
                                         (if (c/partial-hmap? left)
                                           ; remove keys that give no new information.
                                           ; If left is partial, we remove optional
                                           ; keys in right that are not mentioned in left.
                                           (set/difference
                                             (set (keys (:optional right)))
                                             (set (keys (:types left)))
                                             (set (keys (:optional left)))
                                             (:absent-keys left))
                                           #{}))))]
        ;(prn "after final optional pass" o)
        o)
    :absent-keys
      (cond 
        ; (merge (HMap :absent-keys [:a :b :c]) (HMap :optional {:a Foo} :mandatory {:b Bar} :absent-keys [:c]))
        ; => (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar})
        ; (merge (HMap :absent-keys [:a :b :c]) (HMap :optional {:a Foo} :mandatory {:b Bar}))
        ; => (HMap :absent-keys [] :optional {:a Foo} :mandatory {:b Bar})
        (and (c/partial-hmap? left) 
             (c/partial-hmap? right))
          (set/intersection
            (set/difference (:absent-keys left)
                            (set (keys (:optional right)))
                            (set (keys (:types right))))
            (:absent-keys right))

        ; (merge (HMap :absent-keys [:a :b :c]) 
        ;        (HMap :optional {:a Foo} :mandatory {:b Bar} :complete? true))
        ; => (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar})
        (and (c/partial-hmap? left) 
             (c/complete-hmap? right))
          (set/difference (:absent-keys left)
                          (set (keys (:optional right)))
                          (set (keys (:types right))))

        ; (merge (HMap :complete? true)
        ;        (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar}))
        ; => (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar})
        (and (c/complete-hmap? left)
             (c/partial-hmap? right))
          (:absent-keys right)

        ; (merge (HMap :absent-keys [:a :b :c] :complete? true) 
        ;        (HMap :optional {:a Foo} :mandatory {:b Bar} :absent-keys [:c] :complete? true))
        ; => (HMap :optional {:a Foo} :mandatory {:b Bar} :complete? true)
        (and (c/complete-hmap? left) 
             (c/complete-hmap? right))
          #{}
        :else (throw (Exception. "should never get here")))
    :complete?
      (and (c/complete-hmap? left)
           (c/complete-hmap? right))}))

(defn- merge-pair
  [left right opts]
  {:pre [(r/Type? left)
         (r/TCResult? right)]
   :post [((some-fn nil? r/Type?) %)]}
  (cond
    ; preserve the rhs alias when possible
    (and (ind/subtype? left r/-nil opts)
         (ind/subtype? (r/ret-t right) (c/-name 'typed.clojure/Map r/-any r/-any) opts))
    (r/ret-t right)

    :else
    (c/union-or-nil
      (for [rtype (c/resolved-type-vector (r/ret-t right) opts)]
        (cond
          (and (ind/subtype? left r/-nil opts)
               (ind/subtype? rtype (c/Un [r/-nil (c/-name 'typed.clojure/Map r/-any r/-any)] opts) opts))
          rtype

          (and (ind/subtype? left (c/Un [r/-nil (c/-name 'typed.clojure/Map r/-any r/-any)] opts) opts)
               (ind/subtype? rtype r/-nil opts))
          left

          (and (r/F? left)
               (ind/subtype? left (c/Un [r/-nil (c/-name 'typed.clojure/Map r/-any r/-any)] opts) opts)
               (ind/subtype? rtype (c/Un [r/-nil (c/-name 'typed.clojure/Map r/-any r/-any)] opts) opts))
          (r/MergeType-maker [left (r/ret-t right)])

          (and (r/HeterogeneousMap? left)
               (r/HeterogeneousMap? rtype))
          (merge-hmaps left rtype opts)))
      opts)))

(defn merge-types
  ([opts] r/-nil)
  ([opts left-tcresult & r-tcresults]
   {:pre [(r/TCResult? left-tcresult)
          (every? r/TCResult? r-tcresults)]
    :post [((some-fn nil? r/Type?) %)]}
   (c/reduce-type-transform #(merge-pair %1 %2 opts) (:t left-tcresult) r-tcresults {} opts)))

; conj helper

(defn- conj-pair [left right opts]
  {:pre [(r/Type? left)
         (r/TCResult? right)]
   :post [((some-fn nil? r/TCResult?) right)]}
  (cond
    (r/HeterogeneousVector? left)
    (assoc-type-pairs left [[(r/ret (r/-val (count (:types left))))
                             right]]
                      opts)

    (ind/subtype? left r/-nil opts)
    (r/-hvec [(:t right)]
             {:filters [(:fl right)]
              :objects [(:o right)]}
             opts)

    ; other rules need to unwrap the rhs
    :else
    (c/union-or-nil
      (for [rtype (c/resolved-type-vector right opts)]
        (cond
          (and (r/HeterogeneousMap? left)
               (r/HeterogeneousVector? rtype))
          (if (= (count (:types rtype)) 2)
            (assoc-type-pairs left [(map r/ret (:types rtype))] opts)
            (err/int-error "Need vector of length 2 to conj to map" opts))

          (and (r/HeterogeneousMap? left)
               (ind/subtype? rtype r/-nil opts))
          left))
      opts)))

(defn conj-types [opts left & rtypes]
  {:pre [(r/Type? left)
         (every? r/TCResult? rtypes)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform #(conj-pair %1 %2 opts) left rtypes {} opts))

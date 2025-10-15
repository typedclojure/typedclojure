;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.update
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.clojure :as t]
            [typed.cljc.checker.proposition-rep :as fl]
            [typed.cljc.checker.path-rep :as pe]
            [typed.cljc.checker.utils :as u]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.lex-env :as lex]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.remove :as remove]
            [clojure.set :as set])
  (:import (clojure.lang IPersistentMap Keyword)))

;[(Seqable Proposition) Proposition -> Proposition]
(defn resolve* [atoms prop opts]
  {:pre [(every? fl/Proposition? atoms)
         (fl/Proposition? prop)]
   :post [(fl/Proposition? %)]}
  (reduce (fn [prop a]
            (cond
              (fl/AndProposition? a)
              (loop [ps (:fs a)
                     result []]
                (if (empty? ps)
                  (fo/-and result opts)
                  (let [p (first ps)]
                    (cond
                      (fo/opposite? a p opts) fl/-bot
                      (fo/implied-atomic? p a opts) (recur (next ps) result)
                      :else (recur (next ps) (cons p result))))))
              :else prop))
          prop
          atoms))

;[(Seqable Proposition) -> (Seqable Proposition)]
(defn flatten-props [ps]
  {:post [(every? fl/Proposition? %)]}
  (loop [acc #{}
         ps ps]
    (cond
      (empty? ps) acc
      (fl/AndProposition? (first ps)) (recur acc (concat (-> ps first :fs) (next ps)))
      :else (recur (conj acc (first ps)) (next ps)))))

;[(Seqable Proposition) (Seqable Proposition) (Atom Boolean) 
;  -> '[(Seqable (U ImpProposition fl/OrProposition AndProposition))
;       (Seqable (U TypeProposition NotTypeProposition))]]
(defn combine-props [new-props old-props flag opts]
  {:pre [(every? fl/Proposition? (concat new-props old-props))
         (instance? clojure.lang.Volatile flag)
         (boolean? @flag)]
   :post [(let [[derived-props derived-atoms] %]
            (and (every? (some-fn fl/ImpProposition? fl/OrProposition? fl/AndProposition?) derived-props)
                 (every? (some-fn fl/TypeProposition? fl/NotTypeProposition?) derived-atoms)))]}
  (let [atomic-prop? (some-fn fl/TypeProposition? fl/NotTypeProposition?)
        {new-atoms true new-formulas false} (group-by (comp boolean atomic-prop?) (flatten-props new-props))]
    (loop [derived-props []
           derived-atoms new-atoms
           worklist (concat old-props new-formulas)]
      (if (empty? worklist)
        [derived-props derived-atoms]
        (let [p (first worklist)
              p (resolve* derived-atoms p opts)]
          (cond
            (fl/AndProposition? p) (recur derived-props derived-atoms (concat (:fs p) (next worklist)))
            (fl/ImpProposition? p) 
            (let [{:keys [a c]} p
                  implied? (some (fn [p] (fo/implied-atomic? a p opts)) (concat derived-props derived-atoms))]
              #_(prn "combining " (unparse-proposition p opts) " with " (map #(unparse-proposition % opts) (concat derived-props
                                                                                                         derived-atoms))
                     " and implied:" implied?)
              (if implied?
                (recur derived-props derived-atoms (cons c (rest worklist)))
                (recur (cons p derived-props) derived-atoms (next worklist))))
            (fl/OrProposition? p)
            (let [ps (:fs p)
                  new-or (if (some (fn [f] (fo/implied-atomic? p f opts))
                                   (disj (set/union (set worklist) (set derived-props)) 
                                         p))
                           fl/-top
                           (loop [ps ps
                                  result []]
                             (cond
                               (empty? ps) (fo/-or result opts)
                               (some (fn [other-p] (fo/opposite? (first ps) other-p opts))
                                     (concat derived-props derived-atoms))
                               (recur (next ps) result)
                               (some (fn [other-p] (fo/implied-atomic? (first ps) other-p opts))
                                     derived-atoms)
                               fl/-top
                               :else (recur (next ps) (cons (first ps) result)))))]
              (if (fl/OrProposition? new-or)
                (recur (cons new-or derived-props) derived-atoms (next worklist))
                (recur derived-props derived-atoms (cons new-or (next worklist)))))
            (and (fl/TypeProposition? p)
                 (r/Bottom? (:type p)))
            (do 
              ;(prn "Variable set to bottom:" p)
              (vreset! flag false)
              [derived-props derived-atoms])
            (fl/TypeProposition? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (fl/NotTypeProposition? p)
                 (= r/-any (:type p)))
            (do 
              ;(prn "Variable set to bottom:" p)
              (vreset! flag false)
              [derived-props derived-atoms])
            (fl/NotTypeProposition? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (fl/TopProposition? p) (recur derived-props derived-atoms (next worklist))
            (fl/BotProposition? p) (do 
                                ;(prn "Bot filter found")
                                (vreset! flag false)
                                [derived-props derived-atoms])
            :else (recur (cons p derived-props) derived-atoms (next worklist))))))))

;; TODO make extensible
(defn with-updated-SeqOn [t SeqOn opts]
  (if (sub/subtype? ((requiring-resolve 'typed.cljc.checker.check.nthnext/seq-type)
                     t
                     opts)
                    SeqOn
                    opts)
    t
    r/-nothing))

; This is where filters are applied to existing types to generate more specific ones.
; t is the old type
; ft is the new type to update with
; pos? indicates polarity
; - if true, we're updating with a TypeProposition so we use restrict
; - if false, we're updating with a NotTypeProposition so we use remove
; lo is a sequence of path elements, in the same order as -> (left to right)
;[Type Type Boolean PathElems -> Type]
(defn update* [t ft pos? lo opts]
  {:pre [(r/Type? t)
         (r/Type? ft)
         (boolean? pos?)
         (pr/path-elems? lo)]
   :post [(r/Type? %)]}
  (let [t (c/fully-resolve-type t opts)]
    (cond
      ; The easy cases: we have a filter without a further path to travel down.
      ; Just update t with the correct polarity.

      (empty? lo)
      (if pos?
        (c/restrict t ft opts)
        (remove/remove* t ft opts))

      ; unwrap unions and intersections to update their members

      (or (r/Union? t)
          (r/Intersection? t))
      ((if (r/Union? t) c/Un c/In)
       (map #(update* % ft pos? lo opts) (:types t))
       opts)

      ;from here, t is fully resolved and is not a Union or Intersection

      ;heterogeneous map ops
      ; Positive and negative information down a keyword path
      ; eg. (number? (-> hmap :a :b))
      (and (pe/KeyPE? (first lo))
           (r/HeterogeneousMap? t))
      (let [polarity pos?
            update-to-type ft
            path lo
            [fkeype & rstpth] path
            fpth (cu/KeyPE->Type fkeype)
            update-inner (fn 
                           ([old] (update* old ft pos? rstpth opts))
                           ([old new] (update* old new pos? rstpth opts)))
            present? (contains? (:types t) fpth)
            optional? (contains? (:optional t) fpth)
            absent? (contains? (:absent-keys t) fpth)]
        ;updating a KeyPE should consider 3 cases:
        ; 1. the key is declared present
        ; 2. the key is declared absent
        ; 3. the key is not declared present, and is not declared absent
        (cond
          present?
            ; make-HMap simplifies to bottom if a mandatory entry is bottom
            (c/make-HMap opts
              {:mandatory (update (:types t) fpth update-inner)
               :optional (:optional t)
               :absent-keys (:absent-keys t)
               :complete? (c/complete-hmap? t)})
          (or absent?
              (and (c/complete-hmap? t)
                   (not present?)
                   (not optional?)))
            ; if an absent key is not nil, we have a contradiction
            (if (r/Bottom? (update-inner r/-nil))
              (r/Bottom)
              t)


          ; key is either unspoken for or :optional
          :else
          (let [; KeyPE are only used for `get` operations where `nil` is the
                ; not-found value. If the filter does not hold when updating
                ; it to nil, then we can assume this key path is present.
                ; However, for nested paths (rstpth not empty), we should be
                ; conservative and make the key optional with type Any, since
                ; we can't infer much about nested structure in HMaps.
                update-to-mandatory? (and (empty? rstpth)
                                          (r/Bottom? (update-inner r/-nil)))
                old-type (or ((:optional t) fpth) r/-any)]
            (if update-to-mandatory?
              (c/make-HMap opts
                {:mandatory (-> (:types t)
                                (assoc fpth (update-inner old-type)))
                 :optional (dissoc (:optional t) fpth)
                 :absent-keys (:absent-keys t)
                 :complete? (c/complete-hmap? t)})
              (c/make-HMap opts
                {:mandatory (:types t)
                 :optional (-> (:optional t)
                               (assoc fpth (update-inner old-type)))
                 :absent-keys (:absent-keys t)
                 :complete? (c/complete-hmap? t)})))))

      ; nil returns nil on keyword lookups
      (and (not pos?)
           (pe/KeyPE? (first lo))
           (r/Nil? t))
      (update* r/-nil ft pos? (next lo) opts)

      ; nil with positive KeyPE filter
      ; Accessing a key on nil always returns nil
      ; For nested paths, we need to continue recursing
      ; For single-level paths, check if the filter overlaps with nil
      (and pos?
           (pe/KeyPE? (first lo))
           (r/Nil? t))
      (if (seq (next lo))
        ; Nested path: recurse with nil since (:key nil) = nil
        (update* r/-nil ft pos? (next lo) opts)
        ; Single-level path: check if filter overlaps with nil
        (if (or (r/Nil? ft)
                (and (r/Union? ft) (some r/Nil? (:types ft)))
                (c/overlap ft r/-nil opts))
          r/-nil
          r/-nothing))

      ; update count information based on a call to `count`
      ; eg. (= 1 (count a))
      (and pos?
           (pe/CountPE? (first lo)))
      (let [u ft]
        (if-let [cnt (cond 
                       ; for (= 1 (count v))
                       (and (r/Value? u) (integer? (:val u)))
                       (r/make-ExactCountRange (:val u))

                       ; for (#{1 2 3} (count v))
                       (and (r/Union? u) 
                            (every? (every-pred r/Value?
                                                (comp integer? :val))
                                    (:types u)))
                       (let [ns (->> (map :val (:types u))
                                     (remove neg?)
                                     sort
                                     vec)]
                         (when (seq ns)
                           (r/make-CountRange (first ns)
                                              (last ns)))))]
          (c/restrict t cnt opts)
          (do (u/tc-warning (str "Cannot infer Count from type " (prs/unparse-type u opts)) opts)
              t)))

      ;can't do much without a NotCountRange type or difference type
      (and (not pos?)
           (pe/CountPE? (first lo)))
      t

      (and pos?
           (pe/NthPE? (first lo))
           (r/HSequential? t))
      (let [type ft
            path-expr (first lo)
            idx (:idx path-expr)
            fixed-types (conj (vec (repeat idx r/-any)) type)
            restriction-type (r/-hsequential fixed-types
                                             {:rest r/-any
                                              :kind (:kind t)}
                                             opts)]
        (c/restrict t restriction-type opts))

      (and (not pos?)
           (pe/NthPE? (first lo))
           (r/HSequential? t))
      t

      ; Update class information based on a call to `class`
      ; eg. (= java.lang.Integer (class a))
      (pe/ClassPE? (first lo))
      (let [u ft]
        (cond 
          ;restrict the obvious case where the path is the same as a Class Value
          (and pos?
               (r/Value? u)
               (class? (:val u)))
          (update* t (c/RClass-of-with-unknown-params (:val u) opts) true (next lo) opts)

          ; For this case to be sound, we need to prove there doesn't exist a subclass
          ; of (:val u). Finding subclasses is difficult on the JVM, even after verifying
          ; if a class is final.
          ;(and (not pos?)
          ;     (r/Value? u)
          ;     (class? (:val u)))
          ;(update* t (c/RClass-of-with-unknown-params (:val u) opts) true (next lo) opts)

          (and pos?
               (sub/subtype? u (c/RClass-of Object opts) opts))
          (update* t (c/RClass-of Object opts) true (next lo) opts)

          (and pos?
               (sub/subtype? u r/-nil opts))
          (update* t r/-nil true (next lo) opts)

          ;flip polarity in recursive calls
          (and (not pos?)
               (sub/subtype? (c/RClass-of Object opts) u opts))
          (update* t r/-nil true (next lo) opts)

          (and (not pos?)
               (sub/subtype? r/-nil u opts))
          (update* t (c/RClass-of Object opts) true (next lo) opts)

          ;; t = Any
          :else t))

      ; keyword invoke of non-hmaps
      ; When we have a generic map like (Map Kw Any) and we're updating
      ; with specific key information, we should create an HMap with that key.
      ; This enables occurrence typing with intermediate variables.
      (and (pe/KeyPE? (first lo))
           (r/RClass? t)
           (= 'clojure.lang.IPersistentMap (:the-class t)))
      (let [[fkeype & rstpth] lo
            fpth (cu/KeyPE->Type fkeype)
            ; For a generic map (Map K V), when updating key fpth:
            ; - If nil is NOT part of the update, make it a mandatory key
            ; - The key type should be the union of the update with nil (for not-found)
            update-inner (fn [old-type]
                          (update* old-type ft pos? rstpth opts))
            ; Extract value type from map (second type parameter)
            old-val-type (if (and (seq (:poly? t))
                                 (= 2 (count (:poly? t))))
                          (second (:poly? t))
                          r/-any)
            updated-val-type (update-inner old-val-type)
            ; Check if the update would make nil impossible
            update-to-mandatory? (r/Bottom? (update-inner r/-nil))]
        (if update-to-mandatory?
          ; Key must be present with the updated type
          (c/make-HMap opts
            {:mandatory {fpth updated-val-type}
             :optional {}
             :absent-keys #{}
             :complete? false})
          ; Key could be absent (returns nil) or present with updated type
          (c/make-HMap opts
            {:mandatory {}
             :optional {fpth updated-val-type}
             :absent-keys #{}
             :complete? false})))

      ; Other map types with KeyPE: return unchanged
      (pe/KeyPE? (first lo))
      t

      ; calls to `keys` and `vals`
      ((some-fn pe/KeysPE? pe/ValsPE?) (first lo))
      (let [[fstpth & rstpth] lo
            u ft
            ;_ (prn "u" (prs/unparse-type u opts))

            ; solve for x:  t <: (Seqable x)
            x (gensym "x-KeysValsPE")
            subst (let [opts (free-ops/with-bounded-frees opts {(r/make-F x) r/no-bounds})]
                    (u/handle-cs-gen-failure
                      (cgen/infer {x r/no-bounds} {} 
                                  [u]
                                  [(c/-name `t/Seqable (r/make-F x))]
                                  r/-any
                                  opts)))
            ;_ (prn "subst for Keys/Vals" subst)
            ]
        (if-not subst
          t
          (let [
            element-t-subst (get subst x)
            _ (assert (crep/t-subst? element-t-subst))
            ; the updated 'keys/vals' type
            element-t (:type element-t-subst)
            ;_ (prn "element-t" (prs/unparse-type element-t opts))
            _ (assert element-t)]
        ;; FIXME this is easy to implement, just recur update* on rstpth instead of nil.
        ;; should also add a test.
        (assert (empty? rstpth) (str "Further path NYI keys/vals"))
        (if pos?
          (update* t
                   (if (pe/KeysPE? fstpth)
                     (c/RClass-of IPersistentMap [element-t r/-any] opts)
                     (c/RClass-of IPersistentMap [r/-any element-t] opts))
                   pos? nil opts)
          ; can we do anything for a NotTypeProposition?
          t))))

      (pe/KeywordPE? (first lo))
      ;; t is the old type, eg. (Val "my-key"). Can also be any type.
      ;; ft is the new type, eg. (Val :my-key). Can also be in (U nil Kw).
      ;;
      ;; eg. (update* (Val "my-key") (Val :my-key) true [KeywordPE] opts)
      ;; Here we have 
      (update* t
               (cond
                 ;; Take the new type and un-keywordify it, then use that to update the old type.
                 (r/Value? ft) (let [{:keys [val]} ft]
                                 (cond
                                   (keyword? val) (let [kstr (str (when (namespace val)
                                                                    (str (namespace val) "/"))
                                                                  (name val))]
                                                    (c/Un [(r/-val kstr)
                                                           (r/-val (symbol kstr))
                                                           (r/-val val)]
                                                          opts))
                                   (nil? val) r/-any
                                   ;; impossible
                                   :else r/-nothing))

                 ;; if the new type is a keyword, old type must be a (U Str Sym Kw).
                 (sub/subtype? ft (c/RClass-of Keyword opts) opts)
                 (c/Un [(c/RClass-of Keyword opts)
                        (c/RClass-of String opts)
                        (c/RClass-of clojure.lang.Symbol opts)]
                       opts)

                 ;; if the output of `keyword` is at best (U nil Kw), input could be anything
                 (sub/subtype? ft (c/Un [r/-nil (c/RClass-of Keyword opts)] opts) opts)
                 r/-any

                 ;; impossible
                 :else r/-nothing)
               pos? (next lo) opts)

      (pe/SeqPE? (first lo))
      (let [;; t is the argument to `clojure.core/seq`
            ;; ft is the filter on the return value of `clojure.core/seq`
            updated (update* (c/-name `t/SeqOn t) ft pos? (next lo) opts)]
        ;(prn {:t t :ft ft :pos? pos? :lo lo :updated updated})
        (with-updated-SeqOn t updated opts))

      :else (err/int-error (str "update along ill-typed path " (pr-str (prs/unparse-type t opts)) " " (mapv #(prs/unparse-path-elem % opts) lo)) opts))))

(defn update-with-proposition [t lo opts]
  {:pre [((some-fn fl/TypeProposition? fl/NotTypeProposition?) lo)]
   :post [(r/Type? %)]}
  (update* t (:type lo) (fl/TypeProposition? lo) (fl/proposition-path lo) opts))

;; sets the flag box to #f if anything becomes (U)
;[PropEnv (Seqable Proposition) (Atom Boolean) -> PropEnv]
(defn env+ [env fs flag opts]
  {:pre [(lex/PropEnv? env)
         (every? (every-pred fl/Proposition? (complement fl/NoProposition?)) 
                 fs)
         (instance? clojure.lang.Volatile flag)
         (boolean? @flag)]
   :post [(lex/PropEnv? %)
          ; flag should be updated by the time this function exits
          (boolean? @flag)]}
  (let [[props atoms] (combine-props fs (:props env) flag opts)]
    (reduce (fn [env f]
              ;post-condition checked in env+
              {:pre [(lex/PropEnv? env)
                     (fl/Proposition? f)]}
              (cond
                (fl/BotProposition? f) (do ;(prn "Bot filter found in env+")
                                      (vreset! flag false)
                                      (update env :l (fn [l] 
                                                       (zipmap (keys l)
                                                               (repeat r/-nothing)))))
                ((some-fn fl/TypeProposition? fl/NotTypeProposition?) f)
                (let [;_ (prn "Update filter" f)
                      new-env (update env :l update (:id f)
                                      (fn [t]
                                        (when-not t
                                          (err/int-error (str "Updating local not in scope: " (:id f)
                                                              " " (-> env :l keys vec))
                                                         opts))
                                        (update-with-proposition t f opts)))]
                  ; update flag if a variable is now bottom
                  (when (some (comp r/Bottom? val) (:l new-env))
                    (vreset! flag false))
                  new-env)

                (and (fl/OrProposition? f)
                     (every? (some-fn fl/TypeProposition? fl/NotTypeProposition?) (:fs f))
                     (apply = (map fl/proposition-id (:fs f))))
                (let [id (-> f :fs first fl/proposition-id)
                      _ (assert (symbol? id))
                      new-env (update env :l update id
                                      (fn [t]
                                        (when-not t
                                          (err/int-error (str "Updating local not in scope: " (:id f)) opts))
                                        (c/Un (map (fn [f] (update-with-proposition t f opts))
                                                   (:fs f))
                                              opts)))]
                  ; update flag if a variable is now bottom
                  (when (some (comp r/Bottom? val) (:l new-env))
                    (vreset! flag false))
                  new-env)
                :else env))
            (assoc env :props (into (set atoms) props))
            (concat atoms props))))

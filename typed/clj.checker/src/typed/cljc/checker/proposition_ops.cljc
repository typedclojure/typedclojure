;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.proposition-ops
  (:refer-clojure :exclude [#?(:clj delay)])
  (:require [clojure.set :as set]
            [clojure.core.typed.util-vars :as vs]
            [typed.clojure :as t]
            [typed.cljc.checker.proposition-rep :as fr]
            [typed.cljc.checker.object-rep :as or]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r] 
            [typed.cljc.checker.utils :as u]
            #?(:clj [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]))
  (:import (typed.cljc.checker.proposition_rep BotProposition TopProposition NoProposition AndProposition 
                                          OrProposition TypeProposition NotTypeProposition ImpProposition
                                          PropositionSet)))

(defn -proposition [t i & [p]]
  {:pre [(r/Type? t)
         (fr/name-ref? i)
         ((some-fn nil? #(every? pr/PathElem? %)) p)]
   :post [(fr/Proposition? %)]}
  (cond
    (= r/-any t) fr/-top
    (r/Bottom? t) fr/-bot
    :else (fr/TypeProposition-maker t (seq p) i)))

(defn -not-proposition [t i & [p]]
  {:pre [(r/Type? t)
         (fr/name-ref? i)
         ((some-fn nil? #(every? pr/PathElem? %)) p)]
   :post [(fr/Proposition? %)]}
  (cond
    (r/Bottom? t) fr/-top
    (= r/-any t) fr/-bot
    :else (fr/NotTypeProposition-maker t (seq p) i)))

(defn -proposition-at [t o]
  (if (or/Path? o)
    (let [{p :path i :id} o]
      (-proposition t i p))
    fr/-top))

(defn -not-proposition-at [t o]
  (if (or/Path? o)
    (let [{p :path i :id} o]
      (-not-proposition t i p))
    fr/-top))

(defn- subtype?-var []
  (let [v (ns-resolve (find-ns 'typed.clj.checker.subtype) 'subtype?)]
    (assert (var? v) "subtype? unbound")
    v))

(defn opposite? [f1 f2 opts]
  {:pre [(fr/Proposition? f1)
         (fr/Proposition? f2)]
   :post [(boolean? %)]}
  (let [subtype? @(subtype?-var)]
    (cond
      (::vs/under-scope opts) false

      (and (fr/TypeProposition? f1)
           (fr/NotTypeProposition? f2))
      (let [{t1 :type p1 :path i1 :id} f1
            {t2 :type p2 :path i2 :id} f2]
        (and (= p1 p2)
             (= i1 i2)
             (subtype? t1 t2 opts)))

      (and (fr/NotTypeProposition? f1)
           (fr/TypeProposition? f2))
      (let [{t2 :type p2 :path i2 :id} f1
            {t1 :type p1 :path i1 :id} f2]
        (and (= p1 p2)
             (= i1 i2)
             (subtype? t1 t2 opts)))
      :else false)))


;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrProposition (alternative is AndProposition)
(defn compact [props or? opts]
  {:pre [(every? fr/Proposition? props)
         (boolean? or?)]
   :post [(every? fr/Proposition? %)]}
;  (prn "compact")
;  (prn "props" (map typed.clj.checker.parse-unparse/unparse-proposition props))
;  (prn "or?" or?)
  ;; props: the propositions we're processing
  ;; others: props that are neither TF or NTF
  (loop [props props
         others nil
         tf-map {}
         ntf-map {}]
    (if (empty? props)
      (concat others
              (vals tf-map)
              (vals ntf-map))
      (cond
        (and or? (fr/TypeProposition? (first props)))
        (let [{t1 :type f1 :path x :id :as p} (first props)]
          (recur (next props)
                 others
                 (-> tf-map
                     (update [f1 x]
                             #(if %
                                (if (fr/TypeProposition? %)
                                  (let [t2 (:type %)]
                                    (-proposition (c/Un [t1 t2] opts) x f1))
                                  (throw (Exception. (str "got something that isn't a type filter" p))))
                                p)))
                 ntf-map))

        (and (not or?) (fr/TypeProposition? (first props)))
        (let [{t1 :type f1 :path x :id} (first props)
              fl (tf-map [f1 x])]
          (cond
            (and (fr/TypeProposition? fl)
                 (not (c/overlap t1 (:type fl) opts)))
            ;; we're in an And, and we got two types for the same path that do not overlap
            [fr/-bot]

            (fr/TypeProposition? fl)
            (recur (next props)
                   others
                   (-> tf-map
                       (assoc [f1 x] (-proposition (c/restrict t1 (:type fl) opts) x f1)))
                   ntf-map)

            :else
            (recur (next props)
                   others
                   (-> tf-map
                       (assoc [f1 x] (-proposition t1 x f1)))
                   ntf-map)))

        (and (not or?) 
             (fr/NotTypeProposition? (first props)))
        (let [{t1 :type f1 :path x :id :as p} (first props)]
          (recur (next props)
                 others
                 tf-map
                 (-> ntf-map
                     (update [f1 x]
                             (fn [n]
                               (if n
                                 (if (fr/NotTypeProposition? n)
                                   (let [t2 (:type n)]
                                     (-not-proposition (c/Un [t1 t2] opts) x f1))
                                   (throw (Exception. (str "got something that isn't a nottypefilter" p))))
                                 p))))))
        :else
        (let [p (first props)]
          (recur (next props) (cons p others) tf-map ntf-map))))))


(declare -and)

(defn inverse-atom [a]
  {:pre [((some-fn fr/TypeProposition? fr/NotTypeProposition?) a)]
   :post [((some-fn fr/TypeProposition? fr/NotTypeProposition?) a)]}
  (cond
    (fr/TypeProposition? a) (-not-proposition (:type a) (:id a) (:path a))
    (fr/NotTypeProposition? a) (-proposition (:type a) (:id a) (:path a))))

(defn simplify-prop
  "Try and use atomic proposition a to simplify composite
  proposition b. a must be correct polarity."
  [a b opts]
  {:pre [((some-fn fr/TypeProposition? fr/NotTypeProposition?) a)
         ((some-fn fr/AndProposition? fr/OrProposition?) b)]
   :post [(fr/Proposition? %)]}
  (cond
    ; assuming a wrapping OrProposition
    (fr/AndProposition? b)
    (let [fs (set (:fs b))
          fs (set
               (for [f fs]
                 (cond
                   ; A ^ (B v A) => A
                   (fr/OrProposition? f) (simplify-prop a f opts)
                   :else f)))]
      (if (fs a)
        ; A v (notB ^ A) => A v notB
        (-and (disj fs a) opts)
        b))

    ; assuming a wrapping AndProposition
    (fr/OrProposition? b)
    (let [fs (set (:fs b))]
      ; A ^ (B v A) => A
      (if (fs a)
        a
        b))))


(comment
  (-or [(-not-proposition -nil 'a)
        (-and (-proposition -nil 'a)
              (-proposition -false 'b))]
       {})
  (simplify-prop (-proposition -nil 'a) (-and (-proposition -nil 'a)
                                         (-proposition -false 'b)))
  ;=> (-proposition -nil 'a)
  '[-or-proposition
    [-not-proposition (Value :Black) (:tree) 0]
    [-and-proposition
     ; or->and, elim -proposition (:Black) (:tree 0)
     [-proposition (Value :Black) (:tree) 0]
     [-or-proposition
      ;and->or,  elim -proposition (:Black) (:tree 0)
      [-and-proposition
       ;or->and,  elim -not-proposition (:Black) (:tree 0)
       [-proposition (Value :Black) (:tree) 0]
       [-not-proposition (Value :Red) (:left :tree) 0]]

      [-and-proposition
       ;or->and,  elim -not-proposition (:Black) (:tree 0)
       [-proposition (Value :Red) (:left :tree) 0]
       [-proposition (Value :Black) (:tree) 0]
       [-or-proposition
        [-and-proposition
         [-proposition (Value :Red) (:left :tree) 0]
         [-proposition (Value :Black) (:tree) 0]
         [-not-proposition (Value :Red) (:right :tree) 0]]
        [-and-proposition
         [-proposition (Value :Red) (:left :tree) 0]
         [-proposition (Value :Black) (:tree) 0]
         [-proposition (Value :Red) (:right :tree) 0]
         [-not-proposition (Value :Red) (:right :left :tree) 0]]]]]
     ]
    ]
  )

(declare atomic-proposition?)

;remove opposites in and filter
(defn remove-opposite [and-f atom-f opts]
  {:pre [(fr/Proposition? and-f)
         (fr/Proposition? atom-f)]
   :post [(fr/Proposition? %)]}
  (if (fr/AndProposition? and-f)
    (-and (remove #(opposite? % atom-f opts) (:fs and-f)) opts)
    and-f))

;(defn -or [& args]
;  (loop [new-props (set args)
;         ;atomic propositions
;         atoms #{}
;         last-props #{} ;stop iteration when (= (set/union new-props atoms) last-props)
;         ]
;    (assert ((con/set-c? atomic-proposition?) atoms))
;    (assert (every? (con/set-c? fr/Proposition?) [new-props last-props]))
;    (cond
;      ;reached fixed point
;      (= (set/union new-props atoms) last-props)
;      (case (count last-props)
;        0 fr/-bot
;        1 (first last-props)
;        (fr/->OrProposition last-props))
;
;      :else
;      (let [;flatten OrPropositions
;            original-props (set/union new-props atoms)
;            original-atoms atoms
;            fs (-> (apply concat
;                          (for [a (set/union new-props atoms)]
;                            (if (fr/OrProposition? a)
;                              (:fs a)
;                              [a])))
;                 set (disj fr/-bot))
;            {:keys [atoms] old-props :props} (group-by #(cond
;                                                          ((some-fn fr/TypeProposition? fr/NotTypeProposition?) %) :atoms
;                                                          :else :props)
;                                                       fs)
;            ;simplify AndPropositions by removing atomic props directly inside the AndProposition
;            ;if they are opposite of any atomic props we already have
;            next-props (doall
;                         (for [p old-props]
;                           (reduce (fn [p a] (remove-opposite p a))
;                                   p atoms)))
;            {:keys [atoms] new-props :props} (group-by #(cond
;                                                          ((some-fn fr/TypeProposition? fr/NotTypeProposition?) %) :atoms
;                                                          :else :props)
;                                                       (set/union (set next-props) (set atoms)))]
;
;        (assert (<= (count original-atoms) (count atoms)))
;        (recur (set new-props) (set atoms) (set original-props))))))

(declare implied-atomic?)

(defn -or [args opts]
  {:pre [(every? fr/Proposition? args)
         (not (fr/Proposition? opts))]
   :post [(fr/Proposition? %)]}
  (letfn [(mk [fs]
            {:pre [(every? fr/Proposition? fs)]
             :post [(fr/Proposition? %)]}
            (cond
              (empty? fs) fr/-bot
              (= 1 (count fs)) (first fs)
              :else (fr/OrProposition-maker (set fs))))
          (distribute [args]
            (let [{ands true others false} (group-by fr/AndProposition? args)]
              (if (empty? ands)
                (mk others)
                (let [{elems :fs} (first ands)] ;an AndProposition
                  (-and (for [a elems]
                          (-or (cons a (concat (next ands) others)) opts))
                        opts)))))]
    (loop [fs args
           result nil]
      (assert (every? fr/Proposition? fs))
      (assert (every? fr/Proposition? result))
      (if (empty? fs)
        (cond
          (empty? result) fr/-bot
          (= 1 (count result)) (first result)
          :else (distribute (compact result true opts)))
        (cond
          (fr/TopProposition? (first fs)) (first fs)
          (fr/OrProposition? (first fs)) (let [fs* (:fs (first fs))]
                                      (recur (concat fs* (next fs)) result))
          (fr/BotProposition? (first fs)) (recur (next fs) result)
          :else (let [t (first fs)]
                  (assert (fr/Proposition? t))
                  (cond
                    (some (fn [f] (opposite? f t opts)) (concat (rest fs) result))
                    fr/-top

                    (some (fn [f] (or (= f t)
                                      (implied-atomic? f t opts)))
                          result)
                    (recur (next fs) result)

                    :else
                    (recur (next fs) (cons t result)))))))))

(defn -imp [a c]
  {:pre [(fr/Proposition? a)
         (fr/Proposition? c)]
   :post [(fr/Proposition? %)]}
  (cond
    (fr/BotProposition? a) fr/-top
    (fr/TopProposition? a) c
    ;; P -> tt = tt for any P
    (fr/TopProposition? c) fr/-top
    :else (fr/ImpProposition-maker a c)))



;  A ^ (B v ...) -> (simplify A (B v ...))
;(defn -and [& args]
;             ;flatten direct internal AndPropositions
;  (let [flat (apply concat
;                    (for [fl args]
;                      (if (AndProposition? fl)
;                        (:fs fl)
;                        [fl])))
;        fs (set flat)]
;    (cond
;      (empty? fs) -bot
;      (fs -bot) -bot
;      (or (= 1 (count fs))
;          (= 1 (count (disj fs -top)))) (or (first (disj fs -top))
;                                            (first fs))
;      :else (->AndProposition (disj fs -top)))))

(defn -and [args opts]
  {:pre [(every? fr/Proposition? args)]
   :post [(fr/Proposition? %)]}
  (letfn [(mk [fs]
            {:pre [(every? fr/Proposition? fs)]
             :post [(fr/Proposition? %)]}
            (cond
              (empty? fs) fr/-top
              (= 1 (count fs)) (first fs)
              :else (apply fr/make-AndProposition fs)))]
    (loop [fs (set args)
           result nil]
      (if (empty? fs)
        (cond
          (empty? result) fr/-top
          (= 1 (count result)) (first result)
          ;; don't think this is useful here
          (= 2 (count result)) (let [;_ (prn "hit special 2 case in -and")
                                     [f1 f2] result]
                                 (if (opposite? f1 f2 opts)
                                   fr/-bot
                                   (if (= f1 f2)
                                     f1
                                     (mk (compact [f1 f2] false opts)))))
          :else
           ;; first, remove anything implied by the atomic propositions
           ;; We commonly see: (And (Or P Q) (Or P R) (Or P S) ... P), which this fixes
          (let [{atomic true not-atomic false} (group-by atomic-proposition? result)
                ;_ (prn "not-atomic" (map typed.clj.checker.parse-unparse/unparse-proposition not-atomic))
                not-atomic* (for [p not-atomic
                                  :when (not-any? (fn [a] (implied-atomic? p a opts)) atomic)]
                              p)]
            ;(prn "not-atomic*" not-atomic*)
             ;; `compact' takes care of implications between atomic props
            (mk (compact (concat not-atomic* atomic) false opts))))
        (let [ffs (first fs)]
          (cond
            (fr/BotProposition? ffs) ffs
            (fr/AndProposition? ffs) (let [fs* (:fs ffs)]
                                  (recur (next fs) (concat fs* result)))
            (fr/TopProposition? ffs) (recur (next fs) result)
            :else (let [t ffs]
                    (cond
                      (some (fn [f] (opposite? f ffs opts)) (concat (rest fs) result))
                      fr/-bot
                      (some (fn [f] 
                              (or (= f t)
                                  (implied-atomic? t f opts))) 
                            (concat (rest fs) result))
                      (recur (rest fs) result)
                      :else
                      (recur (rest fs) (cons t result))))))))))

(t/ann -FS [fr/Proposition fr/Proposition :-> PropositionSet])
(defn -FS [+ -]
  {:pre [(fr/Proposition? +)
         (fr/Proposition? -)]
   :post [(fr/PropositionSet? %)]}
  (fr/PropositionSet-maker + -))

(defn atomic-proposition? [a]
  {;TODO :pre [(fr/Proposition? a)]
   :post [(boolean? %)]}
  ;; Note: some-fn returns logical-false if no preds match
  ;; see https://clojure.atlassian.net/browse/CLJ-2634
  (boolean
    ((some-fn fr/TypeProposition? fr/NotTypeProposition?
              fr/TopProposition? fr/BotProposition?)
     a)))

; functions to get around compilation issues
(let [f (delay (-FS fr/-top fr/-bot))]
  (defn -true-proposition [] @f))
(let [f (delay (-FS fr/-bot fr/-top))]
  (defn -false-proposition [] @f))
(let [f (delay (-FS fr/-top fr/-top))]
  (defn -simple-proposition [] @f))
(let [f (delay (-FS fr/-bot fr/-bot))]
  (defn -unreachable-proposition [] @f))
(let [f (delay (-FS fr/-infer-top fr/-infer-top))]
  (defn -infer-proposition [] @f))

;; true if f1 is implied by f2
;; (implied-atomic? (is Number 0) (is Integer 0)) ;=> true
;; (implied-atomic? top bot) ;=> true
(defn implied-atomic? [f1 f2 opts]
  {:pre [(fr/Proposition? f1)
         (fr/Proposition? f2)]
   :post [(boolean? %)]}
  ;(prn "implied-atomic?" f1 f2)
  (let [subtype? @(subtype?-var)]
    (if (= f1 f2)
      true
      (cond
        (fr/BotProposition? f2) true
        (and (fr/TopProposition? f1)
             ((some-fn fr/TypeProposition? fr/NotTypeProposition?) f2)) true

        ; we don't learn anything interesting if everything on the right
        ; appears on the left
        (and (fr/OrProposition? f1)
             (fr/OrProposition? f2))
        (empty? (set/difference (:fs f2) (:fs f1)))

        (::vs/under-scope opts) false

        (fr/OrProposition? f1) (contains? (:fs f1) f2)
        (and (fr/TypeProposition? f1)
             (fr/TypeProposition? f2)) (and (= (:id f1) (:id f2))
                                       (= (:path f1) (:path f2))
                                       (subtype? (:type f2) (:type f1) opts))
        (and (fr/NotTypeProposition? f1)
             (fr/NotTypeProposition? f2)) (and (= (:id f1) (:id f2))
                                          (= (:path f1) (:path f2))
                                          (subtype? (:type f1) (:type f2) opts))
        :else false))))

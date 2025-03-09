;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.filter-ops
  (:refer-clojure :exclude [delay])
  (:require [clojure.set :as set]
            [clojure.core.typed.util-vars :as vs]
            [typed.clojure :as t]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.object-rep :as or]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r] 
            [typed.cljc.checker.utils :as u]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])
  (:import (typed.cljc.checker.filter_rep BotFilter TopFilter NoFilter AndFilter 
                                          OrFilter TypeFilter NotTypeFilter ImpFilter
                                          FilterSet)))

(defn -filter [t i & [p]]
  {:pre [(r/Type? t)
         (fr/name-ref? i)
         ((some-fn nil? #(every? pr/PathElem? %)) p)]
   :post [(fr/Filter? %)]}
  (cond
    (= r/-any t) fr/-top
    (r/Bottom? t) fr/-bot
    :else (fr/TypeFilter-maker t (seq p) i)))

(defn -not-filter [t i & [p]]
  {:pre [(r/Type? t)
         (fr/name-ref? i)
         ((some-fn nil? #(every? pr/PathElem? %)) p)]
   :post [(fr/Filter? %)]}
  (cond
    (r/Bottom? t) fr/-top
    (= r/-any t) fr/-bot
    :else (fr/NotTypeFilter-maker t (seq p) i)))

(defn -filter-at [t o]
  (if (or/Path? o)
    (let [{p :path i :id} o]
      (-filter t i p))
    fr/-top))
(defn -not-filter-at [t o]
  (if (or/Path? o)
    (let [{p :path i :id} o]
      (-not-filter t i p))
    fr/-top))

(defn- subtype?-var []
  (let [v (ns-resolve (find-ns 'typed.clj.checker.subtype) 'subtype?)]
    (assert (var? v) "subtype? unbound")
    v))

(defn opposite? [f1 f2 opts]
  {:pre [(fr/Filter? f1)
         (fr/Filter? f2)]
   :post [(boolean? %)]}
  (let [subtype? @(subtype?-var)]
    (cond
      (::vs/under-scope opts) false

      (and (fr/TypeFilter? f1)
           (fr/NotTypeFilter? f2))
      (let [{t1 :type p1 :path i1 :id} f1
            {t2 :type p2 :path i2 :id} f2]
        (and (= p1 p2)
             (= i1 i2)
             (subtype? t1 t2 opts)))

      (and (fr/NotTypeFilter? f1)
           (fr/TypeFilter? f2))
      (let [{t2 :type p2 :path i2 :id} f1
            {t1 :type p1 :path i1 :id} f2]
        (and (= p1 p2)
             (= i1 i2)
             (subtype? t1 t2 opts)))
      :else false)))


;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrFilter (alternative is AndFilter)
(defn compact [props or? opts]
  {:pre [(every? fr/Filter? props)
         (boolean? or?)]
   :post [(every? fr/Filter? %)]}
;  (prn "compact")
;  (prn "props" (map typed.clj.checker.parse-unparse/unparse-filter props))
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
        (and or? (fr/TypeFilter? (first props)))
        (let [{t1 :type f1 :path x :id :as p} (first props)]
          (recur (next props)
                 others
                 (-> tf-map
                     (update [f1 x]
                             #(if %
                                (if (fr/TypeFilter? %)
                                  (let [t2 (:type %)]
                                    (-filter (c/Un [t1 t2] opts) x f1))
                                  (throw (Exception. (str "got something that isn't a type filter" p))))
                                p)))
                 ntf-map))

        (and (not or?) (fr/TypeFilter? (first props)))
        (let [{t1 :type f1 :path x :id} (first props)
              fl (tf-map [f1 x])]
          (cond
            (and (fr/TypeFilter? fl)
                 (not (c/overlap t1 (:type fl) opts)))
            ;; we're in an And, and we got two types for the same path that do not overlap
            [fr/-bot]

            (fr/TypeFilter? fl)
            (recur (next props)
                   others
                   (-> tf-map
                       (assoc [f1 x] (-filter (c/restrict t1 (:type fl) opts) x f1)))
                   ntf-map)

            :else
            (recur (next props)
                   others
                   (-> tf-map
                       (assoc [f1 x] (-filter t1 x f1)))
                   ntf-map)))

        (and (not or?) 
             (fr/NotTypeFilter? (first props)))
        (let [{t1 :type f1 :path x :id :as p} (first props)]
          (recur (next props)
                 others
                 tf-map
                 (-> ntf-map
                     (update [f1 x]
                             (fn [n]
                               (if n
                                 (if (fr/NotTypeFilter? n)
                                   (let [t2 (:type n)]
                                     (-not-filter (c/Un [t1 t2] opts) x f1))
                                   (throw (Exception. (str "got something that isn't a nottypefilter" p))))
                                 p))))))
        :else
        (let [p (first props)]
          (recur (next props) (cons p others) tf-map ntf-map))))))


(declare -and)

(defn inverse-atom [a]
  {:pre [((some-fn fr/TypeFilter? fr/NotTypeFilter?) a)]
   :post [((some-fn fr/TypeFilter? fr/NotTypeFilter?) a)]}
  (cond
    (fr/TypeFilter? a) (-not-filter (:type a) (:id a) (:path a))
    (fr/NotTypeFilter? a) (-filter (:type a) (:id a) (:path a))))

(defn simplify-prop
  "Try and use atomic proposition a to simplify composite
  proposition b. a must be correct polarity."
  [a b opts]
  {:pre [((some-fn fr/TypeFilter? fr/NotTypeFilter?) a)
         ((some-fn fr/AndFilter? fr/OrFilter?) b)]
   :post [(fr/Filter? %)]}
  (cond
    ; assuming a wrapping OrFilter
    (fr/AndFilter? b)
    (let [fs (set (:fs b))
          fs (set
               (for [f fs]
                 (cond
                   ; A ^ (B v A) => A
                   (fr/OrFilter? f) (simplify-prop a f opts)
                   :else f)))]
      (if (fs a)
        ; A v (notB ^ A) => A v notB
        (-and (disj fs a) opts)
        b))

    ; assuming a wrapping AndFilter
    (fr/OrFilter? b)
    (let [fs (set (:fs b))]
      ; A ^ (B v A) => A
      (if (fs a)
        a
        b))))


(comment
  (-or [(-not-filter -nil 'a)
        (-and (-filter -nil 'a)
              (-filter -false 'b))]
       {})
  (simplify-prop (-filter -nil 'a) (-and (-filter -nil 'a)
                                         (-filter -false 'b)))
  ;=> (-filter -nil 'a)
  '[-or-filter
    [-not-filter (Value :Black) (:tree) 0]
    [-and-filter
     ; or->and, elim -filter (:Black) (:tree 0)
     [-filter (Value :Black) (:tree) 0]
     [-or-filter
      ;and->or,  elim -filter (:Black) (:tree 0)
      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Black) (:tree) 0]
       [-not-filter (Value :Red) (:left :tree) 0]]

      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Red) (:left :tree) 0]
       [-filter (Value :Black) (:tree) 0]
       [-or-filter
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-not-filter (Value :Red) (:right :tree) 0]]
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-filter (Value :Red) (:right :tree) 0]
         [-not-filter (Value :Red) (:right :left :tree) 0]]]]]
     ]
    ]
  )

(declare atomic-filter?)

;remove opposites in and filter
(defn remove-opposite [and-f atom-f opts]
  {:pre [(fr/Filter? and-f)
         (fr/Filter? atom-f)]
   :post [(fr/Filter? %)]}
  (if (fr/AndFilter? and-f)
    (-and (remove #(opposite? % atom-f opts) (:fs and-f)) opts)
    and-f))

;(defn -or [& args]
;  (loop [new-props (set args)
;         ;atomic propositions
;         atoms #{}
;         last-props #{} ;stop iteration when (= (set/union new-props atoms) last-props)
;         ]
;    (assert ((con/set-c? atomic-filter?) atoms))
;    (assert (every? (con/set-c? fr/Filter?) [new-props last-props]))
;    (cond
;      ;reached fixed point
;      (= (set/union new-props atoms) last-props)
;      (case (count last-props)
;        0 fr/-bot
;        1 (first last-props)
;        (fr/->OrFilter last-props))
;
;      :else
;      (let [;flatten OrFilters
;            original-props (set/union new-props atoms)
;            original-atoms atoms
;            fs (-> (apply concat
;                          (for [a (set/union new-props atoms)]
;                            (if (fr/OrFilter? a)
;                              (:fs a)
;                              [a])))
;                 set (disj fr/-bot))
;            {:keys [atoms] old-props :props} (group-by #(cond
;                                                          ((some-fn fr/TypeFilter? fr/NotTypeFilter?) %) :atoms
;                                                          :else :props)
;                                                       fs)
;            ;simplify AndFilters by removing atomic props directly inside the AndFilter
;            ;if they are opposite of any atomic props we already have
;            next-props (doall
;                         (for [p old-props]
;                           (reduce (fn [p a] (remove-opposite p a))
;                                   p atoms)))
;            {:keys [atoms] new-props :props} (group-by #(cond
;                                                          ((some-fn fr/TypeFilter? fr/NotTypeFilter?) %) :atoms
;                                                          :else :props)
;                                                       (set/union (set next-props) (set atoms)))]
;
;        (assert (<= (count original-atoms) (count atoms)))
;        (recur (set new-props) (set atoms) (set original-props))))))

(declare implied-atomic?)

(defn -or [args opts]
  {:pre [(every? fr/Filter? args)
         (not (fr/Filter? opts))]
   :post [(fr/Filter? %)]}
  (letfn [(mk [fs]
            {:pre [(every? fr/Filter? fs)]
             :post [(fr/Filter? %)]}
            (cond
              (empty? fs) fr/-bot
              (= 1 (count fs)) (first fs)
              :else (fr/OrFilter-maker (set fs))))
          (distribute [args]
            (let [{ands true others false} (group-by fr/AndFilter? args)]
              (if (empty? ands)
                (mk others)
                (let [{elems :fs} (first ands)] ;an AndFilter
                  (-and (for [a elems]
                          (-or (cons a (concat (next ands) others)) opts))
                        opts)))))]
    (loop [fs args
           result nil]
      (assert (every? fr/Filter? fs))
      (assert (every? fr/Filter? result))
      (if (empty? fs)
        (cond
          (empty? result) fr/-bot
          (= 1 (count result)) (first result)
          :else (distribute (compact result true opts)))
        (cond
          (fr/TopFilter? (first fs)) (first fs)
          (fr/OrFilter? (first fs)) (let [fs* (:fs (first fs))]
                                      (recur (concat fs* (next fs)) result))
          (fr/BotFilter? (first fs)) (recur (next fs) result)
          :else (let [t (first fs)]
                  (assert (fr/Filter? t))
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
  {:pre [(fr/Filter? a)
         (fr/Filter? c)]
   :post [(fr/Filter? %)]}
  (cond
    (fr/BotFilter? a) fr/-top
    (fr/TopFilter? a) c
    ;; P -> tt = tt for any P
    (fr/TopFilter? c) fr/-top
    :else (fr/ImpFilter-maker a c)))



;  A ^ (B v ...) -> (simplify A (B v ...))
;(defn -and [& args]
;             ;flatten direct internal AndFilters
;  (let [flat (apply concat
;                    (for [fl args]
;                      (if (AndFilter? fl)
;                        (:fs fl)
;                        [fl])))
;        fs (set flat)]
;    (cond
;      (empty? fs) -bot
;      (fs -bot) -bot
;      (or (= 1 (count fs))
;          (= 1 (count (disj fs -top)))) (or (first (disj fs -top))
;                                            (first fs))
;      :else (->AndFilter (disj fs -top)))))

(defn -and [args opts]
  {:pre [(every? fr/Filter? args)]
   :post [(fr/Filter? %)]}
  (letfn [(mk [fs]
            {:pre [(every? fr/Filter? fs)]
             :post [(fr/Filter? %)]}
            (cond
              (empty? fs) fr/-top
              (= 1 (count fs)) (first fs)
              :else (apply fr/make-AndFilter fs)))]
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
          (let [{atomic true not-atomic false} (group-by atomic-filter? result)
                ;_ (prn "not-atomic" (map typed.clj.checker.parse-unparse/unparse-filter not-atomic))
                not-atomic* (for [p not-atomic
                                  :when (not-any? (fn [a] (implied-atomic? p a opts)) atomic)]
                              p)]
            ;(prn "not-atomic*" not-atomic*)
             ;; `compact' takes care of implications between atomic props
            (mk (compact (concat not-atomic* atomic) false opts))))
        (let [ffs (first fs)]
          (cond
            (fr/BotFilter? ffs) ffs
            (fr/AndFilter? ffs) (let [fs* (:fs ffs)]
                                  (recur (next fs) (concat fs* result)))
            (fr/TopFilter? ffs) (recur (next fs) result)
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

(t/ann -FS [fr/Filter fr/Filter :-> FilterSet])
(defn -FS [+ -]
  {:pre [(fr/Filter? +)
         (fr/Filter? -)]
   :post [(fr/FilterSet? %)]}
  (fr/FilterSet-maker + -))

(defn atomic-filter? [a]
  {;TODO :pre [(fr/Filter? a)]
   :post [(boolean? %)]}
  ;; Note: some-fn returns logical-false if no preds match
  ;; see https://clojure.atlassian.net/browse/CLJ-2634
  (boolean
    ((some-fn fr/TypeFilter? fr/NotTypeFilter?
              fr/TopFilter? fr/BotFilter?)
     a)))

; functions to get around compilation issues
(let [f (delay (-FS fr/-top fr/-bot))]
  (defn -true-filter [] @f))
(let [f (delay (-FS fr/-bot fr/-top))]
  (defn -false-filter [] @f))
(let [f (delay (-FS fr/-top fr/-top))]
  (defn -simple-filter [] @f))
(let [f (delay (-FS fr/-bot fr/-bot))]
  (defn -unreachable-filter [] @f))
(let [f (delay (-FS fr/-infer-top fr/-infer-top))]
  (defn -infer-filter [] @f))

;; true if f1 is implied by f2
;; (implied-atomic? (is Number 0) (is Integer 0)) ;=> true
;; (implied-atomic? top bot) ;=> true
(defn implied-atomic? [f1 f2 opts]
  {:pre [(fr/Filter? f1)
         (fr/Filter? f2)]
   :post [(boolean? %)]}
  ;(prn "implied-atomic?" f1 f2)
  (let [subtype? @(subtype?-var)]
    (if (= f1 f2)
      true
      (cond
        (fr/BotFilter? f2) true
        (and (fr/TopFilter? f1)
             ((some-fn fr/TypeFilter? fr/NotTypeFilter?) f2)) true

        ; we don't learn anything interesting if everything on the right
        ; appears on the left
        (and (fr/OrFilter? f1)
             (fr/OrFilter? f2))
        (empty? (set/difference (:fs f2) (:fs f1)))

        (::vs/under-scope opts) false

        (fr/OrFilter? f1) (contains? (:fs f1) f2)
        (and (fr/TypeFilter? f1)
             (fr/TypeFilter? f2)) (and (= (:id f1) (:id f2))
                                       (= (:path f1) (:path f2))
                                       (subtype? (:type f2) (:type f1) opts))
        (and (fr/NotTypeFilter? f1)
             (fr/NotTypeFilter? f2)) (and (= (:id f1) (:id f2))
                                          (= (:path f1) (:path f2))
                                          (subtype? (:type f1) (:type f2) opts))
        :else false))))

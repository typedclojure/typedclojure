;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.spec.subtype
  "A subtyping lattice for specs.

  Usage:
    See `direct-subtypes` and `direct-subtypes`.
  
  Extending:
    For new predicates, see `register-subs`.
    For heterogeneous ops like collections and regexes,
    see `direct-sub-op`.")

(create-ns 'clojure.alpha.spec)
(alias 's 'clojure.alpha.spec)

;; database rep

(defonce ^:private rels-db (atom {::sub-rel {}
                                  ::sup-rel {}
                                  ::distinct-rel {}}))

(defn rels
  "Returns a map from relation name to value.
  
  Relations:
  ::sub-rel       (Map SpecForm (Set SpecForm))
  Maps a spec to all its immediate supertypes

  ::sup-rel       (Map SpecForm (Set SpecForm))
  Maps a spec to all its immediate subtypes

  ::distinct-rel  (Map SpecForm (Set SpecForm))
  Maps a spec to known specs that represent distinct values.
  "
  []
  @rels-db)

;; extension interface

(def ^:private sconj (fnil conj #{}))

(defn register-sub
  "Register s1 as an immediate subtype to s2."
  [s1 s2]
  (swap! rels-db #(-> %
                      (update-in [::sub-rel s1] sconj s2)
                      (update-in [::sup-rel s2] sconj s1)))
  nil)

(defn register-distinct
  "Register s1 and s2 as representing distinct values."
  [s1 s2]
  (swap! rels-db #(-> %
                      (update-in [::distinct-rel s1] sconj s2)
                      (update-in [::distinct-rel s2] sconj s1)))
  nil)

(defn register-subs
  "For each adjacent pair of specs s1 s2, registers
  s1 as a subtype of s2. If either is a vector, registers
  each s1 as a subtype of each s2."
  [& specs]
  (run! (fn [[s1 s2]]
          (cond
            (vector? s1) (run! #(register-sub % s2) s1)
            (vector? s2) (run! #(register-sub s1 %) s2)
            :else (register-sub s1 s2)))
        (map vector specs (next specs))))

(defn register-distincts
  "Registers every combination of specs as distinct."
  [& specs]
  (doseq [s1 specs
          s2 specs
          :when (not= s1 s2)]
    (assert ((every-pred qualified-symbol?) s1 s2))
    (register-distinct s1 s2)))

(defmulti direct-sub-op
  "Dispatches on the operation of a spec form (a `seq?`)
  and returns a set of direct subtypes or supertypes of
  the spec. Do not include the spec itself.

  Map of options helps abstract over computing sub or supertypes.
  The implementor can usually assume they are computing subtypes
  (ie., the implementation will be called from a covariant context).
  For finer control, see :direction.

  Takes a map of options:
  - :direction        :sub if computing covariantly (should return subtypes),
                      :sup if computing contravariantly (should return supertypes)
  - :direct-subtypes  A function returning the set of direct subtypes
                      of the spec argument in a covariant context.
  - :direct-supertypes  A function returning the set of direct subtypes
                        of the spec argument in a covariant context.
  "
  (fn [spec opt]
    (and (seq? spec)
         (first spec))))

;; querying interface

(declare direct-subtypes direct-supertypes)

(defn- direct-sub* [t direction]
  (let [[sup-rel direct-subtypes direct-supertypes]
        (case direction
          :sub [::sup-rel direct-subtypes direct-supertypes]
          :sup [::sub-rel direct-supertypes direct-subtypes])]
    (cond-> (get-in (rels) [sup-rel t] #{})
      (seq? t) (into
                 (direct-sub-op
                   t
                   {:direction direction
                    :direct-subtypes direct-subtypes
                    :direct-supertypes direct-supertypes})))))

(defn direct-subtypes
  "Returns the set of known direct subtypes of upper, a spec form."
  [upper]
  (direct-sub* upper :sub))

(defn direct-supertypes
  "Returns the set of known direct subtypes of upper, a spec form."
  [lower]
  (direct-sub* lower :sup))

;; base lattice

(register-subs
  `(s/or)
  [`nil?
   `bytes?
   `uri?
   `uuid?
   `inst?
   `char?
   `string?
   `simple-symbol?
   `qualified-symbol?
   `simple-keyword?
   `qualified-keyword?
   ])

(register-subs
  [`nil?
   `^{::frequency 20} some?]
  `any?)

(register-subs
  [`^{::frequency 2} ident?
   `^{::frequency 2} number?
   `^{::frequency 2} seqable?
   `^{::frequency 2} coll?
   `^{::frequency 2} associative?
   `^{::frequency 0.5} bytes?
   `indexed?
   `sequential?
   `uri?
   `uuid?
   `inst?
   `char?
   `boolean?
   ; [Any * -> Nothing] <: [Nothing * -> Any]
   `(s/fspec :args (s/* (s/or))
             :ret any?)]
  `some?)

(register-subs
  [`map?
   `vector?]
  `associative?)

(register-subs
  [`list?
   `seq?
   `vector?]
  `sequential?)

(register-subs
  [`map?
   `list?
   `seq?
   `vector?
   `set?
   `string?
   `^{::frequency 0.5} nil?]
  `seqable?)

;FIXME empty? belongs here somewhere but nil is not empty
(register-subs
  [`map?
   `list?
   `seq?
   `vector?
   `set?]
  `coll?)

(register-distincts
  `symbol?
  `number?
  `keyword?
  `nil?
  `string?
  `uri?
  `uuid?
  `inst?
  `boolean?)

(register-subs
  [`simple-symbol? `qualified-symbol?]
  `symbol?
  `ident?)

(register-subs
  [`simple-keyword?
   `qualified-keyword?]
  `keyword?
  `ident?)

(register-subs
  [`simple-keyword?
   `simple-symbol?]
  `simple-ident?
  `ident?)

(register-subs
  [`qualified-keyword?
   `qualified-symbol?]
  `qualified-ident?
  `ident?)

(register-distincts
  `simple-ident?
  `qualified-ident?)

(register-subs
  `(s/or)
  `double?
  `float?
  `number?)

(register-subs
  `(s/or)
  [`zero? `pos-int?]
  `nat-int?
  `int?
  `integer?
  `rational?
  `number?)

(register-subs
  `(s/or)
  `neg-int?
  `int?)

(register-distincts
  `neg-int?
  `nat-int?)

(register-distincts
  `zero?
  `pos-int?
  `neg-int?)

(register-subs
  [`integer? `ratio? `decimal?]
  `rational?
  `number?)

(register-distincts
  `integer?
  `ratio?
  `decimal?)

(register-subs
  `(s/or)
  [`true? `false?]
  `boolean?)

(register-distincts
  `true?
  `false?)

; het ops

(register-subs
  `(s/or)
  `(s/map-of (s/or) (s/or))
  `(s/map-of any? any?)
  `map?)

(register-subs
  `(s/or)
  `(s/tuple (s/* (s/or)))
  `(s/tuple (s/* any?))
  `vector?)

;; heterogeneous ops

(defmethod direct-sub-op :default [_ _] nil)

;copied from math.combinatorics
(defn- cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

; (All [a b c]
;   [[a -> (Coll b)]
;    [b -> (Coll c)]
;    a
;    -> (Coll c)])
(defn args-direct-subs* [process-args fcat args]
  (let [subargs (map conj (process-args args) args)]
    (sequence (comp
                (remove #{args})
                (mapcat fcat))
              (apply cartesian-product subargs))))

(defmethod direct-sub-op `s/cat
  [[_ & k-preds :as spec] {:keys [direct-subtypes]}]
  {:pre [(odd? (count spec))]}
  (let [[ks preds] ((juxt #(map first %)
                          #(map second %))
                    (partition 2 k-preds))]
    (args-direct-subs*
      #(map direct-subtypes %)
      (fn [predssub]
        {:pre [(= (count preds)
                  (count predssub))]}
        [`(s/cat ~@(mapcat (fn [k psub]
                             [k psub])
                           ks
                           predssub))])
      preds)))

(defmethod direct-sub-op `s/?
  [[_ t :as spec] {:keys [direct-subtypes]}]
  {:pre [(#{2} (count spec))]}
  (args-direct-subs*
    #(map direct-subtypes %)
    (fn [[tsub :as m]]
      {:pre [(#{1} (count m))]}
      [;`(s/cat)
       ;`(s/cat :single ~t)
       `(s/? ~tsub)])
    [t]))

(defmethod direct-sub-op `s/*
  [[_ t :as spec] {:keys [direct-subtypes]}]
  {:pre [(#{2} (count spec))]}
  (args-direct-subs*
    #(map direct-subtypes %)
    (fn [[tsub :as m]]
      {:pre [(#{1} (count m))]}
      ; could expand further with s/cat
      [`(s/* ~tsub)])
    [t]))

(defmethod direct-sub-op `s/+
  [[_ t :as spec] {:keys [direct-subtypes]}]
  {:pre [(#{2} (count spec))]}
  (args-direct-subs*
    #(map direct-subtypes %)
    (fn [[tsub :as m]]
      {:pre [(#{1} (count m))]}
      ; could expand further with s/cat 
      [`(s/+ ~tsub)])
    [t]))

(defmethod direct-sub-op `s/map-of
  [[_ ks vs & more :as spec] {:keys [direct-subtypes]}]
  (assert (<= 3 (count spec)))
  (assert (not more)
          (str "s/map-of options not supported in subtyping algorithm"))
  (args-direct-subs*
    #(map direct-subtypes %)
    (fn [[ks vs :as m]]
      {:pre [(#{2} (count m))]}
      [`(s/map-of ~ks ~vs)])
    [ks vs]))

(defmethod direct-sub-op `s/fspec
  [[_ & {:keys [args ret] :as opt} :as spec] {:keys [direct-subtypes direct-supertypes]}]
  (assert (and args ret)
          (str "Subtyping requires both :args and :ret for s/fspec"))
  (args-direct-subs*
    (juxt direct-supertypes direct-subtypes)
    (fn [[args ret]]
      [`(s/fspec :args args
                 ~@(find opt :fn)
                 :ret ret)])
    [args ret]))

(defmethod direct-sub-op `s/tuple
  [[_ & args :as spec] {:keys [direct-subtypes]}]
  (args-direct-subs*
    #(map direct-subtypes %)
    (fn [args]
      [`(s/tuple ~@args)])
    args))

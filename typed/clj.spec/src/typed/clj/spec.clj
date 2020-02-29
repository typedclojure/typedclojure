;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.spec
  "Pre-alpha"
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test :refer :all]))

(alias 'sa 'clojure.spec.alpha)
(set! *warn-on-reflection* true)

(deftype Reduced [val])

; modified from clojure.walk
(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall.
  On prewalk, short-circuits on Reduced. `outer` should not return Reduced."
  {:added "1.1"}
  [inner outer config form]
  (let [inner #(inner config %)]
    (cond
     (instance? Reduced form) (outer (.val ^Reduced form))
     (list? form) (outer (apply list (map inner form)))
     (instance? clojure.lang.IMapEntry form)
     (outer (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
     (seq? form) (outer (doall (map inner form)))
     (instance? clojure.lang.IRecord form)
       (outer (reduce (fn [r x] (conj r (inner x))) form form))
     (coll? form) (outer (into (empty form) (map inner form)))
     :else (outer form))))

; modified from clojure.walk
(defn prewalk
  "Like postwalk, but does pre-order traversal. Short-circuits on Reduced."
  {:added "1.1"}
  [f config form]
  (let [{:keys [config form]} (f config form)]
    (walk (partial prewalk f)
          identity
          config
          form)))

(defprotocol ITypeFn
  (apply-typefn* [this arg-map]))

(defprotocol IPoly
  (inst* [this arg-map]))

(defmacro tfn
  "(tfn [:x {:lower (s/or)
             :upper any?
             :variance :covariant}
         :y {:lower (s/or)
             :upper any?
             :variance :covariant}]
        (map-of (tvar :x) (tvar :y)))
  "
  [binder body]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tfn ~binder ~body))))

(defmacro all
  "(all :binder
        (binder
          :x (tvar-spec)
          :y (tvar-spec))
        :body
        (map-of (tvar :x) (tvar :y)))
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(all ~@args))))

(defmacro binder
  "(binder
     :x (tvar-spec :kind ::any-spec?
                   :position #{:output})
     :y (tvar-spec :kind ::any-spec?
                   :position #{:output}))
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(binder ~@args))))

(defmacro tapp
  "(tapp tfn {:x int? :y boolean?})
  "
  [tfn args-map]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tapp ~tfn ~args-map))))

(defmacro inst
  "(inst poly
         :tvars {:x int? :y boolean?}
         :dotted-tvars {:z [{:tvars {:z int?}}
                            {:tvars {:z boolean?}}
                            {:tvars {:z number?}}]})
  "
  [poly args-map]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(inst ~poly ~args-map))))

(defmacro tvar
  "(tvar :a
         :wrap identity)

  An uninstantiated type variable. Acts as a placeholder and must be
  substituted away before use.

  Optionally takes a function that wraps the type variable after instantiation,
  which defaults to `clojure.core/identity`.
  "
  [tv & opt]
  {:pre [(simple-keyword? tv)]}
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tvar ~tv ~@opt))))

(defn coerce-dotted-cat [tv xs]
  {:pre [(simple-keyword? tv)]}
  `(s/cat ~@(mapcat (fn [i x]
                      [(keyword nil (str (name tv) i)) x])
                    (range)
                    xs)))

(comment
  (coerce-dotted-cat :a `[integer? boolean?])
  )

(defmacro dotted-pretype
  "(dotted-pretype (coll-of (tvar :a)) :a)

  Takes a pretype and type variable name tv.

  When tv is instantiated to a sequence of instantiations,
  instantiates a vector a pretype's using the instantiations pairwise,
  and then returns the result of passing that vector to :wrap

  (All [x y]
    [x Kw & :optional {:regex [(Vec x) -> y]} -> Any])

  Defaults:
  - :wrap  (fn [xs]
             (coerce-dotted-cat tv xs))
  "
  [pretype tv & opt]
  {:pre [(simple-keyword? tv)]}
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(dotted-pretype ~pretype ~tv ~@opt))))

(defmacro spec-between
  "(spec-between :lower lower :upper upper)

  Accepts symbolic spec between specs lower and upper.
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(spec-between ~@args))))

(defmacro tvar-spec
  "(tvar-spec :name :x
              :kind ::any-spec?)

  A template for all the instantiations of a type variable.
  If :dotted true, uses :regex to generate the sequence of instantions
  of tvar.

  :name enables self-recursion.

  :kind represents the set of possible values to instantiate
  the type variable with. By default this is the set of all
  specs `s/spec?`.

  Defaults:
  - :kind     ::any-spec?
  - :gen      nil
  "
  [& args]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tvar-spec ~@args))))

; tfn is resolved
; args-map is not
(defn- tapp-impl
  [tfn args-map]
  (let [applied (delay 
                  (apply-typefn*
                    tfn
                    args-map))]
    (reify
      Spec
      (conform* [_ x settings-key settings]
        (conform* @applied x settings-key settings))
      (unform* [_ x]
        (unform* @applied x))
      (explain* [_ path via in x settings-key settings]
        (explain* @applied path via in x settings-key settings))
      (gen* [_ overrides path rmap]
        (gen* @applied overrides path rmap))
      (with-gen* [_ gfn]
        (with-gen* @applied gfn))
      (describe* [_]
        `(tapp ~(s/describe tfn)
               ~args-map)))))

; poly is resolved
; args-map is not
(defn- inst-impl
  [poly args-map]
  (inst* poly args-map))

;TODO rigorous testing for capture-avoidance
(defn subst-tvar [spec sbst]
  {:pre [(map? sbst)]}
  (prewalk (fn [sbst spec]
             {:pre [(map? sbst)]}
             (cond
               ;TODO update to new vector binder syntax
               (and (seq? spec)
                    (`#{tfn all} (first spec)))
               ; TODO traverse :binder and :body correctly
               {:config (apply dissoc sbst (keys (second spec)))
                :form spec}

               (and (seq? spec)
                    (#{`tvar} (first spec)))
               (let [[_ tv & {:keys [wrap]
                              :or {wrap `identity}}]
                     spec
                     _ (assert (simple-keyword? tv))
                     wrap (eval wrap)
                     sval (sbst tv)]
                 {:config sbst
                  :form (->Reduced
                          (if sval
                            (wrap sval)
                            spec))})

               (and (seq? spec)
                    (#{`dotted-pretype} (first spec)))
               (let [[_ pretype tv & {:keys [wrap]}] spec
                     wrap (if wrap
                            (eval wrap)
                            #(coerce-dotted-cat tv %))
                     sval (sbst tv)
                     _ (assert (simple-keyword? tv))
                     _ (assert ((some-fn nil? sequential?) sval)
                               sval)]
                 (if sval
                   (let [;_ (prn "substituting dotted-pretype" sval)
                         ; returns a vector for fast indexing
                         spretypes (mapv ; vars bound by dotted variable shadow others
                                         #(subst-tvar pretype (merge sbst %))
                                         sval)
                         ;_ (prn "spretypes" spretypes)
                         wrapped (wrap spretypes)
                         ;_ (prn "wrapped" wrapped)
                         ]
                     {:config sbst
                      :form (->Reduced wrapped)})
                   {:config sbst
                    :form (->Reduced spec)}))

               :else
               {:config sbst
                :form spec}))
           sbst
           spec))

(comment
  (subst-tvar `(list (tvar :x :... (tvar :x)))
              {:x [1 2 3]})
  (subst-tvar `(list (tvar :x :... (tvar :x)))
              {:x [1 2 3]})
  )

; binder and body are unresolved
(defn- tfn-impl
  [binder body]
  (let [;if this tfn hasn't been instantiated with tapp, generalize args
        generalized (delay
                      (let [default-subst-map (into {}
                                                    (map (fn [[name {:keys [variance super sub]
                                                                     :or {super `any?
                                                                          sub `(s/or)}}]]
                                                           (let [rspec (case variance
                                                                         :covariant super
                                                                         :contravariant sub)]
                                                             [name rspec])))
                                                    binder)]
                        (subst-tvar body default-subst-map)))]
    (reify
      ITypeFn
      (apply-typefn* [_ subst-map]
        (-> body
            (subst-tvar subst-map)
            s/resolve-spec))

      ;only describe* makes sense for tfn. everything else uses generalized body.
      Spec
      (conform* [_ x settings-key settings]
        (conform* @generalized x settings-key settings))
      (unform* [_ x]
        (unform* @generalized x))
      (explain* [_ path via in x settings-key settings]
        (explain* @generalized path via in x settings-key settings))
      (gen* [_ overrides path rmap]
        (gen* @generalized overrides path rmap))
      (with-gen* [_ gfn]
        (with-gen* @generalized gfn))
      (describe* [_]
        `(tfn ~binder ~body)))))

#_
(defn- validate-poly-fn
  "returns f if valid, else smallest"
  [f specs iters]
  (let [g (s/gen (:args specs))
        prop (gen/for-all* [g] #(call-valid? f specs %))]
    (let [ret (gen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        f))))

(comment
  (gen/generate (s/gen (s/resolve-spec `integer?)))
  (gen/generate
    (s/gen
      (s/resolve-spec
        `(tvar-spec :position #{:input :output}
                    :lower (s/or)
                    :upper any?))))
  (gen/generate
    (s/gen
      (s/resolve-spec
        `(s/tuple #{:x} (tvar-spec :position #{:input :output}
                                   :lower (s/or)
                                   :upper any?)))))
  (s/conform
    (s/resolve-spec
      `(all [:x (tvar-spec :position #{:input :output}
                           :lower (s/or)
                           :upper any?)
             :y (tvar-spec :position #{:input :output}
                           :lower (s/or)
                           :upper any?)]
            (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (tvar :x))
                                               :ret (tvar :y))
                                  :coll (s/coll-of (tvar :x)))
                     :ret (s/coll-of (tvar :y)))))
    #_map
    (fn [f c]
      [1 2]))

  (-> (tvar-spec)
      s/gen
      gen/generate)
  (s/conform
    (s/resolve-spec `symbol?)
    (gensym))
  (-> (clojure.test-clojure.typed/tvar-spec :upper symbol?)
      s/form)
  (-> (clojure.test-clojure.typed/tvar-spec :upper symbol?)
      s/gen
      gen/generate)
  (-> (gen/return `integer?)
      gen/generate)

  (s/conform (s/resolve-spec `clojure.core/symbol?) 'a)

  (s/conform
    (s/resolve-spec
      `(all [:x (tvar-spec :position #{:input :output}
                           :lower (s/or)
                           :upper symbol?)
             :y (tvar-spec :position #{:input :output}
                           :lower (s/or)
                           :upper symbol?)]
            (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (tvar :x))
                                               :ret (tvar :y))
                                  :coll (s/coll-of (tvar :x)))
                     :ret (s/coll-of (tvar :y)))))
    map)

  (s/conform
    (s/describe
    (s/resolve-spec
      `(inst
         (all [:x (tvar-spec)
               :y (s/with-gen
                    (tvar-spec)
                    #(gen/return
                       '~`(tvar :x)))]
              (s/fspec :args (s/cat :pred (s/with-gen
                                            (s/fspec :args (s/cat :in (tvar :x))
                                                     :fn (fn [{[~'in] :args :keys [~'ret]}]
                                                           (= (boolean ~'ret)
                                                              (s/valid? (s/resolve-spec '~`(tvar :y)) ~'in)))
                                                     :ret any?)
                                            #(gen/return
                                               (fn [~'x]
                                                 (gen/generate
                                                   (if (s/valid? (s/resolve-spec '~`(tvar :y)) ~'x)
                                                     (gen/such-that identity (gen/any-printable))
                                                     (gen/one-of [(gen/return nil)
                                                                  (gen/return false)]))))))
                                    :coll (s/nilable (s/coll-of (tvar :x))))
                       :ret (s/coll-of (tvar :y))))
         {:x number?
          :y integer?})))
    filter)

  (s/conform
    `(all [:x (tvar-spec :dotted true
                         :position #{:input :output}
                         :lower (s/or)
                         :upper any?)]
          (s/fspec :args (s/cat :els (s/spec (s/tuple (tvar :x :... (tvar :x)))))
                   :ret (s/tuple (tvar :x :... (tvar :x)))))
    vector)

  (s/tuple (tvar... :y (tvar :y)))
  )

; assumes lower <: upper for the moment
; takes unresolved spec forms
(defn- generator-between [lower upper]
  (let [g (gensym)]
    (if (s/valid? (s/resolve-spec upper) g)
      (gen/return #{g})
      (throw (ex-info "Limitation: can only instantiate type variable with symbol currently"
                      {:lower lower
                       :upper upper})))))

; all unresolved
(defn- tvar-spec-impl
  [tv position kind gen gfn]
  (let []
    (reify
      Spec
      (conform* [_ x settings-key settings]
        (conform* (s/resolve-spec kind)
                  x
                  settings-key
                  settings))
      (unform* [_ x]
        (assert nil "TODO")
        )
      (explain* [_ path via in x settings-key settings]
        (assert nil "TODO")
        )
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (if gen
            ((eval gen))
            (-> kind s/resolve-spec s/gen))))
      (with-gen* [_ gfn]
        (tvar-spec-impl tv position kind nil gfn))
      (describe* [_]
        `(tvar-spec :name ~tv
                    :position ~position
                    :kind ~kind
                    :gen ~gen)))))

(def ^:dynamic *all-iterations* 10)

;based on clojure.alpha.spec.impl/validate-fn
(defn- validate-all
  "returns x if valid, else smallest"
  [x binder body iters]
  (let [g (-> binder s/resolve-spec s/gen)
        prop (gen/for-all* [g] #(let [rbody (-> body (subst-tvar %) s/resolve-spec)]
                                  ;(prn "subst" %)
                                  ;(prn "before body" (-> body s/resolve-spec s/describe))
                                  ;(prn "body" (s/describe rbody))
                                  (s/valid? rbody x)))]
    (let [ret (gen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        x))))

(defn- all-impl
  [binder body gfn]
  (let []
    (reify
      IPoly
      (inst* [_ subst-map]
        (let [sbst (s/conform (s/resolve-spec binder) subst-map)]
          (if (s/invalid? sbst)
            (throw (ex-info "Invalid substitution"
                            {:binder binder
                             :subst-map subst-map}))
            (-> body
                (subst-tvar sbst)
                s/resolve-spec))))

      Spec
      (conform* [_ x settings-key settings]
        (if (and binder body)
          (if (identical? x (validate-all x binder body *all-iterations*))
            x
            ::s/invalid)
          (throw (ex-info "Can only conform 'all' with binder and body"
                          {}))))
      (unform* [_ x]
        x)
      (explain* [_ path via in x settings-key settings]
        (assert nil))
      (gen* [_ overrides path rmap]
        (assert nil))
      (with-gen* [_ gfn]
        (all-impl binder body gfn))
      (describe* [_]
        `(all ~binder ~body)))))

(defn- binder-impl
  [tvs gfn]
  (let []
    (reify
      Spec
      ; (conform (binder :x (tvar-spec))
      ;          `{:x integer?})
      ; => `{:x integer?}
      (conform* [_ x settings-key settings]
        ;note: doesn't blow up if x has extra entries
        (if (not (map? x))
          ::s/invalid
          (reduce (fn [smap [b v]]
                    (let [rsspec (s/resolve-spec (subst-tvar v smap))]
                      (if-let [[_ i] (find x b)]
                        (let [ic (conform* rsspec
                                           i
                                           settings-key
                                           settings)]
                          (if (s/invalid? ic)
                            (reduced ic)
                            (assoc smap b ic)))
                        ; infer missing entries
                        (assoc smap b (-> rsspec s/gen gen/generate)))))
                  {}
                  (partition 2 tvs))))
      (unform* [_ x]
        (assert nil))
      (explain* [_ path via in x settings-key settings]
        (assert nil))
      (gen* [_ overrides path rmap]
        (let [; (s/coll-of (s/tuple Kw Spec)) (s/map-of Kw Spec) -> Generator
              bgen (fn bgen [tvs gvs]
                     (if (empty? tvs)
                       (gen/return gvs)
                       (let [[[b v] & tvs] tvs]
                         (gen/bind (-> v
                                       (subst-tvar gvs)
                                       s/resolve-spec
                                       (gen* overrides path rmap))
                                   (fn [gv]
                                     (bgen tvs (assoc gvs b gv)))))))]
          ; allows the first gen* to be chosen multiple times
          (gen/bind (gen/return nil)
                    (fn [_]
                      (bgen (partition 2 tvs) {})))))
      (with-gen* [_ gfn]
        (binder-impl tvs gfn))
      (describe* [_]
        `(binder ~@tvs)))))

(comment
  (s/conform (binder :x (tvar-spec))
             {:x #{(gensym)}})
  (s/conform (binder :x (tvar-spec)
                     :y (tvar-spec))
             {:x #{(gensym)}})
  (s/conform (binder :x (tvar-spec :kind nat-int?)
                     :y (tvar-spec :kind nat-int?))
             {:x 24})
  (s/conform (binder :x (tvar-spec :kind nat-int?)
                     :y (tvar-spec :kind nat-int?))
             {:x 24
              :y 42})
  (s/conform (binder :x (tvar-spec :kind nat-int?)
                     :y (tvar-spec :kind (s/int-in 0 (tvar :x))))
             {:x 2})
  (s/conform (binder :x (tvar-spec :kind (s/int-in 0 5))
                     :y (tvar-spec :kind (s/coll-of integer?
                                                    :count (tvar :x))))
             {:x 4})
  (gen/sample
    (s/gen (binder :x (tvar-spec)
                   :y (tvar-spec))))
  (gen/sample
    (s/gen (binder :x (tvar-spec :kind nat-int?)
                   :y (tvar-spec :kind nat-int?))))

  (gen/sample
    (gen/keyword))

  (gen/sample
    (s/gen (binder)))

  (s/conform (all :binder (binder :x (tvar-spec))
                  :body (tvar :x))
             1)

  (s/conform (all :binder (binder :x (tvar-spec))
                  :body (s/fspec :args (s/cat :x (tvar :x))
                                 :ret (tvar :x)))
             (fn [a]
               (prn a)
               a))
  )

(defn- tvar-impl
  [tv wrap]
  (reify
    Spec
    (conform* [_ x settings-key settings]
      (assert nil "conform"))
    (unform* [_ x]
      (assert nil "unform"))
    (explain* [_ path via in x settings-key settings]
      (assert nil "explain*"))
    (gen* [_ overrides path rmap]
      (assert nil "gen*"))
    (with-gen* [_ gfn]
      (assert nil "with-gen*"))
    (describe* [_]
      `(tvar ~tv ~@(some-> wrap (vector :wrap))))))

;Notes:
; - reread doc/infer-detail.md to remind myself about splicing dotted and a starred pretypes
; - they're more general than dotted pretypes and the deeper abstraction I'm looking for
;   to generate dotted-pretype is probably easier to find with a spliced-dotted-pretype op
(defn- dotted-pretype-impl
  [pretype tv wrap]
  (reify
    Spec
    (conform* [_ x settings-key settings]
      (assert nil "conform"))
    (unform* [_ x]
      (assert nil "unform"))
    (explain* [_ path via in x settings-key settings]
      (assert nil "explain*"))
    (gen* [_ overrides path rmap]
      (assert nil "gen*"))
    (with-gen* [_ gfn]
      (assert nil "with-gen*"))
    (describe* [_]
      `(dotted-pretype ~pretype ~tv :wrap ~wrap))))

(defn- spec-between-impl
  [lower upper gfn]
  (reify
    Spec
    (conform* [_ x settings-key settings]
      (if (set? x)
        (into #{}
              (map #(conform* (s/resolve-spec upper)
                              %
                              settings-key
                              settings))
              x)
        (assert nil (str "spec-between-impl conform: " (pr-str x)))))
    (unform* [_ x]
      (assert nil "unform"))
    (explain* [_ path via in x settings-key settings]
      (assert nil "explain*"))
    (gen* [_ overrides path rmap]
      (if gfn
        (gfn)
        (generator-between lower upper)))
    (with-gen* [_ gfn]
      (spec-between-impl lower upper gfn))
    (describe* [_]
      `(spec-between :lower ~lower
                     :upper ~upper))))

(defmethod s/expand-spec `tfn
  [[_ binder body & more]]
  {:pre [binder
         body
         (not more)]}
  {:clojure.spec/op `tfn
   :binder binder
   :body body})

(defmethod s/create-spec `tfn
  [{:keys [binder body]}]
  (tfn-impl binder body))

(defmethod s/expand-spec `tvar-spec
  [[_ & {tv :name
         :keys [position kind gen]
         :or {position #{:input :output}
              kind ::any-spec?}
         :as opt}]]
  {:pre [(set? position)
         ((some-fn nil? simple-keyword?) tv)
         (empty? (apply dissoc opt [:name :position :kind :gen]))]}
  {:clojure.spec/op `tvar-spec
   :tv tv
   :position position
   :kind kind
   :gen gen})

(defmethod s/create-spec `tvar-spec
  [{:keys [tv position kind gen]}]
  (tvar-spec-impl tv position kind gen nil))

(defmethod s/expand-spec `all
  [[_ & {:keys [binder body]}]]
  {:pre [binder
         body]}
  {:clojure.spec/op `all
   :binder binder
   :body body})

(defmethod s/create-spec `all
  [{:keys [binder body]}]
  (all-impl binder body nil))

(defmethod s/expand-spec `binder
  [[_ & tvs]]
  {:pre []}
  {:clojure.spec/op `binder
   :tvs tvs})

(defmethod s/create-spec `binder
  [{:keys [tvs]}]
  (binder-impl tvs nil))

(defmethod s/expand-spec `tapp
  [[_ tfn args-map & more]]
  {:pre [tfn
         args-map
         (not more)]}
  {:clojure.spec/op `tapp
   :tfn tfn
   :args-map args-map})

(defmethod s/create-spec `tapp
  [{:keys [tfn args-map]}]
  (tapp-impl (s/resolve-spec tfn)
             args-map))

(defmethod s/expand-spec `inst
  [[_ poly args-map & more]]
  {:pre [poly
         args-map
         (not more)]}
  {:clojure.spec/op `inst
   :poly poly
   :args-map args-map})

(defmethod s/create-spec `inst
  [{:keys [poly args-map]}]
  (inst-impl (s/resolve-spec poly)
             args-map))

(defmethod s/expand-spec `tvar
  [[_ tv & {:keys [wrap] :as opt}]]
  {:pre [(simple-keyword? tv)
         (empty? (dissoc opt :wrap))]}
  {:clojure.spec/op `tvar
   :tv tv
   :wrap wrap})

(defmethod s/create-spec `tvar
  [{:keys [tv wrap]}]
  (tvar-impl tv wrap))

(defmethod s/expand-spec `dotted-pretype
  [[_ pretype tv & opt]]
  {:pre [(simple-keyword? tv)]}
  (let [{:keys [wrap]
         :or {wrap `(fn [xs#]
                      (coerce-dotted-cat ~tv xs#))}}
        opt]
    {:clojure.spec/op `dotted-pretype
     :pretype pretype
     :tv tv
     :wrap wrap}))

(defmethod s/create-spec `dotted-pretype
  [{:keys [pretype tv wrap]}]
  (dotted-pretype-impl pretype tv wrap))

(defmethod s/expand-spec `spec-between
  [[_ & {:keys [upper lower]
         :or {upper `any?
              lower `(s/or)}}]]
  {:clojure.spec/op `spec-between
   :lower lower
   :upper upper})

(defmethod s/create-spec `spec-between
  [{:keys [lower upper]}]
  (spec-between-impl lower upper nil))

(s/def ::any-spec? (spec-between))

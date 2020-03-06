;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.spec.impl
  "Implementation details for typed.clj.spec"
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [typed.clj.spec :as t]
            [typed.clj.spec.protocols
             :refer [ITypeFn apply-typefn*
                     IPoly inst*]]
            [clojure.walk :as walk]
            [clojure.alpha.spec.gen :as gen]))

(set! *warn-on-reflection* true)

(deftype Reduced [val])

; modified from clojure.walk
(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall.
  On prewalk, short-circuits on Reduced. `outer` should not return Reduced."
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

(declare subst-tv)

(defn subst-binder [binder sbst]
  {:pre [(map? sbst)
         (seq? binder)
         (#{`t/binder} (first binder))
         (even? (count (rest binder)))]
   :post [(vector? %)
          (seq? (nth % 0))
          (map? (nth % 1))]}
  (-> (reduce (fn [[bpairs sbst] [b v]]
                {:pre [(vector? bpairs)
                       (map? sbst)
                       (simple-keyword? b)
                       (seq? v)
                       (#{`t/bind-tv} (first v))]
                 :post [(vector? %)
                        (vector? (nth % 0))
                        (map? (nth % 1))]}
                (let [[_ & {self-name :name}] v]
                  [(conj bpairs
                         [b (subst-tv v
                                        (cond-> sbst
                                          self-name (dissoc self-name)))])
                   (dissoc sbst b)]))
              [[] sbst]
              (partition 2 (rest binder)))
      (update 0 (fn [bpairs]
                  `(t/binder ~@(mapcat identity bpairs))))))

(defn subst-tv-all-like [spec sbst]
  (let [[_ & {:keys [binder body] :as opt}] spec
        extra-keys (set (keys (dissoc opt :binder :body)))
        _ (when (seq extra-keys)
            (throw (ex-info (str "Found extra keys to " (first spec) ": " extra-keys)
                            {:form spec
                             :extra-keys extra-keys})))
        [binder sbst] (subst-binder binder sbst)
        body (subst-tv body sbst)]
    (list (first spec) :binder binder :body body)))

(defmulti subst-tv-op
  "Extension point for type-variable substitution over specs.
  Extend this multimethod if your spec interacts with type variable
  scope.
  
  To extend to symbolic spec with form (my.ns/foobar spec),
  extend subst-tv-op with a 'my.ns/foobar method that returns
  a fully-substituted spec wrt substitution sbst.

  eg., (defmethod subst-tv-op `my.ns/foobar
         [[_ spec] sbst {:keys [subst-tv] :as opt}]
         (subst-tv spec sbst))
  
  sbst is a map from type variable names to values to substitute
  for them.

  opt map consists of:
  - :subst-tv    a function taking a spec form and a substitution and
                 returns a spec form after applying the substitution.
  "
  (fn [spec sbst opt]
    (and (seq? spec)
         (first spec))))

(defmethod subst-tv-op `t/tfn [spec sbst _] (subst-tv-all-like spec sbst))
(defmethod subst-tv-op `t/all [spec sbst _] (subst-tv-all-like spec sbst))

(defmethod subst-tv-op `t/tv
  [spec sbst _]
  (let [[_ tv & {:keys [wrap]
                 :or {wrap `identity}}]
        spec
        _ (assert (simple-keyword? tv))
        wrap (eval wrap) ;don't substitute tvs in wrap's form
        [_ sval :as found] (find sbst tv)]
    ;(prn "tv" tv sval)
    (if found
      ; substitute wrap's expansion.
      ; note: infinite loop possible, might need Rec spec.
      (subst-tv (wrap sval) sbst)
      spec)))

(defn coerce-dotted-cat [tv xs]
  {:pre [(simple-keyword? tv)]}
  ;(prn "coerce-dotted-cat" tv xs)
  `(s/cat ~@(mapcat (fn [i x]
                      [(keyword (str (name tv) i)) x])
                    (range)
                    xs)))

(comment
  (coerce-dotted-cat :a `[integer? boolean?])
  (coerce-dotted-cat :x '[(clojure.alpha.spec/every clojure.core/boolean? :count 35)])
  )

(defmethod subst-tv-op `t/fold-binders
  [spec sbst opt]
  (let [[_ template tv & {:keys [wrap]}] spec
        wrap (if wrap
               (eval wrap)
               #(coerce-dotted-cat tv %))
        [_ sval :as found] (find sbst tv)
        _ (assert (simple-keyword? tv))
        _ (assert ((some-fn nil? sequential?) sval)
                  sval)]
    (if found
      (let [;_ (prn "substituting fold-binders" sval)
            ; returns a vector for fast indexing
            stemplates (mapv ; vars bound by dotted variable shadow others
                            #(subst-tv template (merge sbst %))
                            sval)
            ;_ (prn "stemplates" stemplates)
            wrapped (wrap stemplates)
            ;_ (prn "wrapped" wrapped)
            ]
         wrapped)
       spec)))

;TODO rigorous testing for capture-avoidance
(defn subst-tv [spec sbst]
  {:pre [(map? sbst)]}
  (prewalk (fn [sbst spec]
             {:pre [(map? sbst)]}
             (if (and (seq? spec)
                      (contains? (methods subst-tv-op) (first spec)))
               {:config sbst
                :form (->Reduced (subst-tv-op spec sbst {:subst-tv subst-tv}))}
               {:config sbst
                :form spec}))
           sbst
           spec))

(comment
  (subst-tv `(list (t/tv :x :... (t/tv :x)))
              {:x [1 2 3]})
  (subst-tv `(list (t/tv :x :... (t/tv :x)))
              {:x [1 2 3]})
  )

; binder and body are unresolved
(defn- tfn-impl
  [binder body gfn]
  (let []
    (reify
      ITypeFn
      (apply-typefn* [_ subst-map]
        (let [bs (s/resolve-spec binder)
              sbst (s/conform bs subst-map)]
          (if (s/invalid? sbst)
            (let [ed (s/explain-data bs subst-map)]
              (throw (ex-info (str "Invalid substitution"
                                   (when ed
                                     (str "\n"
                                          (with-out-str
                                            (s/explain-out ed)))))
                              {:binder binder
                               :explain-data ed
                               :subst-map subst-map})))
            (-> body
                (subst-tv subst-map)
                s/resolve-spec))))

      ;only describe* makes sense for tfn. everything else uses generalized body.
      Spec
      (conform* [_ x settings-key settings]
        (assert nil "conform TFn"))
      (unform* [_ x]
        (assert nil "unform TFn"))
      (explain* [_ path via in x settings-key settings]
        (assert nil "explain TFn"))
      (gen* [_ overrides path rmap]
        (assert nil "gen TFn"))
      (with-gen* [_ gfn]
        (tfn-impl binder body gfn))
      (describe* [_]
        `(t/tfn :binder ~binder :body ~body)))))

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
        `(t/bind-tv :position #{:input :output}
                    :lower (s/or)
                    :upper any?))))
  (gen/generate
    (s/gen
      (s/resolve-spec
        `(s/tuple #{:x} (t/bind-tv :position #{:input :output}
                                   :lower (s/or)
                                   :upper any?)))))
  (s/conform
    (s/resolve-spec
      `(t/all [:x (t/bind-tv :position #{:input :output}
                           :lower (s/or)
                           :upper any?)
             :y (t/bind-tv :position #{:input :output}
                           :lower (s/or)
                           :upper any?)]
            (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (t/tv :x))
                                               :ret (t/tv :y))
                                  :coll (s/coll-of (t/tv :x)))
                     :ret (s/coll-of (t/tv :y)))))
    #_map
    (fn [f c]
      [1 2]))

  (-> (t/bind-tv)
      s/gen
      gen/generate)
  (s/conform
    (s/resolve-spec `symbol?)
    (gensym))
  (-> (clojure.test-clojure.typed/bind-tv :upper symbol?)
      s/form)
  (-> (clojure.test-clojure.typed/bind-tv :upper symbol?)
      s/gen
      gen/generate)
  (-> (gen/return `integer?)
      gen/generate)

  (s/conform
    (s/describe
    (s/resolve-spec
      `(t/inst
         (t/all [:x (t/bind-tv)
               :y (s/with-gen
                    (t/bind-tv)
                    #(gen/return
                       '~`(t/tv :x)))]
              (s/fspec :args (s/cat :pred (s/with-gen
                                            (s/fspec :args (s/cat :in (t/tv :x))
                                                     :fn (fn [{[~'in] :args :keys [~'ret]}]
                                                           (= (boolean ~'ret)
                                                              (s/valid? (s/resolve-spec '~`(t/tv :y)) ~'in)))
                                                     :ret any?)
                                            #(gen/return
                                               (fn [~'x]
                                                 (gen/generate
                                                   (if (s/valid? (s/resolve-spec '~`(t/tv :y)) ~'x)
                                                     (gen/such-that identity (gen/any-printable))
                                                     (gen/one-of [(gen/return nil)
                                                                  (gen/return false)]))))))
                                    :coll (s/nilable (s/coll-of (t/tv :x))))
                       :ret (s/coll-of (t/tv :y))))
         {:x number?
          :y integer?})))
    filter)

  (s/conform
    `(t/all [:x (t/bind-tv :dotted true
                         :position #{:input :output}
                         :lower (s/or)
                         :upper any?)]
          (s/fspec :args (s/cat :els (s/spec (s/tuple (t/tv :x :... (t/tv :x)))))
                   :ret (s/tuple (t/tv :x :... (t/tv :x)))))
    vector)

  (s/tuple (t/tv... :y (t/tv :y)))
  )

(def sample-seq
  (delay
    (requiring-resolve 'clojure.test.check.generators/sample-seq)))

; assumes lower <: upper and lower == (or) for the moment
; takes unresolved spec forms
(defn- generator-between [lower upper]
  (let [rupper (s/resolve-spec upper)
        rgen (s/gen rupper)
        tries 100
        ncandidates 20
        ; try and find ncandidates varied constant values
        gs (into []
                 (comp
                   (take tries)
                   (filter #(try ;ensure it's equivable
                                 (s/valid? (s/resolve-spec #{%}) (walk/prewalk identity %))
                                 (catch Error e)))
                   (distinct)
                   (take ncandidates))
                 (@sample-seq rgen))]
    (if (seq gs)
      (gen/return #{(rand-nth gs)})
      (throw (ex-info (str "Could not find a constant value between lower and upper "
                           "after " tries " tries")
                      {:lower lower
                       :upper upper
                       :tries tries
                       :ncandidates ncandidates})))))

; all unresolved
(defn- bind-tv-impl
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
        (explain* (s/resolve-spec kind) (conj path :kind) (conj via `t/bind-tv) in x settings-key settings))
      (gen* [_ overrides path rmap]
        (if gfn
          (gfn)
          (if gen
            ((eval gen))
            (-> kind s/resolve-spec s/gen))))
      (with-gen* [_ gfn]
        (bind-tv-impl tv position kind nil gfn))
      (describe* [_]
        `(t/bind-tv ~@(some->> tv (vector :name))
                    :position ~position
                    :kind ~kind
                    ~@(some->> gen (vector :gen)))))))

; cases = (Map Int v)
; default = (U nil v)
(defn- fcase-impl
  [cases default gfn]
  (let []
    (reify
      Spec
      (conform* [_ x settings-key settings]
        (reduce (fn [x spec]
                  (let [res (conform* (s/resolve-spec spec)
                                      x
                                      settings-key
                                      settings)]
                    (if (s/invalid? res)
                      (reduced res)
                      res)))
                x
                (concat (vals cases)
                        (some-> default vector))))
      (unform* [_ x]
        (assert nil "TODO")
        )
      (explain* [_ path via in x settings-key settings]
        (assert nil "TODO")
        )
      (gen* [_ overrides _ _]
        (if gfn
          (gfn)
          (gen/return
            (fn [& args]
              (let [spec (loop [cases cases
                                n 0
                                args-tail args]
                           (if (empty? cases)
                             (or default
                                 (throw (ex-info (str "No fcase match for " n " or more arguments")
                                                 {:n n
                                                  :cases cases
                                                  :args args})))
                             (if (empty? args-tail)
                               (or (cases n)
                                   default
                                   (throw (ex-info (str "No fcase match for " n " arguments")
                                                   {:n n
                                                    :cases cases
                                                    :args args})))
                               (recur (dissoc cases n)
                                      (inc n) 
                                      (next args-tail)))))
                    gf (-> spec
                           s/resolve-spec
                           (s/gen overrides)
                           gen/generate)]
                (apply gf args))))))
      (with-gen* [_ gfn]
        (fcase-impl cases default gfn))
      (describe* [_]
        `(t/fcase ~@(mapcat (fn [[v gs]]
                            (let [ks (set (map first gs))]
                              [(if (= 1 (count ks))
                                 (first ks)
                                 (set ks))
                               v]))
                          (group-by val cases))
                ~@(some-> default vector))))))

(def ^:dynamic *all-iterations* 10)

;based on clojure.alpha.spec.impl/validate-fn
(defn- validate-all
  "returns x if valid, else smallest"
  [x {:keys [binder body]} iters]
  (let [g (-> binder s/resolve-spec s/gen)
        prop (gen/for-all* [g] #(let [rbody (-> body (subst-tv %) s/resolve-spec)]
                                  ;(prn "subst" %)
                                  ;(prn "before body" (-> body s/resolve-spec s/describe))
                                  ;(prn "body" (s/describe rbody))
                                  (s/valid? rbody x)))]
    (let [ret (gen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        x))))

;FIXME roundtrip (s/spec (t/all ..)) and (s/spec (t/tfn ..))
(defn- all-impl
  [binder body gfn]
  (let [specs {:binder binder
               :body body}]
    (reify
      IPoly
      (inst* [_ subst-map]
        (let [bs (s/resolve-spec binder)
              ;_ (prn "inst*" subst-map)
              sbst (s/conform bs subst-map)]
          (if (s/invalid? sbst)
            (let [ed (s/explain-data bs subst-map)]
              (throw (ex-info (str "Invalid substitution"
                                   (when ed
                                     (str "\n"
                                          (with-out-str
                                            (s/explain-out ed)))))
                              {:binder binder
                               :explain-data ed
                               :subst-map subst-map})))
            (-> body
                (subst-tv sbst)
                s/resolve-spec))))

      Spec
      (conform* [_ x settings-key settings]
        (if (and binder body)
          (if (identical? x (validate-all x specs *all-iterations*))
            x
            ::s/invalid)
          (throw (ex-info "Can only conform 't/all' with binder and body"
                          {}))))
      (unform* [_ x] x)
      (explain* [_ path via in x settings-key settings]
        (let [subst (validate-all x specs 100)]
          (if (identical? x subst) ;;hrm, we might not be able to reproduce
            nil
            (let [thrown (atom nil)
                  inst-body (try (s/resolve-spec `(t/inst ~body ~subst))
                                 (catch Throwable t (reset! thrown t)))]
              (if-some [e @thrown]
                ;;TODO add exception data
                [{:path path :pred `inst :val subst :reason (.getMessage ^Throwable e) :via via :in in}]
                [{:path path :pred `all :val {:subst subst :inst-body inst-body} :via via :in in}])))))
      (gen* [_ overrides path rmap]
        (assert nil))
      (with-gen* [_ gfn]
        (all-impl binder body gfn))
      (describe* [_]
        `(t/all ~binder ~body)))))

(defn- binder-impl
  [tvs gfn]
  {:pre [(even? (count tvs))
         (apply distinct? (map first (partition 2 tvs)))]}
  (let []
    (reify
      Spec
      ; (conform (t/binder :x (t/bind-tv))
      ;          `{:x integer?})
      ; => `{:x integer?}
      (conform* [_ x settings-key settings]
        ;note: doesn't blow up if x has extra entries
        (if (not (map? x))
          ::s/invalid
          (reduce (fn [smap [b v]]
                    (let [rsspec (s/resolve-spec (subst-tv v smap))]
                      (if-let [[_ i] (find x b)]
                        (let [ic (conform* rsspec
                                           i
                                           settings-key
                                           settings)]
                          (if (s/invalid? ic)
                            (reduced ic)
                            (assoc smap b ic)))
                        ; infer missing entries
                        (let [;_ (prn "gv" (s/describe rsspec))
                              gv (try (-> rsspec s/gen gen/generate)
                                      (catch Exception e
                                        (throw (ex-info (str "Error catch while generating default instantiation"
                                                             " for" (s/describe rsspec))
                                                        {:spec (s/form rsspec)
                                                         :smap smap
                                                         :b b}
                                                        e))))]
                          (assoc smap b gv)))))
                  {}
                  (partition 2 tvs))))
      (unform* [_ x]
        (assert nil))
      (explain* [_ path via in x settings-key settings]
        ;note: doesn't blow up if x has extra entries
        (if (not (map? x))
          [{:path path :pred `map? :val x :via (conj via `t/binder) :in in}]
          (let [problem (atom nil)]
            (reduce (fn [smap [b v]]
                      (let [rsspec (s/resolve-spec (subst-tv v smap))]
                        (if-let [[_ i] (find x b)]
                          (let [ic (conform* rsspec
                                             i
                                             settings-key
                                             settings)]
                            (if (s/invalid? ic)
                              (reduced (reset! problem (explain* rsspec (conj path b) (conj via `t/binder) in i settings-key settings)))
                              (assoc smap b ic)))
                          ; infer missing entries
                          (assoc smap b (-> rsspec s/gen gen/generate)))))
                    {}
                    (partition 2 tvs))
            @problem)))
      (gen* [_ overrides path rmap]
        (let [; (s/coll-of (s/tuple Kw Spec)) (s/map-of Kw Spec) -> Generator
              bgen (fn bgen [tvs gvs]
                     (if (empty? tvs)
                       (gen/return gvs)
                       (let [[[b v] & tvs] tvs]
                         (gen/bind (-> v
                                       (subst-tv gvs)
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
        `(t/binder ~@tvs)))))

(comment
  (s/conform (t/binder :x (t/bind-tv))
             {:x #{(gensym)}})
  (s/conform (t/binder :x (t/bind-tv)
                     :y (t/bind-tv))
             {:x #{(gensym)}})
  (s/conform (t/binder :x (t/bind-tv :kind nat-int?)
                     :y (t/bind-tv :kind nat-int?))
             {:x 24})
  (s/conform (t/binder :x (t/bind-tv :kind nat-int?)
                     :y (t/bind-tv :kind nat-int?))
             {:x 24
              :y 42})
  (s/conform (t/binder :x (t/bind-tv :kind nat-int?)
                     :y (t/bind-tv :kind (s/int-in 0 (t/tv :x))))
             {:x 2})
  (s/conform (t/binder :x (t/bind-tv :kind (s/int-in 0 5))
                     :y (t/bind-tv :kind (s/coll-of integer?
                                                    :count (t/tv :x))))
             {:x 4})
  (gen/sample
    (s/gen (t/binder :x (t/bind-tv)
                   :y (t/bind-tv))))
  (gen/sample
    (s/gen (t/binder :x (t/bind-tv :kind nat-int?)
                   :y (t/bind-tv :kind nat-int?))))

  (gen/sample
    (gen/keyword))

  (gen/sample
    (s/gen (t/binder)))

  (s/conform (t/all :binder (t/binder :x (t/bind-tv))
                  :body (t/tv :x))
             1)

  (s/conform (t/all :binder (t/binder :x (t/bind-tv))
                  :body (s/fspec :args (s/cat :x (t/tv :x))
                                 :ret (t/tv :x)))
             (fn [a]
               (prn a)
               a))
  )

;Notes:
; - reread doc/infer-detail.md to remind myself about splicing dotted and a starred pretypes
; - they're more general than dotted pretypes and the deeper abstraction I'm looking for
;   to generate fold-binders is probably easier to find with a spliced-fold-binders op
(defn- fold-binders-impl
  [template tv wrap]
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
      `(t/fold-binders ~template ~tv :wrap ~wrap))))

(defn- spec-between-impl
  [lower upper gfn]
  (let [rupper (delay (s/resolve-spec upper))]
    (reify
      Spec
      (conform* [_ x settings-key settings]
        (if (set? x)
          (into #{}
                (map #(conform* @rupper
                                %
                                settings-key
                                settings))
                x)
          ;FIXME use a subtyping relation instead of this brittle hack
          (if (every? #(s/valid? @rupper %) (-> x s/resolve-spec s/gen (gen/sample 10)))
            x
            (assert nil (str "spec-between-impl conform: " (pr-str x))))))
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
        `(t/spec-between :lower ~lower
                         :upper ~upper)))))

(defmethod s/expand-spec `t/tfn
  [[_ & {:keys [binder body] :as opt}]]
  {:pre [(empty? (dissoc opt :binder :body))]}
  {:clojure.spec/op `t/tfn
   :binder binder
   :body body})

(defmethod s/create-spec `t/tfn
  [{:keys [binder body]}]
  (tfn-impl binder body nil))

(defmethod s/expand-spec `t/bind-tv
  [[_ & {tv :name
         :keys [position kind gen]
         :or {position #{:input :output}
              kind `(t/spec-between)}
         :as opt}]]
  {:pre [(set? position)
         ((some-fn nil? simple-keyword?) tv)
         (empty? (apply dissoc opt [:name :position :kind :gen]))]}
  {:clojure.spec/op `t/bind-tv
   :tv tv
   :position position
   :kind kind
   :gen gen})

(defmethod s/create-spec `t/bind-tv
  [{:keys [tv position kind gen]}]
  (bind-tv-impl tv position kind gen nil))

(defmethod s/expand-spec `t/all
  [[_ & {:keys [binder body]}]]
  {:pre [binder
         body]}
  {:clojure.spec/op `t/all
   :binder binder
   :body body})

(defmethod s/create-spec `t/all
  [{:keys [binder body]}]
  (all-impl binder body nil))

(defmethod s/expand-spec `t/binder
  [[_ & tvs]]
  {:pre []}
  {:clojure.spec/op `t/binder
   :tvs tvs})

(defmethod s/create-spec `t/binder
  [{:keys [tvs]}]
  (binder-impl tvs nil))

(defmethod s/expand-spec `t/tapp
  [[_ tfn args-map & more]]
  {:pre [tfn
         args-map
         (not more)]}
  {:clojure.spec/op `t/tapp
   :tfn tfn
   :args-map args-map})

(defmethod s/create-spec `t/tapp
  [{:keys [tfn args-map]}]
  (apply-typefn* (s/resolve-spec tfn) args-map))

(defmethod s/expand-spec `t/inst
  [[_ poly args-map & more]]
  {:pre [poly
         args-map
         (not more)]}
  {:clojure.spec/op `t/inst
   :poly poly
   :args-map args-map})

(defmethod s/create-spec `t/inst
  [{:keys [poly args-map]}]
  (inst* (s/resolve-spec poly) args-map))

(defmethod s/expand-spec `t/tv
  [[_ tv & {:keys [wrap] :as opt}]]
  {:pre [(simple-keyword? tv)
         (empty? (dissoc opt :wrap))]}
  {:clojure.spec/op `t/tv
   :tv tv
   :wrap wrap})

(defmethod s/create-spec `t/tv
  [{:keys [tv wrap]}]
  (throw (ex-info (str "Cannot create spec for " tv)
                  {:tv tv
                   :wrap wrap})))

(defmethod s/expand-spec `t/fcase
  [[_ & args :as form]]
  (let [vargs (vec args)
        [default args] (if (odd? (count args))
                         ((juxt peek pop) vargs)
                         [nil args])
        ; flatten cases
        cases (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (throw (ex-info (str "Duplicate fcase entry for arity " k)
                                          {:form form}))
                          (assoc m k v)))
                      {}
                      (mapcat (fn [[k v]]
                                (if (set? k)
                                  (map #(vector % v) k)
                                  [[k v]]))
                              (partition 2 vargs)))]
    {:clojure.spec/op `t/fcase
     :cases cases
     :default default}))

(defmethod s/create-spec `t/fcase
  [{:keys [cases default]}]
  (fcase-impl cases default nil))

(defmethod s/expand-spec `t/fold-binders
  [[_ template tv & opt]]
  {:pre [(simple-keyword? tv)]}
  (let [{:keys [wrap]
         :or {wrap `(fn [~'xs]
                      (coerce-dotted-cat ~tv ~'xs))}}
        opt]
    {:clojure.spec/op `t/fold-binders
     :template template
     :tv tv
     :wrap wrap}))

(defmethod s/create-spec `t/fold-binders
  [{:keys [template tv wrap]}]
  (fold-binders-impl template tv wrap))

(defmethod s/expand-spec `t/spec-between
  [[_ & {:keys [upper lower]
         :or {upper `any?
              lower `(s/or)}}]]
  {:clojure.spec/op `t/spec-between
   :lower lower
   :upper upper})

(defmethod s/create-spec `t/spec-between
  [{:keys [lower upper]}]
  (spec-between-impl lower upper nil))

;; reduced-of

(defmethod s/expand-spec `t/reduced-of
  [[_ s & more]]
  {:pre [(not more)]}
  {:clojure.spec/op `t/reduced-of
   :s s})

(defn- reduced-of-impl [s gfn]
  (reify
    Spec
    (conform* [_ x settings-key settings]
      (if (reduced? x)
        (let [res (conform* (s/resolve-spec s)
                            @x
                            settings-key
                            settings)]
          (if (s/invalid? res)
            res
            (if (identical? res x)
              x
              (reduced res))))
        ::s/invalid))
    (unform* [_ x]
      (assert nil "TODO")
      )
    (explain* [_ path via in x settings-key settings]
      (assert nil "TODO")
      )
    (gen* [_ overrides path rmap]
      (if gfn
        (gfn)
        (gen/fmap reduced (-> s s/resolve-spec s/gen))))
    (with-gen* [_ gfn]
      (reduced-of-impl s gfn))
    (describe* [_]
      `(t/reduced-of ~s))))

(defmethod s/create-spec `t/reduced-of
  [{:keys [s]}]
  (reduced-of-impl s nil))


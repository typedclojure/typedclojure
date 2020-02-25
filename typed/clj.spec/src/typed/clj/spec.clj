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
(deftype Splice [vals])

(defn- mapcat-spliced [f coll]
  (mapcat (fn [el]
            (map
              f
              (if (instance? Splice el)
                (:vals el)
                [el])))
          coll))

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
     (instance? Splice form) (->Splice (mapcat-spliced inner (.vals ^Splice form)))
     (list? form) (outer (apply list (mapcat-spliced inner form)))
     (instance? clojure.lang.IMapEntry form)
     (outer (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
     (seq? form) (outer (doall (mapcat-spliced inner form)))
     (instance? clojure.lang.IRecord form)
       (outer (reduce (fn [r x] (conj r (inner x))) form form))
     (coll? form) (outer (into (empty form) (mapcat-spliced inner form)))
     :else (outer form))))

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
  "(tfn {:x {:sub (s/or)
             :super any?
             :variance :covariant}
         :y {:sub (s/or)
             :super any?
             :variance :covariant}}
        (map-of (tvar :x) (tvar :y)))
  "
  [binder body]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tfn ~binder ~body))))

(defmacro all
  "(all {:x {:sub (s/or)
             :super any?
             :variance :covariant}
         :y {:sub (s/or)
             :super any?
             :variance :covariant}}
        (map-of (tvar :x) (tvar :y)))
  "
  [binder body]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(all ~binder ~body))))

(defmacro tapp
  "(tapp tfn {:x int? :y boolean?})
  "
  [tfn args-map]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tapp ~tfn ~args-map))))

(defmacro inst
  "(inst poly {:x int? :y boolean?})
  "
  [poly args-map]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(inst ~poly ~args-map))))

(defmacro tvar
  "(tvar :a)
  "
  [tv]
  {:pre [(simple-keyword? tv)]}
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(tvar ~tv))))

(defmacro tvar-spec
  "(tvar :variance :covariant
         :lower (s/or)
         :upper any?)
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
               {:config (apply dissoc sbst (keys (second spec)))
                :form spec}

               (and (seq? spec)
                    (#{`tvar} (first spec)))
               (let [[_ tv & {dotted :...}] spec
                     sval (sbst tv)]
                 (assert (simple-keyword? tv))
                 (assert (if dotted (vector? sval) true))
                 (prn "tvar" spec sval)
                 (if dotted
                   {:config sbst
                    :form (->Splice
                            (map #(subst-tvar dotted (assoc sbst tv %)) sval))}
                   {:config sbst
                    :form (->Reduced
                            (or sval spec))}))

               :else
               {:config sbst
                :form spec}))
           sbst
           spec))

(comment
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

; all unresolved
(defn- tvar-spec-impl
  [position lower upper dotted gfn]
  (let []
    (reify
      Spec
      (conform* [_ x settings-key settings]
        (conform* (s/resolve-spec upper)
                  x
                  settings-key
                  settings))
      (unform* [_ x]
        (assert nil)
        )
      (explain* [_ path via in x settings-key settings]
        (assert nil)
        )
      (gen* [_ overrides path rmap]
        (prn "generating tvar-spec" gfn)
        (if gfn
          (gfn)
          (cond-> (gen/one-of
                    (map (fn [spc]
                           (assert nil "TODO")
                           #_
                           (->> spc
                                s/resolve-spec
                                s/gen
                                (gen/fmap identical-spec)))
                         [lower upper]))
            dotted gen/vector)))
      (with-gen* [_ gfn]
        (tvar-spec-impl position lower upper dotted gfn))
      (describe* [_]
        `(tvar-spec :position ~position
                    :lower ~lower
                    :upper ~upper)))))

(defn- all-impl
  [binder body gfn]
  {:pre [(vector? binder)
         (even? (count binder))]}
  (let []
    (reify
      IPoly
      (inst* [_ subst-map]
        (-> body
            (subst-tvar subst-map)
            s/resolve-spec))

      Spec
      (conform* [_ x settings-key settings]
        (let [smap (reduce (fn [smap [b v]]
                             (prn "all-impl v" v)
                             (let [sspec (subst-tvar v smap)
                                   _ (prn "sspec" sspec)
                                   res (s/resolve-spec sspec)]
                               (prn "res" res)
                               (assoc smap b (-> res
                                                 s/gen
                                                 gen/generate))))
                           {}
                           (partition 2 binder))
              _ (prn "smap" smap)
              spc (-> (subst-tvar body smap)
                      s/resolve-spec)]
          (prn "spc" (s/describe spc))
          (conform* spc
                    x
                    settings-key
                    settings)))
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

(defn- tvar-impl
  [tv dotted]
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
      `(tvar ~tv ~@(some->> dotted (vector :...))))))

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
  [[_ & {:keys [position lower upper dotted]
         :or {position #{:input :output}
              lower `(s/or)
              upper `any?
              dotted false}
         :as opt}]]
  {:pre [(set? position)
         lower
         upper
         (empty? (apply dissoc opt [:position :lower :upper :dotted]))]}
  {:clojure.spec/op `tvar-spec
   :position position
   :lower lower
   :upper upper
   :dotted dotted})

(defmethod s/create-spec `tvar-spec
  [{:keys [position lower upper dotted]}]
  (tvar-spec-impl position lower upper dotted nil))

(defmethod s/expand-spec `all
  [[_ binder body & more]]
  {:pre [binder
         body
         (not more)]}
  {:clojure.spec/op `all
   :binder binder
   :body body})

(defmethod s/create-spec `all
  [{:keys [binder body]}]
  (all-impl binder body nil))

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
  [[_ tv & {dotted :...}]]
  {:pre [(simple-keyword? tv)]}
  {:clojure.spec/op `tvar
   :tv tv
   :dotted dotted})

(defmethod s/create-spec `tvar
  [{:keys [tv dotted]}]
  (tvar-impl tv dotted))

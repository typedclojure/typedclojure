;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.malli.schema-to-type
  "Parsing malli schemas to Typed Clojure syntax"
  (:require [typed.clojure :as t]
            [malli.core :as m]))

(declare ^:private malli-syntax->type*)

(defn- malli->arg-lists
  [m {::keys [source] :as opts}]
  (let [assert-no-rest-arg (fn [argv]
                             (assert (not-any? #{:* :... '& '<* '<...} argv)
                                     (str "Malli regex is too complex to convert to type"
                                          (when source
                                            (str " (sourced from malli schema for " source ", use t/ann to annotate instead)")))))]
    (letfn [(gen-inner [m acc]
              ;; TODO flatten all regex ops
              (case (m/type m)
                :* (let [inner-t (malli-syntax->type* (m/-get m 0 nil) (assoc opts ::mode :validator-type))]
                     (into []
                           (map (fn [argv]
                                  (assert-no-rest-arg argv)
                                  (conj argv inner-t :*)))
                           acc))
                (:cat :catn) (reduce (fn [acc argm]
                                       (gen-inner argm acc))
                                     acc
                                     (cond->> (m/children m)
                                       (= :catn (m/type m)) (map #(nth % 2))))
                :? (let [suffixes (into [[]]
                                        (gen-inner (m/-get m 0 nil) [[]]))]
                     (into []
                           (mapcat (fn [argv]
                                     (assert-no-rest-arg argv)
                                     (map #(into argv %) suffixes)))
                           acc))
                (let [t (malli-syntax->type* m (assoc opts ::mode :validator-type))]
                  (mapv (fn [argv]
                          (assert-no-rest-arg argv)
                          (conj argv t))
                        acc))))]
      (gen-inner m [[]]))))

(defmulti -malli->type
  "Internal. Use register-malli->type-extension."
  (fn [m opts] (m/type m))
  :default ::default)

#?(:clj
   (defmacro register-malli->type-extension
     "argv should take 2 arguments: the malli schema, and options.
     Should also case on :typed.malli.schema-to-type/mode in body,
     see examples in this namespace."
     [dval argv & body]
     `(defmethod -malli->type ~dval ~argv ~@body)))

(defmethod -malli->type ::default [m opts]
  (println (str "WARNING: No conversion from malli to Typed Clojure for "
                (pr-str (m/type m)) " in " `-malli->type " (" #?(:clj "Clojure" :cljs "ClojureScript") " implementation)"))
  nil)

(comment
  (malli->type :int {::mode :validator-type})
  )

(defn malli->type
  [m {::keys [mode] :as opts}]
  {:post [%]}
  (assert (#{:validator-type :parser-type} mode)
          mode)
  (letfn [(gen-inner [m {::keys [mode] :as opts}]
            {:pre [(#{:validator-type :parser-type} mode)]}
            (or (get-in opts [::schema-form->free (m/form m)])
                (let [_ (assert (<= (get (::seen opts) m 0)
                                    2)
                                (str "Infinitely expanding schema: " m))
                      opts (update-in opts [::seen m] (fnil inc 0))
                      gen-inner (fn
                                  ([m] (gen-inner m opts))
                                  ([m opts] (gen-inner m opts)))]
                  (case (m/type m)
                    (:any any?) `t/Any
                    some? #?(:clj `Object :cljs `t/Any)
                    (:> :< :>= :<= number?) `t/Num
                    integer? `t/AnyInteger
                    ;;FIXME unify cljs/clj
                    (int? pos-int? neg-int?) `t/Num
                    (:nil nil?) `(t/Val nil)
                    (:boolean boolean?) `t/Bool
                    (:string string?) `t/Str
                    :map-of (let [[kt vt :as cts] (map gen-inner (m/children m))
                                  _ (assert (= 2 (count cts)) (m/children m))]
                              `(t/Map ~kt ~vt))
                    :map (let [{req true opt false} (->> (m/children m) (group-by (m/-comp not :optional second)))
                               opt (not-empty (apply array-map (mapcat (fn [[k _ s]] [k (gen-inner s)]) opt)))
                               req (not-empty (apply array-map (mapcat (fn [[k _ s]] [k (gen-inner s)]) req)))]
                           (cond
                             (and (not opt) req) (list 'quote req)
                             :else `(t/HMap ~@(when req [:mandatory req])
                                            ~@(when opt [:optional opt]))))
                    (:or :and) (let [_ (assert (= mode :validator-type) "FIXME is :parser-type the same as :validator-type for :or/:and?")
                                     inners (mapv gen-inner (m/children m))]
                                 (if (= 1 (count inners))
                                   (first inners)
                                   (list* (case (m/type m) :or `t/U :and `t/I)
                                          inners)))
                    :function (let [ts (map gen-inner (m/children m))
                                    arities (mapcat (fn [t]
                                                      (if (vector? t)
                                                        [t]
                                                        (do (assert (= `t/IFn (first t)))
                                                            (rest t))))
                                                    ts)]
                                (if (= 1 (count arities))
                                  (first arities)
                                  `(t/IFn ~@arities)))
                    :vector `(t/Vec ~(gen-inner (first (m/children m))))
                    :set `(t/Set ~(gen-inner (first (m/children m))))
                    :sequential `(t/SequentialColl ~(gen-inner (first (m/children m))))
                    ;;TODO :tuple
                    ;;TODO :enum
                    ;;TODO :multi
                    :fn `t/Any
                    :maybe `(t/Maybe ~(gen-inner (first (m/children m))))
                    :re `t/Str
                    ;; should we have a :instrument-type mode? m/validate + m/=> is probably not sound.
                    :=> (let [[param-malli ret-malli :as cs] (m/children m)
                              _ (assert (= 2 (count cs)))
                              param-ts (malli->arg-lists param-malli opts)
                              rett (gen-inner ret-malli)
                              arities (map #(conj % :-> rett) param-ts)]
                          (if (= 1 (count arities))
                            (first arities)
                            `(t/IFn ~@arities)))
                    ;; FIXME regex conversions are wrong
                    :? `(t/Nilable ~(gen-inner (m/-get m 0 nil)))
                    :* (let [inner-t (gen-inner (m/-get m 0 nil))]
                         (case mode
                           :validator-type `(t/Seqable ~inner-t)
                           :parser-type `(t/Vec ~inner-t)))
                    :ref (let [n (m/-ref m)
                               _ (assert ((some-fn keyword? symbol? string?) n))
                               _ (assert (re-matches #"^[a-zA-Z]+[a-zA-Z0-9]*$" (name n)) n)
                               gn (gensym (name n))]
                           `(t/Rec [~gn] ~(gen-inner (m/-deref m) (assoc-in opts [::schema-form->free (m/form m)] gn))))
                    :int `t/AnyInteger
                    (keyword? :keyword simple-keyword? #_:simple-keyword qualified-keyword? :qualified-keyword) `t/Kw
                    (symbol? :symbol simple-symbol? #_:simple-symbol qualified-symbol? :qualified-symbol) `t/Sym
                    :catn `'~(into (case mode :validator-type [] :parser-type {})
                                   (map (fn [[cat-k _props cat-schema :as c]]
                                          (assert (= 3 (count c)) c)
                                          (assert ((some-fn string? keyword? symbol? number?) cat-k) c)
                                          (assert (m/schema? cat-schema) c)
                                          (let [inner-t (gen-inner cat-schema)]
                                            (case mode
                                              :validator-type inner-t
                                              :parser-type [cat-k inner-t]))))
                                   (m/children m))
                    :orn `(t/U ~@(mapv (fn [[case-k _props case-schema :as c]]
                                         (assert (= 3 (count c)) c)
                                         (assert ((some-fn string? keyword? symbol? number?) case-k) c)
                                         (assert (m/schema? case-schema) c)
                                         (let [inner-t (gen-inner case-schema)]
                                           (case mode
                                             :validator-type inner-t
                                             :parser-type `'[(t/Val ~case-k) ~inner-t])))
                                       (m/children m)))
                    (:schema ::m/schema) (gen-inner (m/deref m))
                    :not= `(t/Not (t/Val ~(first (m/children m))))
                    := `(t/Val ~(first (m/children m)))
                    :uuid `t/UUID
                    (:merge :union :select-keys) (throw (ex-info (str "FIXME want to expand :merge/:union/:select keys in " `malli->type)
                                                                 {}))
                    (or (-malli->type m opts)
                        `t/Any)))))]
    (let [inner-t (gen-inner m opts)]
      (case mode
        :validator-type inner-t
        :parser-type `(t/U (t/Val ::m/invalid)
                           ~inner-t)))))

#?(:clj
   (defn- malli-syntax->type*
     [m opts]
     (malli->type (-> m eval m/schema)
                  opts)))

#?(:clj
   (defn malli-syntax->parser-type
     "The types of values returned by malli.core/parse as Typed Clojure syntax.
     Takes unevalated malli syntax.  Uses *ns* for resolution."
     [m]
     (malli-syntax->type* m {::mode :parser-type})))

#?(:clj
   (defn malli-syntax->validator-type
     "The types of values that succeed for malli.core/validate as Typed Clojure syntax.
     Takes unevalated malli syntax. Uses *ns* for resolution."
     [m]
     (malli-syntax->type* m {::mode :validator-type})))

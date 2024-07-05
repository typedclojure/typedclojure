(ns typed.spec1.spec-to-type
  (:require [clojure.spec.alpha :as spec1]
            [typed.clojure :as t]))

(declare spec-syntax->type)

(defmulti -spec-syntax->type
  "Internal. Use register-spec-syntax->type-extension."
  (fn [s opts]
    (if (and (seq? s)
             (qualified-symbol? (first s)))
      (first s)
      (when (qualified-ident? s)
        s)))
  :default ::default)

(defmethod -spec-syntax->type ::default [s opts] nil)

(defmacro register-spec-syntax->type-extension
  "argv should take 2 arguments: the spec form, and options."
  [dval argv & body]
  `(defmethod -spec-syntax->type ~dval ~argv ~@body))

(defn- spec-syntax->arg-lists
  [s {::keys [source] :as opts}]
  (let [assert-no-rest-arg (fn [argv]
                             (assert (not-any? #{:* :.. :... '& '<* '<...} argv)
                                     (str "Spec1 regex is too complex to convert to type"
                                          (when source
                                            (str " (sourced from spec for " source ", use t/ann to annotate instead)")))))]
    (letfn [(gen-inner [s acc]
              ;; TODO flatten all regex ops
              (or (cond
                    (seq? s) (condp = (first s)
                               `spec1/* (let [inner-t (spec-syntax->type (second s) opts)]
                                          (into []
                                                (map (fn [argv]
                                                       (assert-no-rest-arg argv)
                                                       (conj argv inner-t :*)))
                                                acc))
                               `spec1/? (let [suffixes (into [[]]
                                                             (gen-inner (second s)  [[]]))]
                                          (into []
                                                (mapcat (fn [argv]
                                                          (assert-no-rest-arg argv)
                                                          (map #(into argv %) suffixes)))
                                                acc))
                               `spec1/cat (reduce (fn [acc s]
                                                    (gen-inner s acc))
                                                  acc
                                                  (map second (partition 2 (rest s))))
                               nil))
                  (let [t (spec-syntax->type s opts)]
                    (mapv (fn [argv]
                            (assert-no-rest-arg argv)
                            (conj argv t))
                          acc))))]
      (gen-inner s [[]]))))

(defn spec-syntax->type
  [s opts]
  (letfn [(gen-inner [s opts]
            (or (get-in opts [::spec-form->free s])
                (let [_ (assert (<= (get (::seen opts) s 0)
                                    2)
                                (str "Infinitely expanding spec: " s))
                      opts (update-in opts [::seen s] (fnil inc 0))
                      gen-inner (fn
                                  ([s] (gen-inner s opts))
                                  ([s opts] (gen-inner s opts)))]
                  (cond
                    (keyword? s) (or (-spec-syntax->type s opts)
                                     (let [gn (gensym (name s))
                                           spec-obj (spec1/get-spec s)
                                           _ (assert (some? spec-obj) (str "Could not resolve spec " s))]
                                       `(t/Rec [~gn] ~(gen-inner (spec1/form spec-obj) (assoc-in opts [::spec-form->free s] gn)))))
                    (symbol? s) (condp = s
                                  ;; FIXME t/AnyInteger is really `integer?`
                                  `int? `t/AnyInteger
                                  `nil? `(t/Val nil)
                                  nil)
                    (seq? s) (condp = (first s)
                               `spec1/fspec (let [{:keys [args ret]} (rest s)
                                                  param-ts (if args
                                                             (spec-syntax->arg-lists args opts)
                                                             [[]])
                                                  rett (if ret (gen-inner ret opts) `t/Any)
                                                  arities (map #(conj % :-> rett) param-ts)]
                                              (if (= 1 (count arities))
                                                (first arities)
                                                `(t/IFn ~@arities)))
                               nil)))
                (-spec-syntax->type s opts)
                (throw (ex-info (str "Unsupported " `spec->type " " s) {}))))]
    (gen-inner s opts)))

(defn spec->type [spec-obj opt]
  (spec-syntax->type (spec1/form spec-obj) opt))

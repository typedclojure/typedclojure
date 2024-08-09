;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;flat contracts only
;; important! any runtime dependencies declared here must be explicitly required by clojure.core.typed
;; for AOT compatibility. This is why ast->pred uses requiring-resolve--so cct can load faster.
(ns ^:no-doc clojure.core.typed.type-contract
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  (:require [clojure.core.typed.ast-ops :as ops]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

(defn keyword-singleton? [{:keys [op val]}]
  (when (= :singleton op)
    (keyword? val)))

(def ^:private int-error #((requiring-resolve 'clojure.core.typed.errors/int-error) %1 %2))

(defn- AND
  "Like `clojure.core/and` but produces better bytecode when used with
  compile-time known booleans. Must take boolean test."
  ([] true)
  ([x] x)
  ([x & next]
   (cond
     (true? x) (apply AND next)
     (false? x) false
     :else `(if ~x ~(apply AND next) false))))

(defn- OR
  "Like `clojure.core/or` but produces better bytecode when used with
  compile-time known booleans. Must take boolean test."
  ([] false)
  ([x] x)
  ([x & next]
   (cond
     (true? x) true
     (false? x) (apply OR next)
     :else `(if ~x true ~(apply OR next)))))

(defn ast->pred
  "Returns syntax representing a runtime predicate on the
  given type ast."
  ([t opt opts]
   (letfn [(gen-inner
             ([{:keys [op] :as t} arg opts]
              (case op
                (:F) (int-error "Cannot generate predicate for free variable" opts)
                (:Poly) (if-some [Poly-predicate (::Poly-pred-syntax opt)]
                          (Poly-predicate t arg opt)
                          (int-error "Cannot generate predicate for polymorphic type" opts))
                (:PolyDots) (int-error (str "Cannot generate predicate for dotted polymorphic type: " (:form t)) opts)
                (:Fn) (if-some [Fn-predicate (::Fn-pred-syntax opt)]
                        (Fn-predicate t arg opt)
                        (int-error "Cannot generate predicate for function type" opts))
                (:TApp) (let [{:keys [rator rands]} t]
                          (cond 
                            ;needs resolving
                            (#{:Name} (:op rator))
                            (gen-inner (update t :rator ops/resolve-Name opts) arg opts)
                            ;polymorphic class
                            (#{:Class} (:op rator))
                            (let [{:keys [args pred] :as rcls} (get ((requiring-resolve 'clojure.core.typed.current-impl/rclass-env)
                                                                     ((requiring-resolve 'typed.cljc.runtime.env/checker) opts))
                                                                    (:name rator))
                                  _ (when-not rcls
                                      (int-error (str "Class does not take arguments: "
                                                      (:name rator))
                                                 opts))
                                  _ (when-not (args (count rands))
                                      (int-error (str "Wrong number of arguments to "
                                                      (:name rator) ", expected " args
                                                      " actual " (count rands))
                                                 opts))
                                  rands-args (repeatedly (count rands) gensym)
                                  rands-p (mapv (fn [ast gsym]
                                                  `(fn [~gsym] ~(gen-inner ast gsym opts))) 
                                                rands rands-args)]
                              (AND `(instance? ~(:name rator) ~arg)
                                   (apply pred arg rands-p)))
                            ;substitute
                            (#{:TFn} (:op rator))
                            (gen-inner (ops/instantiate-TFn rator rands) arg opts)
                            :else
                            (int-error (str "Don't know how to apply type: " (:form t)) opts)))
                (:Class) `(instance? ~(:name t) ~arg)
                (:Name) 
                (case ((requiring-resolve 'clojure.core.typed.current-impl/current-impl) opts)
                  :clojure.core.typed.current-impl/clojure (gen-inner (ops/resolve-Name t opts) arg opts)
                  (int-error (str "TODO CLJS Name") opts))
                ;              (cond
                ;                              (empty? (:poly? t)) `(instance? ~(:the-class t) ~arg)
                ;                              :else (int-error (str "Cannot generate predicate for polymorphic Class") opts))
                (:Any) true
                ;TODO special case for union of HMap, and unions of constants
                (:U) (apply OR (map #(gen-inner % arg opts) (:types t)))
                (:I) (apply AND (map #(gen-inner % arg opts) (:types t)))
                (:HVec) (apply AND
                               `(vector? ~arg)
                               (cond
                                 (:rest t)
                                 `(<= ~(count (:types t)) (count ~arg))
                                 (:drest t)
                                 (int-error (str "Cannot generate predicate for dotted HVec") opts)
                                 :else
                                 `(= ~(count (:types t)) (count ~arg)))
                               (concat
                                 (map-indexed 
                                   (fn [i t*]
                                     (let [vlocal (gensym "vlocal")]
                                       `(let [~vlocal (nth ~arg ~i)]
                                          ~(gen-inner t* vlocal opts))))
                                   (:types t))
                                 (when (:rest t)
                                   (let [nfixed (count (:types t))]
                                     [`(let [rstvec# (subvec ~arg ~nfixed)]
                                         (every? ~(let [vlocal (gensym "vlocal")]
                                                    `(fn [~vlocal] 
                                                       ~(gen-inner (:rest t) vlocal opts)))
                                                 rstvec#))]))))
                (:CountRange) (let [cnt (gensym "cnt")
                                    {:keys [lower upper]} t]
                                (AND ;; immutable collections only
                                     (OR `(nil? ~arg)
                                         `(coll? ~arg)
                                         `(string? ~arg))
                                     (let [cnt `(bounded-count ~(inc (if (and lower upper)
                                                                      (max lower upper)
                                                                      (or lower upper)))
                                                               ~arg)]
                                       `(<= ~@(cond-> [lower cnt]
                                                upper (conj upper))))))
                (:singleton) (let [v (:val t)]
                               (cond
                                 (nil? v) `(nil? ~arg)
                                 ((some-fn symbol? string?) v) `(= '~v ~arg)
                                 (keyword? v) `(identical? '~v ~arg)
                                 ((some-fn true? false?) v) `(identical? '~v ~arg)
                                 (number? v) `(if (number? ~arg)
                                                ; I think = models the type system's behaviour better than ==
                                                (= '~v ~arg)
                                                false)
                                 :else (int-error 
                                         (str "Cannot generate predicate for value type: " (pr-str v))
                                         opts)))
                (:HMap) (let [mandatory (apply hash-map (:mandatory t))
                              optional (apply hash-map (:optional t))
                              absent-keys (:absent-keys t)
                              valgen (fn [tmap]
                                       (zipmap (map :val (keys tmap))
                                               (mapv (fn [tsyn gi]
                                                       `(fn [~gi]
                                                          ~(gen-inner tsyn gi opts)))
                                                     (vals tmap)
                                                     (repeatedly (count tmap) gensym))))]
                          `(((requiring-resolve 'clojure.core.typed.current-impl/hmap-c?)
                             :mandatory ~(valgen mandatory)
                             :optional ~(valgen optional)
                             :absent-keys ~(set (map :val absent-keys))
                             :complete? ~(:complete? t))
                            ~arg))
                (:Rec) (cond
                         ;we're already inside this rec
                         (contains? (::inside-rec opts) (:unwrap-id t))
                         (let [{:keys [unwrap-id]} t]
                           `(~unwrap-id ~arg))

                         :else
                         (let [unwrap-id (gensym 'Rec-id)
                               body (ops/unwrap-rec t unwrap-id)
                               garg (gensym 'garg)]
                           `((fn ~unwrap-id
                               [~garg]
                               ~(gen-inner body garg (update opts ::inside-rec (fnil conj #{}) unwrap-id)))
                             ~arg)))
                (int-error (str op " not supported in type->pred: " (:form t)) opts))))]
     (let [arg (gensym "arg")]
       `(fn [~arg] 
          ~(gen-inner t arg opts))))))

(defn ast->contract 
  "Returns syntax representing a runtime predicate on the
  given type ast."
  [t opts]
  (letfn [(gen-inner
            ([{:keys [op] :as t} arg opts]
             (case op
               (:F) (int-error "Cannot generate predicate for free variable" opts)
               (:Poly) (int-error "Cannot generate predicate for polymorphic type" opts)
               (:PolyDots) (int-error "Cannot generate predicate for dotted polymorphic type" opts)
               (:Fn) (cond
                       (= 1 (count (:arities t)))
                       (let [{:keys [dom rng filter object rest drest] :as method}
                             (first (:arities t))]
                         (if (or rest drest filter object)
                           (int-error "Cannot generate predicate for this function type" opts)
                           `((requiring-resolve 'clojure.core.typed.contract/ifn-c)
                             ~(mapv #(gen-inner % arg opts) dom)
                             ~(gen-inner rng arg opts))))
                       :else (int-error "Cannot generate predicate for function type" opts))
               (:TApp) (let [{:keys [rator rands]} t]
                         (cond 
                           ;needs resolving
                           (#{:Name} (:op rator))
                           (gen-inner (update t :rator ops/resolve-Name opts) arg opts)
                           ;polymorphic class
                           ;(#{:Class} (:op rator))
                           ;  (let [{:keys [args pred] :as rcls} (get (impl/rclass-env ((requiring-resolve 'typed.cljc.runtime.env/checker) opts)) (:name rator))
                           ;        _ (when-not rcls
                           ;            (int-error (str "Class does not take arguments: "
                           ;                                (:name rator)) opts))
                           ;        _ (when-not (args (count rands))
                           ;            (int-error (str "Wrong number of arguments to "
                           ;                                (:name rator) ", expected " args
                           ;                                " actual " (count rands)) opts))
                           ;        rands-args (repeatedly (count rands) gensym)
                           ;        rands-p (mapv (fn [ast gsym]
                           ;                        `(fn [~gsym] ~(gen-inner ast gsym opts))) 
                           ;                      rands rands-args)]
                           ;    `(and (instance? ~(:name rator) ~arg)
                           ;          ~(apply pred arg rands-p)))
                           ;substitute
                           (#{:TFn} (:op rator))
                           (gen-inner (ops/instantiate-TFn rator rands) arg opts)
                           :else
                           (int-error (str "Don't know how to apply type: " (:form t)) opts)))
               (:Class) `((requiring-resolve 'clojure.core.typed.contract/instance-c*)
                          ~(:name t)
                          #(instance? ~(:name t) %))
               (:Name) 
               (case ((requiring-resolve 'clojure.core.typed.current-impl/current-impl) opts)
                 :clojure.core.typed.current-impl/clojure (gen-inner (ops/resolve-Name t opts) arg opts)
                 (int-error (str "TODO CLJS Name") opts))
               ;              (cond
               ;                              (empty? (:poly? t)) `(instance? ~(:the-class t) ~arg)
               ;                              :else (int-error (str "Cannot generate predicate for polymorphic Class") opts))
               (:Any) `@(requiring-resolve 'clojure.core.typed.contract/any-c)
               ;TODO special case for union of HMap, and unions of constants
               (:U) `((requiring-resolve 'clojure.core.typed.contract/or-c)
                      ;; TODO flatten unions, ensuring Names are resolved
                      ~@(mapv #(gen-inner % arg opts) (:types t)))
               (:I) `((requiring-resolve 'clojure.core.typed.contract/and-c)
                      ~@(mapv #(gen-inner % arg opts) (:types t)))
               ;(:HVec) `(and (vector? ~arg)
               ;              ~(cond
               ;                 (:rest t)
               ;                 `(<= ~(count (:types t)) (count ~arg))
               ;                 (:drest t)
               ;                 (int-error (str "Cannot generate predicate for dotted HVec") opts)
               ;                 :else
               ;                 `(== ~(count (:types t)) (count ~arg)))
               ;              ~@(doall
               ;                  (map-indexed 
               ;                    (fn [i t*]
               ;                      (let [vlocal (gensym "vlocal")]
               ;                        `(let [~vlocal (nth ~arg ~i)]
               ;                           ~(gen-inner t* vlocal opts))))
               ;                    (:types t)))
               ;              ~@(when (:rest t)
               ;                  (let [nfixed (count (:types t))]
               ;                    [`(let [rstvec# (subvec ~arg ~nfixed)]
               ;                        (every? ~(let [vlocal (gensym "vlocal")]
               ;                                   `(fn [~vlocal] 
               ;                                      ~(gen-inner (:rest t) vlocal opts)))
               ;                                rstvec#))])))
               (:CountRange) `((requiring-resolve 'clojure.core.typed.contract/count-range-c)
                               ~(:lower t) ~(:upper t))
               (:singleton) (let [v (:val t)]
                              (cond
                                (nil? v) `@(requiring-resolve 'clojure.core.typed.contract/nil-c)
                                (symbol? v) `((requiring-resolve 'clojure.core.typed.contract/equiv-c) ~v)
                                (keyword? v) `((requiring-resolve 'clojure.core.typed.contract/identical-c) ~v)
                                ((some-fn true? false?) v) `((requiring-resolve 'clojure.core.typed.contract/identical-c) ~v)
                                (number? v) ; I think = models the type system's behaviour better than ==
                                `((requiring-resolve 'clojure.core.typed.contract/equiv-c) ~v)

                                :else (int-error 
                                        (str "Cannot generate predicate for value type: " v)
                                        opts)))

               (:HMap) (let [mandatory (apply hash-map (:mandatory t))
                             optional (apply hash-map (:optional t))
                             absent-keys (:absent-keys t)
                             congen (fn [tmap]
                                      (zipmap (map :val (keys tmap))
                                              (map #(gen-inner % arg opts) (vals tmap))))]
                         `((requiring-resolve 'clojure.core.typed.contract/hmap-c)
                           :mandatory ~(congen mandatory)
                           :optional ~(congen optional)
                           :absent-keys ~(set (map :val absent-keys))
                           :complete? ~(:complete? t)))

               ;(:Rec) (cond
               ;         ;we're already inside this rec
               ;         (contains? (::inside-rec opts) (:unwrap-id t))
               ;           (let [{:keys [unwrap-id]} t]
               ;             `(~unwrap-id ~arg))
               ;         
               ;         :else
               ;          (let [unwrap-id (gensym 'Rec-id)
               ;                body (ops/unwrap-rec t unwrap-id)
               ;                garg (gensym 'garg)]
               ;            `((fn ~unwrap-id
               ;                [~garg]
               ;                ~(gen-inner body garg (update opts ::inside-rec (fnil conj #{}) unwrap-id)))
               ;              ~arg)))
               (int-error (str op " not supported in type->pred: " (:form t)) opts))))]
    (gen-inner t nil opts)))

(defn type-syntax->pred
  ([t opt opts]
   (-> ((requiring-resolve 'clojure.core.typed.parse-ast/parse) t opts)
       (ast->pred opt opts))))

(defn type-syntax->contract [t opts]
  (-> ((requiring-resolve 'clojure.core.typed.parse-ast/parse) t opts)
      (ast->contract opts)))

(comment
        (type-syntax->pred 'Any)
        (type-syntax->pred 'Nothing)
        (type-syntax->pred '(U Number Boolean))

        ((requiring-resolve 'clojure.core.typed.contract/contract) (type-syntax->contract 'nil) 1)

  (clojure.pprint/pprint (type-syntax->pred '(HMap :optional {:c Number})))
  (clojure.pprint/pprint (type-syntax->pred '(HMap :mandatory {:c Number})))
  (clojure.pprint/pprint (type-syntax->pred ''[Number]))
  (clojure.pprint/pprint (type-syntax->pred '(Rec [x] (U '[x] Number))))
  (clojure.pprint/pprint (type-syntax->pred '(clojure.core.typed/Option Number)))
  
  (walk (type-syntax->pred '(HMap :optional {:c Number}))
        (fn [e] (prn 'pre (:op e)))
        (fn [e] (prn 'post (:op e))))

  (def ast (clojure.core.typed.parse-ast/parse-clj '(HMap :optional {:c Number})))

  (:children ast)

  (ops/walk ast
            (fn f [e] (prn 'pre (:op e)))
            (fn [e] (prn 'post (:op e))))
  (ops/unwrap-rec (clojure.core.typed.parse-ast/parse-clj '(Rec [x] (U '[x] Number))) 'abc)
  )

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; TODO unparse functionality
(ns ^:no-doc clojure.core.typed.parse-ast
  #?(:clj (:refer-clojure :exclude [requiring-resolve delay]))
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.set :as set]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])
            #?(:clj [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])))

(t/ann *parse-type-in-ns* (t/U nil t/Sym))
(defonce ^:dynamic *parse-type-in-ns* nil)

;(t/ann ^:no-check clojure.core.typed.current-impl/*current-impl* (t/U nil t/Kw))
(t/ann ^:no-check clojure.core.typed.current-impl/current-impl [t/Any -> t/Kw])

(t/ann parse-in-ns [t/Any -> t/Sym])
(defn- parse-in-ns [opts]
  {:post [(symbol? %)]}
  (or *parse-type-in-ns* 
      (impl/impl-case opts
        :clojure (ns-name *ns*)
        :cljs (t/tc-ignore ((requiring-resolve 'typed.cljs.checker.util/cljs-ns))))))

(t/ann ^:no-check clojure.core.typed.current-impl/assert-clojure
       [t/AnySeqable :? :-> nil])

(t/ann ^:no-check clojure.core.typed.errors/int-error 
       [t/Any (t/HMap :optional {:use-current-env t/Any}) :? :-> t/Nothing])

(t/ann resolve-type-clj [t/Sym t/Any -> (t/U t/AnyVar Class nil)])
(defn- resolve-type-clj
  "Returns a var, class or nil"
  [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn var? class? nil?) %)]}
  (impl/assert-clojure opts)
  (let [nsym (parse-in-ns opts)]
    (if-let [ns (find-ns nsym)]
      (ns-resolve ns sym)
      (err/int-error (str "Cannot find namespace: " sym) opts))))

(def ^:private ns-rewrites-clj {'clojure.core.typed 'typed.clojure})

(defn- resolve-type-alias-clj
  "Returns a symbol if sym maps to a type alias, otherwise nil"
  [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol? nil?) %)]}
  (impl/assert-clojure opts)
  (let [checker (impl/clj-checker)
        nsym (parse-in-ns opts)
        nsp (some-> (namespace sym) symbol)]
    (if-let [ns (find-ns nsym)]
      (when-let [qual (if nsp
                        (some-> (or ((ns-aliases ns) nsp)
                                    (find-ns nsp))
                                ns-name)
                        (ns-name ns))]
        (let [qual (ns-rewrites-clj qual qual)
              _ (assert (and (symbol? qual)
                             (not (namespace qual))))
              qsym (symbol (name qual) (name sym))]
          (when (contains? (impl/alias-env checker) qsym)
            qsym)))
      (err/int-error (str "Cannot find namespace: " sym) opts))))

(defn- resolve-type-clj->sym
  [sym opts]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (impl/assert-clojure opts)
  (let [opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
        nsym (parse-in-ns opts)]
    (if-some [ns (find-ns nsym)]
      (or (when (special-symbol? sym)
            sym)
          ('#{quote Array ReadOnlyArray Array2 List*} sym)
          (when-some [res (ns-resolve ns sym)]
            (or (when (var? res)
                  (if-some [rewrite-nsym (ns-rewrites-clj (some-> res symbol namespace symbol))]
                    (symbol (name rewrite-nsym) (-> res symbol name))
                    (coerce/var->symbol res)))
                (when (class? res)
                  (coerce/Class->symbol res))))
          (when-some [alias-sym (some-> ((ns-aliases ns)
                                         (some-> (namespace sym)
                                                 symbol))
                                        ns-name)]
            (symbol (name (ns-rewrites-clj alias-sym alias-sym))
                    (name sym)))
          (let [sym-nsym (or (some-> sym namespace symbol)
                             nsym)]
            (symbol (name (ns-rewrites-clj sym-nsym sym-nsym)) (name sym))))
      (err/int-error (str "Cannot find namespace: " sym) opts))))

(declare parse parse-path-elem)

(t/ann ^:no-check parse [t/Any t/Any -> Type])

(t/defalias Type
  (t/Rec [Type]
    (t/U '{:op ':singleton
           :val t/Any
           :form t/Any}
         '{:op ':Fn
           :arities (t/Vec Function)
           :form t/Any
           :children (t/Vec t/Kw)})))

;Map from scoped vars to unique names
(t/ann *tvar-scope* (t/Map t/Sym t/Sym))
(def ^:dynamic *tvar-scope* {})

(t/ann *dotted-scope* (t/Map t/Sym t/Sym))
(def ^:dynamic *dotted-scope* {})

#?(:cljs :ignore :default
(defmacro with-frees [fs & args]
  `(binding [*tvar-scope* (merge *tvar-scope* ~fs)]
     ~@args)))

#?(:cljs :ignore :default
(defmacro with-dfrees [fs & args]
  `(binding [*dotted-scope* (merge *dotted-scope* ~fs)]
     ~@args)))

(t/defalias Filter
  (t/Rec [Filter]
  (t/U '{:op ':top-filter}
       '{:op ':bot-filter}
       '{:op ':or-filter
         :fs (t/Vec Filter)}
       '{:op ':and-filter
         :fs (t/Vec Filter)}
       '{:op ':impl-filter
         :a Filter
         :c Filter}
       (HMap :mandatory {:op ':type-filter
                         :type Type
                         :id NameRef}
             :optional {:path (t/Vec PathElem)})
       (HMap :mandatory {:op ':not-type-filter
                         :type Type
                         :id NameRef}
             :optional {:path (t/Vec PathElem)}))))

#?(:cljr

(do 
  (def ^:private init-symbol-escape *allow-symbol-escape*)
  (.bindRoot #'*allow-symbol-escape* false))

)

(t/ann parse-filter [t/Any t/Any -> Filter])
(defn parse-filter [syn opts]
  (case syn
    tt {:op :top-filter}
    ff {:op :bot-filter}
    (let [m (when ((every-pred seq? sequential?) syn)
              (let [[f & args] syn]
                (case f
                  is
                  (let [[tsyn nme psyns] args
                        _ (when (and (= 3 (count args))
                                     (not (vector? psyns)))
                            (err/int-error
                              (str "3rd argument to 'is' must be a vector")
                              opts))
                        _ (when-not (<= 2 (count args) 3)
                            (throw (ex-info "Bad arguments to 'is'"
                                            {:form syn})))
                        t (parse tsyn opts)
                        p (when (= 3 (count args))
                            (mapv #(parse-path-elem % opts) psyns))]
                    (cond-> 
                      {:op :type-filter
                       :type t
                       :id nme}
                      p (assoc :path p)))
                  !
                  (let [[tsyn nme psyns] args
                        _ (when (and (= 3 (count args))
                                     (not (vector? psyns)))
                            (err/int-error
                              (str "3rd argument to '!' must be a vector")
                              opts))
                        _ (when-not (<= 2 (count args) 3)
                            (throw (ex-info "Bad arguments to '!'"
                                            {:form syn})))
                        t (parse tsyn opts)
                        p (when (= 3 (count args))
                            (mapv #(parse-path-elem % opts) psyns))]
                    (cond-> 
                      {:op :not-type-filter
                       :type t
                       :id nme}
                      p (assoc :path p)))
                  &
                  {:op :and-filter
                   :fs (mapv #(parse-filter % opts) args)}
                  when
                  (let [[a c] args]
                    (when-not (= 2 (count args))
                      (throw (ex-info "Bad arguments to 'when'"
                                      {:form syn})))
                    {:op :impl-filter
                     :a (parse-filter a opts)
                     :c (parse-filter c opts)})
                  (cond
                    (or (= 'or f)
                        (and (simple-symbol? f)
                             ;; clojure-clr treats pipes in symbols as special
                             (= "|" (name f))))
                    {:op :or-filter
                     :fs (mapv #(parse-filter % opts) args)}))))]
      (if m
        m
        (err/int-error (str "Bad filter syntax: " syn) opts)))))


#?(:cljr

  (.bindRoot #'*allow-symbol-escape* init-symbol-escape)
)

(t/defalias FilterSet
  '{:op ':filter-set
    :then Filter
    :else Filter})

(t/ann parse-filter-set [t/Any t/Any -> FilterSet])
(defn parse-filter-set [syn opts]
  (when-not (map? syn)
    (err/int-error "Filter set must be a map" opts))
  (let [then (:then syn)
        else (:else syn)]
    {:op :filter-set
     :then (if then
             (parse-filter then opts)
             {:op :top-filter})
     :else (if else
             (parse-filter else opts)
             {:op :top-filter})}))

(t/defalias NameRef (t/U t/Sym t/Int))

(t/ann ^:no-check name-ref? (t/Pred NameRef))
(def name-ref? (some-fn symbol? (every-pred integer? (complement neg?))))

(t/defalias PathElem
  (t/U '{:op ':ClassPE}
       '{:op ':CountPE}
       '{:op ':KeysPE}
       '{:op ':ValsPE}
       '{:op ':NthPE
         :idx t/Int}
       '{:op ':KeysPE
         :key t/Any}
       '{:op ':ValsPE
         :key t/Any}))

; no-check for perf issues
(t/ann ^:no-check parse-path-elem [t/Any t/Any -> PathElem])
(defn parse-path-elem [syn opts]
  (case syn
    Class {:op :ClassPE}
    Count {:op :CountPE}
    Keys {:op :KeysPE}
    Vals {:op :ValsPE}
    Keyword {:op :KeywordPE}
    (let [m (when (seq? syn)
              (let [[f & args] syn]
                (case f
                  Nth (do
                        (when-not (#{1} (count args))
                          (err/int-error (str "Wrong arguments to Nth: " syn) opts))
                        {:op :NthPE
                         :idx (first args)})
                  Key (do
                        (when-not (#{1} (count args))
                          (err/int-error (str "Wrong arguments to Key: " syn) opts))
                        {:op :KeyPE
                         :key (first args)})
                  Val (do
                        (when-not (#{1} (count args))
                          (err/int-error (str "Wrong arguments to Val" syn) opts))
                        {:op :ValPE
                         :val (first args)})
                  nil)))]
      (if m
        m
        (err/int-error (str "Bad path element syntax: " syn) opts)))))

(t/ann parse-object-path [t/Any t/Any -> FObject])
(defn parse-object-path [syn opts]
  (when-not (map? syn)
    (err/int-error (str "Object must be a map") opts))
  (let [id (:id syn)
        path (:path syn)]
    (when-not (name-ref? id)
      (err/int-error (str "Must pass natural number or symbol as id: " (pr-str id)) opts))
    (when-not ((some-fn nil? vector?) path)
      (err/int-error "Path must be a vector" opts))
    (when (contains? syn :path)
      (when-not (vector? path)
        (err/int-error "Path must be a vector" opts)))
    (merge
      {:op :object
       :id id}
      (when path
        {:path-elems (mapv #(parse-path-elem % opts) path)}))))

(defn parse-object [syn opts]
  (case syn
    empty {:op :empty-object}
    (parse-object-path syn opts)))

(t/defalias RestDrest
  (HMap :mandatory {:types (t/U nil (t/Coll Type))}
        :optional {:rest (t/U nil Type)
                   :drest (t/U nil DottedPretype)}))

(t/ann parse-with-rest-drest [t/Str (t/Seq t/Any) t/Any -> RestDrest])
(defn parse-with-rest-drest [msg syns opts]
  (let [syns (vec syns)
        rest? (#{:* '*} (peek syns))
        dotted? (and (#{:... '...} (some-> (not-empty syns) pop peek))
                     (<= 3 (count syns)))
        _ (when (and rest? dotted?)
            (err/int-error (str msg syns) opts))
        {:keys [types rest drest]}
        (cond
          rest?
          (let [fixed (mapv #(parse % opts) (-> syns pop pop))
                rest (parse (-> syns pop peek) opts)]
            {:types fixed
             :rest rest})
          dotted?
          (let [fixed (mapv #(parse % opts) (-> syns pop pop pop))
                [drest-type _dots_ drest-bnd :as dot-syntax] (take-last 3 syns)
                ; should never fail, if the logic changes above it's probably
                ; useful to keep around.
                _ (when-not (symbol? drest-bnd)
                    (err/int-error "Dotted rest bound after ... must be a symbol" opts))
                _ (when-not (#{3} (count dot-syntax))
                    (err/int-error (str "Bad vector syntax: " dot-syntax) opts))
                bnd (*dotted-scope* drest-bnd)
                _ (when-not bnd 
                    (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable") opts))
                gdrest-bnd (gensym bnd)]
            {:types fixed
             :drest (t/ann-form
                      {:op :dotted-pretype
                       :f {:op :F :name gdrest-bnd}
                       :drest (with-frees {drest-bnd gdrest-bnd} ;with dotted bound in scope as free
                                (parse drest-type opts))
                       :name gdrest-bnd}
                      DottedPretype)})
          :else {:types (mapv #(parse % opts) syns)})]
    {:types types
     :rest rest
     :drest drest}))

(defn parse-h* [op msg]
  (fn [[_ fixed & {:keys [filter-sets objects repeat]}] opts]
    (let [{:keys [types drest rest]}
          (parse-with-rest-drest msg fixed opts)]
      (merge
        {:op op
         :types types
         :children (vec (concat
                          [:types]
                          (when drest
                            [:drest])
                          (when rest
                            [:rest])
                          (when filter-sets
                            [:filter-sets])
                          (when objects
                            [:objects])
                          (when (true? repeat) [:repeat])))}
        (when drest
          {:drest drest})
        (when rest
          {:rest rest})
        (when filter-sets
          {:filter-sets (mapv #(parse-filter-set % opts) filter-sets)})
        (when objects
          {:objects (mapv #(parse-object % opts) objects)})
        (when (true? repeat) {:repeat true})))))

(def parse-HVec (parse-h* :HVec "Invalid HVec syntax:"))
(def parse-HSequential (parse-h* :HSequential "Invalid HSequential syntax:"))
(def parse-HSeq (parse-h* :HSeq "Invalid HSeq syntax:"))
(def parse-HList (parse-h* :HList "Invalid HList syntax:"))

(def parse-quoted-hvec (fn [syn opts]
                         (parse-HVec [nil syn] opts)))

(defn parse-quoted-hseq [syn opts]
  (let [types (mapv #(parse % opts) syn)]
    {:op :HSeq
     :types types
     :children [:types]}))

(defn parse-quoted-hlist [syn opts]
  (let [types (mapv #(parse % opts) syn)]
    {:op :HList
     :types types
     :children [:types]}))

(defn- syn-to-hmap [mandatory optional absent-keys complete? opts]
  (when mandatory
    (when-not (map? mandatory)
      (err/int-error (str "Mandatory entries to HMap must be a map: " mandatory) opts)))
  (when optional
    (when-not (map? optional)
      (err/int-error (str "Optional entries to HMap must be a map: " optional) opts)))
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [{:op :singleton :val k}
                        (parse v opts)])))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (err/int-error (str "HMap options contain duplicate key entries: ") opts))
          _ (when-not (every? keyword? (keys mandatory)) (err/int-error "HMap's mandatory keys must be keywords" opts))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (err/int-error "HMap's optional keys must be keywords" opts))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (err/int-error "HMap's absent keys must be keywords" opts))
          absent-keys (set (map (fn [a] {:op :singleton :val a}) absent-keys))]
      {:op :HMap
       :mandatory (vec (apply concat mandatory))
       :optional (vec (apply concat optional))
       :complete? complete?
       :absent-keys (vec absent-keys)
       :children [:mandatory :optional :absent-keys]})))

(defn parse-quote [[_ syn] opts]
  (cond
    ((some-fn number? keyword? symbol?) syn) {:op :singleton :val syn}
    (vector? syn) (parse-quoted-hvec syn opts)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false opts)
    :else (err/int-error (str "Invalid use of quote:" (pr-str syn)) opts)))

(def any-bounds {:op :bounds
                 :upper-bound {:op :Any}
                 :lower-bound {:op :U :types []}})

#?(
:cljr
(def clj-primitives
  {'byte     {:op :clj-prim :name 'byte}
   'sbyte    {:op :clj-pimr :name 'sbyte}
   'short    {:op :clj-prim :name 'short}
   'int      {:op :clj-prim :name 'int}
   'long     {:op :clj-prim :name 'long}
   'ushort   {:op :clj-prim :name 'ushort}
   'uint     {:op :clj-prim :name 'uint}
   'ulong    {:op :clj-prim :name 'ulong}
   'float    {:op :clj-prim :name 'float}
   'double   {:op :clj-prim :name 'double}
   'boolean  {:op :clj-prim :name 'boolean}
   'char     {:op :clj-prim :name 'char}
   'decimal  {:op :clj-prim :name 'decimal}
   'void     {:op :singleton :val nil}})

:default
(def clj-primitives
  {'byte     {:op :clj-prim :name 'byte}
   'short    {:op :clj-prim :name 'short}
   'int      {:op :clj-prim :name 'int}
   'long     {:op :clj-prim :name 'long}
   'float    {:op :clj-prim :name 'float}
   'double   {:op :clj-prim :name 'double}
   'boolean  {:op :clj-prim :name 'boolean}
   'char     {:op :clj-prim :name 'char}
   'void     {:op :singleton :val nil}})
   )

(def cljs-primitives
  {'int     {:op :cljs-prim :name 'int}
   'object  {:op :cljs-prim :name 'object}})

(defn parse-free [f gsym opts]
  (if (symbol? f)
    {:op :F
     :name gsym
     :bounds any-bounds}
    (let [[n & {:keys [< >] :as fopts}] f]
      (when (contains? fopts :kind)
        (err/deprecated-warn "kind annotation for TFn parameters is removed" opts))
      (when (:variance fopts) 
        (err/int-error "Variance not supported for variables introduced with All" opts))
      {:op :F
       :name gsym
       :bounds {:upper (when (contains? fopts :<)
                         (parse < opts))
                :lower (when (contains? fopts :>)
                         (parse > opts))}})))

(defn parse-tfn-binder [[nme & opts-flat :as all] gsym opts]
  (let [_ (when-not (even? (count opts-flat))
            (err/int-error (str "Uneven arguments passed to TFn binder: "
                                (pr-str all)) opts))
        {:keys [variance < >] 
         :or {variance :inferred}
         :as fopts} 
        (apply hash-map opts-flat)]
    (when-not (symbol? nme)
      (err/int-error "Must provide a name symbol to TFn" opts))
    (when (contains? fopts :kind)
      (err/deprecated-warn "kind annotation for TFn parameters is removed" opts))
    #_(when-not (r/variance? variance)
      (err/int-error (str "Invalid variance: " (pr-str variance)) opts))
    {:name gsym :variance variance
     :bound {:op :bounds
             :upper-bound (when (contains? fopts :<)
                            (parse < opts))
             :lower-bound (when (contains? fopts :>)
                            (parse > opts))}}))

(defn parse-TFn
  [[_ binder bodysyn :as tfn] opts]
  (when-not (= 3 (count tfn))
    (err/int-error (str "Wrong number of arguments to TFn: " (pr-str tfn)) opts))
  (when-not (every? vector? binder)
    (err/int-error (str "TFn binder should be vector of vectors: " (pr-str tfn)) opts))
  (let [; don't scope a free in its own bounds. Should review this decision
        [fs free-maps] (reduce
                         (fn [[fs prsed] b]
                           (let [sym (first b)
                                 _ (assert (symbol? sym))
                                 gsym (gensym sym)
                                 fs (conj fs [sym gsym])]
                             (with-frees fs
                               [fs (conj prsed (parse-tfn-binder b gsym opts))])))
                         [{} []]
                         binder)
        bodyt (with-frees fs
                (parse bodysyn opts))]
    {:op :TFn
     :binder free-maps
     :body bodyt}))

(defn multi-frequencies 
  "Like frequencies, but only returns frequencies greater
  than one"
  [coll]
  (->> coll
       frequencies
       (filter (fn [[k freq]]
                 (when (< 1 freq)
                   true)))
       (into {})))

(defn parse-HMap
  [[_HMap_ & flat-opts :as all] opts]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (err/deprecated-warn
                                 "HMap syntax changed. Use :mandatory keyword argument instead of initial map"
                                 opts)
                               (first flat-opts))
        flat-opts (if deprecated-mandatory
                    (next flat-opts)
                    flat-opts)
        _ (when-not (even? (count flat-opts))
            (err/int-error (str "Uneven keyword arguments to HMap: " (pr-str all)) opts))
        flat-keys (->> flat-opts
                       (partition 2)
                       (map first))
        _ (when-not (every? keyword? flat-keys)
            (err/int-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                                #_#_" in: " (pr-str all)) opts))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (err/int-error (str "Repeated keyword argument to HMap: " (pr-str k)) opts)))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (err/int-error (str "Unsupported HMap keyword argument: " (pr-str k)) opts))
        _ (when (and deprecated-mandatory mandatory)
            (err/int-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap") opts))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete? opts)))

(defn parse-All
  [[_All_ & args :as syn] opts]
  (let [_ (when-not (#{2} (count args))
            (err/int-error "Wrong arguments to All" opts))
        [bnds type] args
        _ (when-not (vector? bnds)
            (err/int-error "Wrong arguments to All" opts))
        [bnds kwargs] (split-with (complement keyword?) bnds)
        _ (when-not (even? (count kwargs))
            (err/int-error "Wrong arguments to All" opts))
        {:keys [named] :as kwargs} kwargs
        dotted? (boolean 
                  ('#{...} (last bnds)))
        [fs frees-with-bnds] (reduce (fn [[fs prsed] fsyn]
                                       (let [sym (if (symbol? fsyn)
                                                   fsyn
                                                   (first fsyn))
                                             _ (assert (symbol? sym))
                                             gsym (gensym sym)
                                             fs (conj fs [sym gsym])]
                                         (with-frees fs
                                           [fs (conj prsed (parse-free fsyn gsym opts))])))
                                     [{} []]
                                     (if dotted?
                                       (drop-last 2 bnds)
                                       bnds))
        dvar-plain-name (when dotted?
                          (-> bnds butlast last))
        _ (assert ((some-fn nil? symbol?) dvar-plain-name))
        gdvar (gensym dvar-plain-name)
        dvar (when dotted?
               (parse-free dvar-plain-name gdvar opts))]
    (with-frees fs
      (with-dfrees (if dvar 
                     {dvar-plain-name (:name dvar)}
                     {})
        {:op (if dotted? :PolyDots :Poly)
         :binder (concat frees-with-bnds
                         (when dotted?
                           [dvar]))
         :named (or named {})
         :type (parse type opts)
         :children [:type]}))))

(defn parse-Extends
  [[_Extends_ & args :as syn] opts]
  (let [[extends & {:keys [without] :as fopts}] args]
    (when-not (empty? (set/difference (set (keys fopts)) #{:without}))
      (err/int-error (str "Invalid options to Extends:" (keys fopts)) opts))
    (when-not (vector? extends) 
      (err/int-error (str "Extends takes a vector of types: " (pr-str syn)) opts))
    {:op :Extends
     :types (mapv #(parse % opts) extends)
     :without (mapv #(parse % opts) without)
     :children [:types :without]}))

(defn parse-U
  [[_U_ & args] opts]
  {:op :U
   :types (mapv #(parse % opts) args)
   :children [:types]})

(defn parse-I
  [[_I_ & args] opts]
  {:op :I
   :types (mapv #(parse % opts) args)
   :children [:types]})

(t/ann parse-seq* [(t/Seq t/Any) t/Any -> Type])
(defmulti parse-seq*
  (fn [[n] opts]
    {:post [((some-fn nil? symbol?) %)]}
    (when (symbol? n)
      (or (impl/impl-case opts
            :clojure (resolve-type-clj->sym n opts)
            :cljs n)
          n))))

(defmethod parse-seq* 'quote [syn opts] (parse-quote syn opts))

(defn parse-Value [[f & args :as syn] opts]
  (when-not (#{1} (count args))
    (err/int-error (str "Wrong arguments to Value: " syn) opts))
  {:op :singleton
   :val (first args)})

(defmethod parse-seq* 'Value [syn opts] 
  (err/deprecated-plain-op 'Value 'Val opts)
  (parse-Value syn opts))
(defmethod parse-seq* 'typed.clojure/Value [syn opts] (parse-Value syn opts))
(defmethod parse-seq* 'typed.clojure/Val [syn opts] (parse-Value syn opts))

(defn parse-Difference [[f & args :as syn] opts]
  (let [_ (when-not (<= 2 (count args))
            (err/int-error "Wrong arguments to Difference" opts))
        [t & without] args]
    {:op :Difference
     :type (parse t opts)
     :without (mapv #(parse % opts) without)
     :children [:type :without]}))

(defmethod parse-seq* 'Difference [syn opts] 
  (err/deprecated-plain-op 'Difference opts)
  (parse-Difference syn opts))
(defmethod parse-seq* 'typed.clojure/Difference [syn opts] (parse-Difference syn opts))

(defn parse-Rec [[f & args :as syn] opts]
  (let [_ (when-not (#{2} (count args))
            (err/int-error "Wrong arguments to Rec" opts))
        [[sym :as binder] t] args
        gsym (gensym sym)]
    {:op :Rec
     :f {:op :F :name gsym}
     :type (with-frees {sym gsym}
             (parse t opts))
     :children [:type]}))

(defmethod parse-seq* 'Rec [syn opts] 
  (err/deprecated-plain-op 'Rec opts)
  (parse-Rec syn opts))
(defmethod parse-seq* 'typed.clojure/Rec [syn opts] (parse-Rec syn opts))

(defn parse-CountRange [[f & args :as syn] opts]
  (let [_ (when-not (#{1 2} (count args))
            (err/int-error "Wrong arguments to CountRange" opts))
        [l u] args]
    {:op :CountRange
     :upper u
     :lower l}))

(defmethod parse-seq* 'CountRange [syn opts] 
  (err/deprecated-plain-op 'CountRange opts)
  (parse-CountRange syn opts))
(defmethod parse-seq* 'typed.clojure/CountRange [syn opts] (parse-CountRange syn opts))

(defn parse-ExactCount [[f & args :as syn] opts]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Wrong arguments to ExactCount" opts))
        [n] args]
    {:op :CountRange
     :upper n
     :lower n}))

(defmethod parse-seq* 'ExactCount [syn opts] 
  (err/deprecated-plain-op 'ExactCount opts)
  (parse-ExactCount syn opts))
(defmethod parse-seq* 'typed.clojure/ExactCount [syn opts] (parse-ExactCount syn opts))

(defn parse-Pred [[f & args :as syn] opts]
  (let [_ (when-not (#{1} (count args))
            (err/int-error (str "Wrong arguments to " f) opts))
        [t] args]
    {:op :predicate
     :type (parse t opts)
     :children [:type]}))

(defmethod parse-seq* 'predicate [syn opts] 
  (err/deprecated-plain-op 'predicate 'Pred opts)
  (parse-Pred syn opts))
(defmethod parse-seq* 'typed.clojure/Pred [syn opts] (parse-Pred syn opts))

(defn parse-Assoc [[f & args :as syn] opts]
  (let [_ (when-not (<= 1 (count args))
            (err/int-error "Wrong arguments to Assoc" opts))
        [t & entries] args
        {ellipsis-pos '...}
        (zipmap entries (range))

        [entries dentries] (split-at (if ellipsis-pos
                                       (dec ellipsis-pos)
                                       (count entries))
                                     entries)
        _ (when-not (-> entries count even?)
            (err/int-error (str "Incorrect Assoc syntax: "
                                syn
                                " , must have even number of key/val pair.") opts))
        _ (when-not (or (not ellipsis-pos)
                        (= (count dentries) 3))
            (err/int-error (str "Incorrect Assoc syntax: "
                                syn
                                " , must have even number of key/val pair.") opts))
        [drest-type _ drest-bnd] (when ellipsis-pos
                                   dentries)
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol" opts))]
    {:op :Assoc
     :type (parse t opts)
     :entries (mapv #(parse % opts) entries)
     :dentries (when ellipsis-pos
                 (let [bnd (*dotted-scope* drest-bnd)
                       _ (when-not (symbol? bnd)
                           (err/int-error (str (pr-str drest-bnd)
                                               " is not in scope as a dotted variable") opts))
                       gbnd (gensym bnd)]
                   {:drest
                    {:op :dotted-pretype
                     :f {:op :F :name gbnd}
                     :drest (with-frees {drest-bnd gbnd} ;with dotted bound in scope as free
                              (parse drest-type opts))
                     :name gbnd}}))
     :children (concat [:type :entries] (when ellipsis-pos [:dentries]))}))

(defmethod parse-seq* 'Assoc [syn opts] 
  (err/deprecated-plain-op 'Assoc opts)
  (parse-Assoc syn opts))
(defmethod parse-seq* 'typed.clojure/Assoc [syn opts] (parse-Assoc syn opts))

(defn parse-Get [[f & args :as syn] opts]
  (let [_ (when-not (#{2 3} (count args))
            (err/int-error "Wrong arguments to Get" opts))
        [t ksyn not-foundsyn] args]
    (merge 
      {:op :Get
       :type (parse t opts)
       :key (parse ksyn opts)
       :children [:type :key]}
      (when (#{3} (count args))
        {:not-found (parse not-foundsyn opts)
         :children [:type :key :not-found]}))))

(defmethod parse-seq* 'typed.clojure/Get [syn opts] (parse-Get syn opts))

(defmethod parse-seq* 'typed.clojure/All [syn opts] (parse-All syn opts))

(defmethod parse-seq* 'Extends [syn opts] (parse-Extends syn opts))

(defmethod parse-seq* 'typed.clojure/U [syn opts] (parse-U syn opts))

(defmethod parse-seq* 'I [syn opts] 
  (err/deprecated-plain-op 'I opts)
  (parse-I syn opts))
(defmethod parse-seq* 'typed.clojure/I [syn opts] (parse-I syn opts))

(defn parse-Array [[f & args :as syn] opts]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Expected 1 argument to Array" opts))
        [syn] args
        t (parse syn opts)]
    {:op :Array
     :read t
     :write t
     :children [:read :write]}))

(defmethod parse-seq* 'Array [syn opts] (parse-Array syn opts))

(defn parse-Array2 [[f & args :as syn] opts]
  (let [_ (when-not (#{2} (count args))
            (err/int-error "Expected 2 arguments to Array2" opts))
        [wsyn rsyn] args
        w (parse wsyn opts)
        r (parse rsyn opts)]
    {:op :Array
     :read r
     :write w
     :children [:read :write]}))

(defmethod parse-seq* 'Array2 [syn opts] (parse-Array2 syn opts))

(defn parse-ReadOnlyArray [[f & args :as syn] opts]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Expected 1 arguments to ReadOnlyArray" opts))
        [rsyn] args
        r (parse rsyn opts)]
    {:op :Array
     :read r
     :write {:op :U :types []}
     :children [:read :write]}))

(defmethod parse-seq* 'ReadOnlyArray [syn opts] (parse-ReadOnlyArray syn opts))

(defn parse-Array3 [[f & args :as syn] opts]
  (let [_ (when-not (#{3} (count args))
            (err/int-error "Expected 3 arguments to Array3" opts))
        [wsyn rsyn jsyn] args
        w (parse wsyn opts)
        r (parse rsyn opts)]
    {:op :Array
     :read r
     :write {:op :U :types []
             :children [:types]}
     :java-syntax jsyn
     :children [:read :write]}))

(defmethod parse-seq* 'Array3 [syn opts] (parse-Array3 syn opts))

(defmethod parse-seq* 'typed.clojure/TFn [syn opts] (parse-TFn syn opts))

(t/defalias F
  '{:op ':F
    :name t/Sym})

(t/defalias DottedPretype
  '{:op ':dotted-pretype
    :f F
    :drest Type
    :name t/Sym})

(t/defalias FObject
  (t/U '{:op ':empty-object}
       (t/HMap :mandatory
               {:op ':object
                :id (t/U t/Sym t/Int)}
               :optional
               {:path-elems (t/Vec PathElem)})))

(t/defalias Function
  (t/HMap :mandatory
          {:op ':Fn-method
           :dom (t/Vec Type)
           :rng Type
           :filter FilterSet
           :object FObject
           :children (t/Vec t/Kw)}
          :optional
          {:rest Type
           :drest DottedPretype}))

(t/ann parse-function [t/Any t/Any -> Function])
(defn parse-function [f opts]
  (when-not (vector? f) 
    (err/int-error "Function arity must be a vector" opts))
  (let [is-arrow '#{-> :->}
        all-dom (take-while (complement is-arrow) f)
        [the-arrow rng & opts-flat :as chk] (drop-while (complement is-arrow) f) ;opts aren't used yet
        ;TODO deprecate
        ;_ (when ('#{->} the-arrow)
        ;    )
        _ (when-not (<= 2 (count chk)) 
            (err/int-error (str "Incorrect function syntax: " f) opts))

        _ (when-not (even? (count opts-flat)) 
            (err/int-error (str "Incorrect function syntax, must have even number of keyword parameters: " f) opts))

        fopts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*
         ampersand-pos '&
         push-rest-pos '<*
         push-dot-pos '<...}
        (zipmap all-dom (range))

        _ (when-not (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos push-rest-pos push-dot-pos])))
            (err/int-error "Can only provide one rest argument option: & ... * or <*" opts))

        _ (when-let [ks (seq (remove #{:filters :object} (keys fopts)))]
            (err/int-error (str "Invalid function keyword option/s: " ks) opts))

        filters (when-let [[_ fsyn] (find fopts :filters)]
                  (parse-filter-set fsyn opts))

        object (when-let [[_ obj] (find fopts :object)]
                 (parse-object obj opts))

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    ampersand-pos (take ampersand-pos all-dom)
                    push-rest-pos (take (dec push-rest-pos) all-dom)
                    push-dot-pos (take (dec push-dot-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        _ (when-not (or (not asterix-pos)
                        (= (count all-dom) (inc asterix-pos)))
            (err/int-error (str "Trailing syntax after rest parameter: " (pr-str (drop (inc asterix-pos) all-dom))) opts))
        [drest-type _ drest-bnd :as drest-seq] (when ellipsis-pos
                                                 (drop (dec ellipsis-pos) all-dom))
        _ (when-not (or (not ellipsis-pos) (= 3 (count drest-seq))) 
            (err/int-error "Dotted rest entry must be 3 entries" opts))
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol" opts))
        [pdot-type _ pdot-bnd :as pdot-seq] (when push-dot-pos
                                                 (drop (dec push-dot-pos) all-dom))
        _ (when-not (or (not push-dot-pos) (= 3 (count pdot-seq)))
            (err/int-error "push dotted rest entry must be 3 entries" opts))
        _ (when-not (or (not push-dot-pos) (symbol? pdot-bnd))
            (err/int-error "push dotted bound must be symbol" opts))
        [& {optional-kws :optional mandatory-kws :mandatory} :as kws-seq]
        (let [kwsyn (when ampersand-pos
                      (drop (inc ampersand-pos) all-dom))]
          ; support deprecated syntax [& {} -> ] to be equivalent to [& :optional {} -> ]
          (if (and kwsyn
                   (map? (first kwsyn)))
            (do (err/deprecated-warn "[& {} -> ] function syntax is deprecated. Use [& :optional {} -> ]" opts)
                (cons :optional kwsyn))
            kwsyn))

        _ (when-not (or (not ampersand-pos) (seq kws-seq)) 
            (err/int-error "Must provide syntax after &" opts))

        prest-type (when push-rest-pos
                     (nth all-dom (dec push-rest-pos)))
        _ (when-not (or (not push-rest-pos)
                        (= (count all-dom) (inc push-rest-pos)))
            (err/int-error (str "Trailing syntax after push-rest parameter: " (pr-str (drop (inc push-rest-pos) all-dom))) opts))]
    (merge
      {:op :Fn-method
       :dom (mapv #(parse % opts) fixed-dom)
       :rng (parse rng opts)
       :filter filters
       :object object
       :children (vec (concat [:dom :rng :filter :object]
                              (when asterix-pos
                                [:rest])
                              (when ellipsis-pos
                                [:drest])
                              (when push-rest-pos
                                [:prest])
                              (when push-dot-pos
                                [:pdot])))}
      (when asterix-pos
        {:rest (parse rest-type opts)})
      (when ellipsis-pos
        (let [bnd (*dotted-scope* drest-bnd)
              _ (when-not (symbol? bnd)
                  (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable") opts))
              gbnd (gensym bnd)]
          {:drest
           {:op :dotted-pretype
            :f {:op :F :name gbnd}
            :drest (with-frees {drest-bnd gbnd} ;with dotted bound in scope as free
                     (parse drest-type opts))
            :name gbnd}}))
      (when push-rest-pos
        {:prest (parse prest-type opts)})
      (when push-dot-pos
        (let [bnd (*dotted-scope* pdot-bnd)
              _ (when-not (symbol? bnd)
                  (err/int-error (str (pr-str pdot-bnd) " is not in scope as a dotted variable") opts))
              gbnd (gensym bnd)]
          {:pdot
           {:op :dotted-pretype
            :f {:op :F :name gbnd}
            :drest (with-frees {pdot-bnd gbnd} ;with dotted bound in scope as free
                     (parse pdot-type opts))
            :name gbnd}})))))

(defn parse-Fn [[_ & args :as syn] opts]
  {:op :Fn
   :arities (mapv #(parse-function % opts) args)
   :form syn
   :children [:arities]})

(defmethod parse-seq* 'Fn [syn opts] 
  (err/deprecated-plain-op 'Fn 'IFn opts)
  (parse-Fn syn opts))
(defmethod parse-seq* 'typed.clojure/IFn [syn opts] (parse-Fn syn opts))

(defmethod parse-seq* 'HMap [syn opts] 
  (err/deprecated-plain-op 'HMap opts)
  (parse-HMap syn opts))
(defmethod parse-seq* 'typed.clojure/HMap [syn opts] (parse-HMap syn opts))

(defmethod parse-seq* 'Seq* [syn opts] 
  (err/deprecated-plain-op 'Seq* 'HSeq opts)
  (parse-quoted-hseq (rest syn) opts))

(defmethod parse-seq* 'HVec [syn opts] 
  (err/deprecated-plain-op 'HVec opts)
  (parse-HVec syn opts))
(defmethod parse-seq* 'typed.clojure/HVec [syn opts] (parse-HVec syn opts))

(defmethod parse-seq* 'HSequential [syn opts] 
  (err/deprecated-plain-op 'HSequential opts)
  (parse-HSequential syn opts))
(defmethod parse-seq* 'typed.clojure/HSequential [syn opts] (parse-HSequential syn opts))

(defmethod parse-seq* 'HSeq [syn opts] 
  (err/deprecated-plain-op 'HSeq opts)
  (parse-HSeq syn opts))

(defmethod parse-seq* 'typed.clojure/HSeq [syn opts] (parse-HSeq syn opts))

(defmethod parse-seq* 'typed.clojure/HList [syn opts] (parse-HList syn opts))

(defn parse-HSet [[_ ts & {:keys [complete?] :or {complete? true}} :as args]]
  {:op :HSet
   :fixed ts
   :complete? complete?})

(defmethod parse-seq* 'typed.clojure/HSet [syn opts] (parse-HSet syn))

(defmethod parse-seq* :default [[f & args :as syn] opts]
  {:op :TApp
   :rator (parse f opts)
   :rands (mapv #(parse % opts) args)
   :children [:rator :rands]})

(defn parse-Not [[f & args :as syn] opts]
  (let [_ (when-not (#{1} (count args))
            (err/int-error "Wrong arguments to Not" opts))]
    {:op :Not
     :type (parse (first args) opts)}))

(defmethod parse-seq* 'Not [syn opts] (parse-Not syn opts))

(defn parse-seq [syn opts]
  (parse-seq* syn opts))

(defmulti parse-symbol*
  (fn [n opts]
    {:pre [(symbol? n)]}
    (or (impl/impl-case opts
          :clojure (resolve-type-clj->sym n opts)
          ;TODO
          :cljs n)
        n)))

(defn parse-Any [s] {:op :Any :form s})
(defn parse-Nothing [s opts]
  (parse-U `(typed.clojure/U) opts))

(defmethod parse-symbol* 'typed.clojure/Any [s opts] (parse-Any s))

(defmethod parse-symbol* 'typed.clojure/Nothing [s opts] (parse-Nothing s opts))

(defn parse-AnyFunction [s]
  {:op :AnyFunction :form s})

(defmethod parse-symbol* 'typed.clojure/AnyFunction [s opts] (parse-AnyFunction s))

(defmethod parse-symbol* :default
  [sym opts]
  (let [checker ((requiring-resolve 'typed.cljc.runtime.env/checker) opts)
        primitives (impl/impl-case opts
                     :clojure clj-primitives
                     :cljs cljs-primitives)
        free (when (symbol? sym)
               (*tvar-scope* sym))]
    (cond
      free {:op :F :name free :form sym}
      (primitives sym) (assoc (primitives sym)
                              :form sym)
      :else 
        (or (impl/impl-case opts
              :clojure (let [res (when (symbol? sym)
                                   (resolve-type-clj sym opts))]
                         (cond 
                           (class? res) (let [csym (coerce/Class->symbol res)
                                              dt? (contains? (impl/datatype-env checker) csym)]
                                          {:op (if dt? :DataType :Class) :name csym
                                           :form sym})
                           (var? res) (let [vsym (coerce/var->symbol res)
                                            vsym-nsym (-> vsym namespace symbol)
                                            vsym (symbol (name (ns-rewrites-clj vsym-nsym vsym-nsym))
                                                         (name vsym))]
                                        (if (contains? (impl/alias-env checker) vsym)
                                          {:op :Name :name vsym :form sym}
                                          {:op :Protocol :name vsym :form sym}))
                           (symbol? sym)
                           (if-let [qsym (resolve-type-alias-clj sym opts)]
                             ; a type alias without an interned var
                             {:op :Name :name qsym :form sym}
                             ;an annotated datatype that hasn't been defined yet
                             ; assume it's in the current namespace
                                      ; do we want to munge the sym also?
                             (let [qname (symbol (str (namespace-munge (parse-in-ns opts)) "." sym))]
                               (when (contains? (impl/datatype-env checker) qname)
                                 {:op :DataType :name qname :form sym})))))
              :cljs (assert nil)
               #_(when-let [res (when (symbol? sym)
                                      (resolve-type-cljs sym))]
                       (:name res)))
            (err/int-error (str "Cannot resolve type: " (pr-str sym)) opts)))))

(defn parse-symbol [s opts]
  (parse-symbol* s opts))

(def ^:private register! (delay (t/register!)))

(defn parse [syn opts]
  @register!
  (let [opts (update opts ::vs/current-env #(let [ne (when-let [m (meta syn)]
                                                       (select-keys m [:file :line :column :end-line :end-column]))]
                                              (or (when ((every-pred :file :line :column) ne)
                                                    ne)
                                                  %)))]
    (cond
      ((some-fn nil? true? false?) syn) {:op :singleton :val syn :form syn}
      (vector? syn) {:op :Fn 
                     :arities [(parse-function syn opts)]
                     :form syn
                     :children [:arities]}
      (symbol? syn) (assoc (parse-symbol syn opts)
                           :form syn)
      (seq? syn) (assoc (parse-seq syn opts)
                        :form syn)
      :else (err/int-error (str "Bad type syntax: " syn) opts))))

(defn parse-clj [syn]
  (impl/with-impl impl/clojure
    (parse syn ((requiring-resolve 'typed.clj.runtime.env/clj-opts)))))

(comment
  (parse-clj `'[String ~'* t/Any])
  (= (parse 'nil)
     {:op :singleton
      :val nil
      :form nil})
  (= (parse 'true)
     {:op :singleton
      :val true
      :form true})

  (= (parse 'true)
     {:op :singleton
      :val true
      :form true})

  (impl/with-impl impl/clojure
    (parse 'a))
  )

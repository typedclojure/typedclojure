;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.annotator.parse
  (:require [typed.clj.annotator.rep :as r
             :refer [-any -class -nothing -alias make-HMap]]
            [typed.clj.annotator.util
             :refer [alias-env *envs*]]
            [typed.clj.annotator.join
             :refer [make-Union]]))

(def ^:dynamic *type-var-scope* #{})

(declare parse-type)

(defn parse-HVec [v]
  {:op :HVec 
   :vec (mapv parse-type v)})

(defn parse-literal-HMap [m]
  {:op :HMap
   :typed.clj.annotator.rep/HMap-req
   (into {}
         (map (fn [[k v]]
                [k (parse-type v)]))
         m)
   :typed.clj.annotator.rep/HMap-opt {}})

(defn parse-HMap [[_ & {:keys [mandatory optional]}]]
  (let [prs-map (fn [m]
                  (into {}
                        (map (fn [[k v]]
                               [k (parse-type v)]))
                        m))]
    (make-HMap (prs-map mandatory)
               (prs-map optional))))

(defn parse-arity [a]
  (let [[doms [_->_ rng :as rng-arrow]] (split-with (complement #{:->}) a)
        [doms [_ rst :as has-rst]] (split-with (complement #{'&}) doms)
        _ (assert (#{0 2} (count has-rst)))
        _ (assert (= 2 (count rng-arrow)))]
    {:op :IFn1
     :dom (mapv parse-type doms)
     :rng (parse-type rng)
     :rest (when (seq has-rst)
             (parse-type rst))}))

(defn parse-type [m]
  (cond
    (#{'Any 'clojure.core.typed/Any 'typed.clojure/Any} m) -any
    (= '? m) {:op :unknown}

    (or (= nil m)
        (= false m)
        (keyword? m)) {:op :val :val m}

    (vector? m) {:op :IFn
                 :arities [(parse-arity m)]}

    (symbol? m) (case m
                  (typed.clojure/Nothing clojure.core.typed/Nothing Nothing) -nothing
                  (typed.clojure/Sym clojure.core.typed/Sym Sym) (-class :symbol [])
                  (Integer Long
                   java.lang.Long java.lang.Integer) (-class :int [])
                  (String java.lang.String) (-class :string [])
                  (Boolean) (-class :boolean [])
                  (Double) (-class :double [])
                  (Number clojure.lang.Number) (-class :number [])
                  (clojure.lang.IFn) (-class :ifn [])
                  (clojure.lang.Symbol Symbol) (-class :symbol [])
                  (cond
                    (contains? *type-var-scope* m)
                    {:op :var
                     :name m}

                    (contains? (alias-env @*envs*) m)
                    (-alias m)

                    :else
                    (throw (ex-info (str "No resolution for " m) {}))))
    (seq? m) (case (first m)
                All (let [[vs t :as rst] (second m)
                          _ (assert (= 2 (count rst)))]
                      {:op :poly
                       :known-params (into []
                                           (map (fn [m]
                                                  {:pre [(symbol? m)]}
                                                  m))
                                           vs)
                       :params {}
                       :type (binding [*type-var-scope* (into *type-var-scope* vs)]
                               (parse-type t))})
                quote (let [in (second m)]
                        (cond
                          (vector? in) (parse-HVec in)
                          (map? in) (parse-literal-HMap in)
                          (keyword? in) {:op :val :val in}
                          :else (assert nil (str "Bad quote: " m))))

                IFn {:op :IFn
                     :arities (mapv parse-arity (rest m))}
                U (make-Union
                    (into #{}
                          (map parse-type)
                          (rest m)))
                HMap (parse-HMap m)
                Vec (-class :vector
                            [(parse-type (second m))])
                (Seqable clojure.lang.Seqable) (-class :seqable
                                                       [(parse-type (second m))])
                (PersistentHashSet clojure.lang.PersistentHashSet
                                   IPersistentSet
                                   clojure.lang.IPersistentSet)
                (-class :set [(parse-type (second m))])
                (typed.clojure/Map clojure.core.typed/Map
                  IPersistentMap
                  clojure.lang.IPersistentMap) (let [[_ k v] m]
                                                 (-class :map
                                                         [(parse-type k)
                                                          (parse-type v)]))
                Set (-class :set
                            [(parse-type (second m))])
                #?(:clj
                   (let [res (resolve (first m))]
                     (assert nil (str "TODO no more classes in :class" res))
                     (cond ;(contains? (alias-env @*envs*) (:name (first m)))
                           ;(-alias (first m))

                           (class? res) (-class res (mapv parse-type (drop 1 m)))

                           :else (assert nil (str "What is this?" m))))))


    :else (assert nil (str "bad type " m))))

#?(:clj
(defmacro prs [t]
  `(parse-type '~t)))

;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.utils
(ns typed.cljc.analyzer.utils
  (:refer-clojure :exclude [update-vals])
  #?(:cljs (:require [cljs.analyzer :as cljs-ana]))
  #?@(:cljs []
      ; cljs+cljr
      :default [(:import (clojure.lang IType IObj Var))]))

(defn into!
  "Like into, but for transients"
  [to from]
  (reduce conj! to from))

(defn merge!
  "Like `merge`, but accepts and returns a transient as the first arg."
  [to-transient m]
  (reduce-kv assoc! to-transient m))

(defn rseqv
  "Same as (comp vec rseq)"
  [v]
  (vec (rseq v)))

(defn ctx
  "Returns a copy of the passed environment with :context set to ctx"
  [env ctx]
  (assoc env :context ctx))

(defn dissoc-env
  "Dissocs :env from the ast"
  [ast]
  (dissoc ast :env))

(defn butlast+last
  "Returns same value as (juxt butlast last), but slightly more
   efficient since it only traverses the input sequence s once, not
   twice."
  [s]
  (loop [butlast (transient [])
         s s]
    (if-let [xs (next s)]
      (recur (conj! butlast (first s)) xs)
      [(seq (persistent! butlast)) (first s)])))

(defn update-kv
  "Applies f to all the keys and vals in the map"
  [m f]
  (into {}
        (map (fn [[k v]]
               [(f k) (f v)]))
        m))

(do
  #?@(:cljs []
      ; clj + cljr
      :default
      [(defn type?
         "Returns true if x is a type"
         [x]
         (instance? IType x))]))

(defn obj?
  "Returns true if x implements IObj"
  [x]
  #?(:cljs (implements? IWithMeta x)
     ; clj + cljr
     :default (instance? IObj x)))

(defn regex?
  "Returns true if x is a regex"
  [x]
  (instance? #?(:clj java.util.regex.Pattern
                :cljr System.Text.RegularExpressions.Regex
                :cljs js/RegExp
                :default (throw (ex-info "No impl for regex?")))
             x))

(defn classify
  "Returns a keyword describing the form type"
  [form]
  (cond
   (nil? form)     :nil
   (boolean? form) :bool
   (keyword? form) :keyword
   (symbol? form)  :symbol
   (string? form)  :string
   (number? form)  :number
   #?@(:cljs []
       ; clj+cljr
       :default [(type? form) :type])
   (record? form)  :record
   (map? form)     :map
   (vector? form)  :vector
   (set? form)     :set
   (seq? form)     :seq
   (char? form)    :char
   (regex? form)   :regex
   #?@(:cljs []
       ; clj+cljr
       :default [(class? form) :class])
   (var? form)     :var
   :else           :unknown))

(defn private?
  "Returns true if the var is private"
  ([var] (private? var nil))
  ([var m]
     (:private (or m (meta var)))))

(defn macro?
  "Returns true if the var maps to a macro"
  ([var] (macro? var nil))
  ([var m]
     (:macro (or m (meta var)))))

(defn constant?
  "Returns true if the var is a const"
  ([var] (constant? var nil))
  ([var m]
     (:const (or m (meta var)))))

(defn dynamic?
  "Returns true if the var is dynamic"
  ([var] (dynamic? var nil))
  ([var m]
     (or (:dynamic (or m (meta var)))
         #?@(:cljs []
             :default
             [(when (var? var) ;; workaround needed since Clojure doesn't always propagate :dynamic
                (.isDynamic ^Var var))]))))

(defn protocol-node?
  "Returns true if the var maps to a protocol function"
  ([var] (protocol-node? var nil))
  ([var m]
     (boolean (:protocol (or m (meta var)))))) ;; conveniently this is true in both clojure and clojurescript

(defn arglist-for-arity
  "Takes a fn node and an argc and returns the matching arglist"
  [fn argc]
  (let [arglists (->> fn :arglists (sort-by count))
        arglist (->> arglists (filter #(= argc (count %))) first)]
    (or arglist
        (let [last-arglist (last arglists)]
          (when (and (some '#{&} last-arglist)
                     (>= argc (- (count last-arglist) 2)))
            last-arglist)))))

(defn select-keys'
  "Like clojure.core/select-keys, but uses transients and doesn't preserve meta"
  [map keyseq]
  (loop [ret (transient {}) keys (seq keyseq)]
    (if keys
      (let [entry (find map (first keys))]
        (recur (if entry
                 (conj! ret entry)
                 ret)
               (next keys)))
      (persistent! ret))))

(defn merge'
  "Like merge, but uses transients"
  [m & mms]
  (persistent! (reduce merge! (transient (or m {})) mms)))

(defn mapv'
  "Like mapv, but short-circuits on reduced"
  [f v]
  (let [c (count v)]
    (loop [ret (transient []) i 0]
      (if (> c i)
        (let [val (f (nth v i))]
          (if (reduced? val)
            (reduced (persistent! (reduce conj! (conj! ret @val) (subvec v (inc i)))))
            (recur (conj! ret val) (inc i))))
        (persistent! ret)))))

(def ^:private source-info-keys #{:file :line :column :end-line :end-column :source-span})

(defn- source-info-into-transient! [m dest]
  (if (:line m)
    (reduce-kv (fn [acc k v]
                 (cond-> acc
                   (source-info-keys k)
                   (assoc! k v)))
               dest m)
    dest))

(defn source-info
  "Returns the available source-info keys from a map"
  [m]
  (when (:line m)
    (persistent!
     (source-info-into-transient! m (transient {})))))

(defn- -source-info-into-transient!
  "Like `-source-info`, but returns a raw transient."
  [x dest]
  (let [t (source-info-into-transient! (meta x) dest)]
    (if-let [file (let [file #?(:cljs cljs-ana/*cljs-file*
                                :default *file*)]
                    (and (not= file "NO_SOURCE_FILE")
                         file))]
      (assoc! t :file file)
      t)))

(defn -source-info
  "Returns the source-info of x"
  ([x env] (-source-info x env {}))
  ([x env base]
   (persistent! (if (identical? env base)
                  (-source-info-into-transient! x (transient base))
                  (->> (transient base)
                       (source-info-into-transient! env)
                       (-source-info-into-transient! x))))))

(defn const-val
  "Returns the value of a constant node (either :quote or :const)"
  [{:keys [form val]}]
  (or val form))

(def mmerge
  "Same as (fn [m1 m2] (merge-with merge m2 m1))"
  #(merge-with merge' %2 %1))

(defmacro update-record-child
  "Equivalent to (update this field f)"
  [this field f]
  (assert (simple-keyword? field))
  `(let [this# ~this]
     (assoc this# ~field (~f (~(symbol (str ".-" (name field))) this#)))))

(defmacro update-record-children 
  "Equivalent to (update this field #(mapv f %))"
  [this field f]
  (assert (simple-keyword? field))
  `(let [this# ~this]
     (assoc this# ~field (mapv ~f (~(symbol (str ".-" (name field))) this#)))))

#?(:clj
   (defn update-vals
     "m f => {k (f v) ...}

     Given a map m and a function f of 1-argument, returns a new map where the keys of m
     are mapped to result of applying f to the corresponding values of m."
     {:added "1.11"}
     [m f]
     (with-meta
       (persistent!
         (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                    (if (instance? clojure.lang.IEditableCollection m)
                      (transient m)
                      (transient {}))
                    m))
       (meta m)))
   :default (def update-vals clojure.core/update-vals))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.utils
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.clojure :as t]
            [clojure.core.typed.util-vars :as uvs]
            [typed.cljc.runtime.env-utils :as env-utils]
            [clojure.repl :as repl]
            [clojure.set :as set]))

(t/ann cs-gen-exn Exception)
(def cs-gen-exn (Exception. "Constraint generation failed."))

(defmacro handle-cs-gen-failure [& body]
  `(try
     (do ~@body)
     (catch Exception e#
       (if (identical? cs-gen-exn e#)
         ;; TODO return nil instead to use keep
         false
         (throw e#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defmacro ann-record 
  "Like ann-record, but also adds an unchecked annotation for core.contract's generated
  nme? predicate."
  [nme fields & {:keys [maker-name]}]
  `(do ~(-> `(clojure.core.typed/ann-record ~nme ~fields)
            (with-meta (meta &form)))
       ~(-> `(clojure.core.typed/ann ~(with-meta (or maker-name (symbol (str nme "-maker"))) {:no-check true})
                                     [~@(map #(nth % 2) (partition 3 fields)) (t/Option (t/Map t/Any t/Any)) :? :-> ~nme])
            (with-meta (meta &form)))))

(t/def ^:const default-xor :- Long, 1)

(t/tc-ignore

(defn ^:private inner-deftype [fields hash-field meta-field this that name-sym type-hash gs
                               maker methods* compute-valAt]
  (let [_ (assert (not-any? #(= name-sym %) (list* hash-field meta-field fields)))
        this (gensym 'this)
        gclass (gensym 'gclass)
        k (gensym 'k)
        ;; note: intentionally avoids condp throughout due to lack of inlining https://github.com/frenchy64/clojure/pull/13
        valAt-body `(cond
                      ~@(mapcat (fn [fld]
                                  (let [kfld (keyword fld)]
                                    [`(identical? ~k ~kfld)
                                     (if-some [f (get compute-valAt kfld)]
                                       ((eval f) this)
                                       `(. ~this ~(symbol (str "-" fld))))]))
                                fields)
                      :else (throw (UnsupportedOperationException. (str "lookup on " '~name-sym " " ~k))))]
    `(deftype ~name-sym [~@fields ~(with-meta hash-field {:unsynchronized-mutable true}) ~meta-field]
       clojure.lang.IHashEq
       (equals [~this ~that]
         (and (instance? ~name-sym ~that)
              ~@(let [that (with-meta that {:tag name-sym})]
                  (for [f fields
                        :let [f (symbol (str "-" f))]]
                    `(= (. ~that ~f)
                        (. ~this ~f))))))
       (hasheq [~this] (or (. ~this ~(symbol (str "-" hash-field)))
                           (let [h# ~(if-let [ts (seq (map (fn [f] `(hash (. ~this ~(symbol (str "-" f))))) fields))]
                                       `(bit-xor ~type-hash ~@ts)
                                       `(bit-xor ~type-hash ~default-xor))]
                             (set! (. ~this ~(symbol (str "-" hash-field))) h#)
                             h#)))
       (hashCode [~this] (or (. ~this ~(symbol (str "-" hash-field)))
                             (let [h# ~(if-let [ts (seq (map (fn [f] `(hash (. ~this ~(symbol (str "-" f))))) fields))]
                                         `(bit-xor ~type-hash ~@ts)
                                         `(bit-xor ~type-hash ~default-xor))]
                               (set! (. ~this ~(symbol (str "-" hash-field))) h#)
                               h#)))

       clojure.lang.IObj
       (meta [~this] (. ~this ~(symbol (str "-" meta-field))))
       ;; can use unchecked ctor
       (withMeta [~this ~gs] (new ~name-sym
                                  ~@(map #(do `(. ~this ~(symbol (str "-" %)))) fields)
                                  (. ~this ~(symbol (str "-" hash-field)))
                                  ~gs))

       clojure.lang.ILookup
       (valAt [~this ~k else#] ~valAt-body)
       (valAt [~this ~k] ~valAt-body)

       clojure.lang.IKeywordLookup
       (getLookupThunk [this# ~k]
         (let [~gclass (class this#)]
           (cond
             ~@(let [gtarget (gensym 'gtarget)
                     hinted-target (with-meta gtarget {:tag name-sym})
                     thunk (gensym 'thunk)]
                 (mapcat
                   (fn [fld]
                     (let [kfld (keyword fld)]
                       [`(identical? ~k ~kfld)
                        `(reify clojure.lang.ILookupThunk
                           (get [~thunk ~gtarget]
                             (if (identical? (class ~gtarget) ~gclass)
                               ~(if-some [f (get compute-valAt kfld)]
                                  ((eval f) hinted-target)
                                  `(. ~hinted-target ~(symbol (str "-" fld)))) 
                               ~thunk)))]))
                   fields))
             :else (throw (UnsupportedOperationException. (str "lookup on " '~name-sym " " ~k))))))

       clojure.lang.IPersistentMap
       (assoc [~this ~k ~gs]
         (cond
           ~@(mapcat (fn [fld]
                       [`(identical? ~k ~(keyword fld))
                        `(if (identical? ~gs (. ~this ~(symbol (str "-" fld))))
                           ~this ;; don't reconstruct if field is unchanged
                           (~maker ~@(map (fn [fld']
                                            (if (= fld fld')
                                              gs
                                              `(. ~this ~(symbol (str "-" fld')))))
                                          fields)
                                   (. ~this ~(symbol (str "-" meta-field)))))])
                     fields)
           :else (throw (UnsupportedOperationException. (str "assoc on " '~name-sym " " ~k)))))
       (entryAt [this# k#]
         (let [v# (.valAt this# k# this#)]
           (when-not (identical? this# v#)
             (clojure.lang.MapEntry/create k# v#))))
       (count [this#] (throw (UnsupportedOperationException. ~(str "count on " name-sym))))
       ;; hack for pr-on, don't use empty
       (empty [this#] this#)
       (cons [this# e#] (throw (UnsupportedOperationException. ~(str "cons on " name-sym))))
       (equiv [~this ~that]
         (and (instance? ~name-sym ~that)
              ~@(let [that (with-meta that {:tag name-sym})]
                  (for [f fields
                        :let [f (symbol (str "-" f))]]
                    `(= (. ~that ~f)
                        (. ~this ~f))))))
       (containsKey [this# k#] (throw (UnsupportedOperationException. ~(str "containsKey on " name-sym))))
       (seq [~this] (seq [~@(map #(list `new `clojure.lang.MapEntry (keyword %) `(. ~this ~(symbol (str "-" %))))
                                 fields)]))

       (iterator [this#] (throw (UnsupportedOperationException. ~(str "iterator on " name-sym))))
       (without [this# k#] (throw (UnsupportedOperationException. ~(str "without on " name-sym))))

       Comparable
       (compareTo [~this ~that]
         ;returns 1 if we have 2 instances of name-sym with
         ; identical hashs, but are not =
         (if (= ~this ~that)
           0
           (if (instance? ~name-sym ~that)
             (if (< (hash ~this)
                    (hash ~that))
               -1
               1)
             (if (< (hash ~name-sym) (hash (class ~that)))
               -1
               1))))

       ~@methods*)))

(defn emit-deftype [original-ns def-kind name-sym fields invariants {methods* :methods :keys [computed-fields ctor-meta compute-valAt] :as opt}]
  (assert (symbol? name-sym))
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." name-sym)) (meta name-sym))
        ->ctor (symbol (-> *ns* ns-name str) (str "->" name-sym))
        maker (with-meta (or (:maker-name opt)
                             (symbol (str name-sym "-maker")))
                         ctor-meta)
        _ (assert (apply distinct? maker fields))
        pred (or (:pred-name opt) (symbol (str name-sym "?")))
        this (gensym)
        that (gensym)
        gs (gensym)
        type-hash (hash classname)
        meta-field '_meta
        hash-field '_hash;
        assertions (map #(do `(assert ~% [~@fields])) invariants)]
    `(do
       (declare ~maker)
       ~(inner-deftype fields hash-field meta-field this that name-sym type-hash gs
                       maker methods* compute-valAt)

       (swap! ~(symbol (str original-ns) (str "all-" def-kind "s")) conj '~classname)

       (alter-meta! (var ~->ctor) assoc :private true)

       (t/ann ~pred (t/Pred ~name-sym))
       (defn ~pred
         {:inline (fn [x#] (list 'instance? ~name-sym a#))}
         [a#]
         (instance? ~name-sym a#))

       (defn ~maker
         ([~@fields]
          (let [~@computed-fields]
            ~@assertions
            (~->ctor ~@fields nil nil)))
         ([~@fields meta#]
          (let [~@computed-fields]
            ~@assertions
            (~->ctor ~@fields nil meta#)))))))

(defmacro mk [original-ns def-kind name-sym fields invariants & {:keys [methods computed-fields maker-name] :as opts}]
  (when-not (resolve name-sym)
    (let [unannotated-fields (loop [out []
                                    fields (seq fields)]
                               (if (nil? fields)
                                 out
                                 (let [fst (first fields)]
                                   (if (keyword? fst)
                                     (recur out (nnext fields))
                                     (recur (conj out fst) (next fields))))))]
      `(t/tc-ignore
         (ann-record ~name-sym ~fields
                     ~@(when maker-name [:maker-name maker-name]))
         (env-utils/invalidate-parsed-types!)
         ~(emit-deftype original-ns def-kind name-sym unannotated-fields invariants opts)))))

(defmacro defspecial [name]
  (let [all-entries (symbol (str "all-" name "s"))]
    `(do (defn ~(symbol (str name "="))
           [t1# t2#]
           (= t1# t2#))
         (defn ~(symbol (str name "<"))
           [t1# t2#]
           (neg? (compare t1# t2#)))
         (def ~all-entries (atom #{}))
         (defmacro ~(symbol (str "def-" name))
           [name# fields# doc# invariants# & opts#]
           `(mk ~'~(ns-name *ns*)
                ~'~name
                ~name# 
                ~fields# 
                ~invariants# 
                ~@opts#)))))

(defspecial type)
(defspecial filter)
(defspecial object)
(defspecial path)

)

(t/ann typed-ns-opts [t/Any -> t/Any])
(defn typed-ns-opts [ns]
  (-> ns meta :core.typed))

(t/ann demunge-ns [(t/U t/Sym t/Str) -> t/Sym])
(defn demunge-ns [nsym]
  (symbol (repl/demunge (str nsym))))

(t/tc-ignore
;; multimethods for dispatching on special forms like (do ::special-form ::foobar ...)
(defn internal-dispatch-val [expr]
  (:form (second (:statements expr))))


(defmacro special-do-op
  "Define a multimethod that takes an expr and an expected type
  and dispatches on the second statement"
  [kw nme]
  `(defmulti ~nme (fn [expr# & _#] (internal-dispatch-val expr#))))

(defn internal-form? [expr kw]
  (= kw (:form (first (:statements expr)))))

(defn ns? [n]
  (instance? clojure.lang.Namespace n))

(def expr-type ::expr-type)

(t/ann tc-warning [t/Any :* :-> nil])
(defn tc-warning [& ss]
  (let [env uvs/*current-env*]
    (binding [*out* *err*]
      (println 
        (apply str "WARNING (" (:file env) ":" (:line env) 
               (when-let [col (:column env)]
                 (str ":" col))
               "): " ss))
      (flush))))

(defmacro with-tracing [& body]
  `(binding [uvs/*trace-checker* true]
     ~@body))

(defmacro trace [& ss]
  `(when uvs/*trace-checker*
     (println 
       "TRACE: " 
       " "
       (:line uvs/*current-env*)
       ~@ss)
     (flush)))

(defmacro trace-when [p & ss]
  `(when uvs/*trace-checker*
     (when ~p
       (println 
         "TRACE: " 
         " "
         (:line uvs/*current-env*)
         ~@ss)
       (flush))))

(defmacro trace-when-let [p & ss]
  `(when uvs/*trace-checker*
     (when-let ~p
       (println 
         "TRACE: " 
         " "
         (:line uvs/*current-env*)
         ~@ss)
       (flush))))

(defn pad-right
  "Pad s with v until length cnt."
  [^long cnt s v]
  {:pre [(integer? cnt)
         (<= (count s) cnt)]
   :post [(= cnt (count %))]}
  (concat s
          (repeat (- cnt (count s)) v)))

(defmacro rewrite-when [p & body]
  `(binding [vs/*can-rewrite* (if ~p
                                vs/*can-rewrite*
                                nil)]
     ~@body))

(defn core-typed-ns-meta 
  "Returns the :core.typed entry in the given namespace's
  metadata"
  [ns]
  {:pre [(instance? clojure.lang.Namespace ns)]}
  (-> ns meta :core.typed))

(defn ns-has-feature? [ns k]
  (-> (core-typed-ns-meta ns)
      :features
      (contains? k)))

(defn should-runtime-check-ns?
  [ns]
  (ns-has-feature? ns :runtime-check))

(defn should-runtime-infer-ns?
  [ns]
  (ns-has-feature? ns :runtime-infer))
)

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.utils
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.clojure :as t]
            [clojure.core.typed.util-vars :as uvs]
            [typed.cljc.runtime.env-utils :refer [invalidate-parsed-types!]]
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
  [nme & args]
  `(do ~(-> `(clojure.core.typed/ann-record ~nme ~@args)
            (with-meta (meta &form)))
       ~(-> `(clojure.core.typed/ann ~(with-meta (symbol (str nme "-maker")) {:no-check true})
                                     [~@(map #(nth % 2) (partition 3 (first args))) ~'-> ~nme])
            (with-meta (meta &form)))
       ~(-> `(clojure.core.typed/ann ~(with-meta (symbol (str nme "?")) {:no-check true}) ~(list `t/Pred nme))
            (with-meta (meta &form)))))

(t/tc-ignore
(def ^:const default-xor 1)

(defn ^:private inner-deftype [fields hash-field meta-field that name-sym type-hash gs
                               maker methods*]
  `(deftype ~name-sym [~@fields ~(with-meta hash-field {:unsynchronized-mutable true}) ~meta-field]
     clojure.lang.IHashEq
     (equals [_# ~that]
       (and (instance? ~name-sym ~that)
            ; do not shadow fields here!
            ~@(for [f fields]
                `(= (~(keyword f) ~that) ~f))))
     ; don't shadow fields here!
     (hasheq [this#] (if-let [h# ~hash-field]
                       h#
                       (let [h# ~(if-let [ts (seq (map (fn [f] `(hash ~f)) fields))]
                                   `(bit-xor ~type-hash ~@ts)
                                   `(bit-xor ~type-hash ~default-xor))]
                         (set! ~hash-field h#)
                         h#)))
     ; don't shadow fields here!
     (hashCode [this#] (if-let [h# ~hash-field]
                         h#
                         (let [h# ~(if-let [ts (seq (map (fn [f] `(hash ~f)) fields))]
                                     `(bit-xor ~type-hash ~@ts)
                                     `(bit-xor ~type-hash ~default-xor))]
                           (set! ~hash-field h#)
                           h#)))

     clojure.lang.IObj
     (meta [this#] ~meta-field)
     (withMeta [this# ~gs] (~maker ~@fields ~gs))


     clojure.lang.ILookup
     (valAt [this# k# else#]
       (case k# ~@(mapcat (fn [fld] [(keyword fld) fld]) 
                          fields)
         (throw (UnsupportedOperationException. (str "lookup on " '~name-sym " " k#)))))
     (valAt [this# k#]
       (case k# ~@(mapcat (fn [fld] [(keyword fld) fld]) 
                          fields)
         (throw (UnsupportedOperationException. (str "lookup on " '~name-sym " " k#)))))

     clojure.lang.IKeywordLookup
     (getLookupThunk [this# k#]
       (let [~'gclass (class this#)]              
         (case k#
           ~@(let [hinted-target (with-meta 'gtarget {:tag name-sym})] 
               (mapcat 
                 (fn [fld]
                   [(keyword fld)
                    `(reify clojure.lang.ILookupThunk
                       (get [~'thunk ~'gtarget]
                         (if (identical? (class ~'gtarget) ~'gclass)
                           (. ~hinted-target ~(symbol (str "-" fld)))
                           ~'thunk)))])
                 fields))
           (throw (UnsupportedOperationException. (str "lookup on " '~name-sym " " k#))))))

     clojure.lang.IPersistentMap
     (assoc [this# k# ~gs]
       (condp identical? k#
         ~@(mapcat (fn [fld]
                     [(keyword fld) `(~maker ~@(replace {fld gs} fields) ~meta-field)])
                   fields)
         (throw (UnsupportedOperationException. (str "assoc on " '~name-sym " " k#)))))
     (entryAt [this# k#]
       (let [v# (.valAt this# k# this#)]
         (when-not (identical? this# v#)
           (clojure.lang.MapEntry/create k# v#))))
     (count [this#] (throw (UnsupportedOperationException. (str "count on " '~name-sym))))
     ;; hack for pr-on, don't use empty
     (empty [this#] this#)
     (cons [this# e#] (throw (UnsupportedOperationException. (str "cons on " '~name-sym))))
     (equiv [_# ~that]
       (and (instance? ~name-sym ~that)
            ; do not shadow fields here!
            ~@(for [f fields]
                `(= (~(keyword f) ~that) ~f))))
     (containsKey [this# k#] (throw (UnsupportedOperationException. (str "containsKey on " '~name-sym))))
     (seq [this#] (seq [~@(map #(list `new `clojure.lang.MapEntry (keyword %) %) (concat fields [#_meta-field]))]))

     (iterator [this#] (throw (UnsupportedOperationException. (str "iterator on " '~name-sym))))
     (without [this# k#] (throw (UnsupportedOperationException. (str "without on " '~name-sym))))

     Comparable
     ~(let [this (gensym 'this)]
        `(compareTo [~this ~that]
                    ;returns 1 if we have 2 instances of name-sym with
                    ; identical hashs, but are not =
                    (cond (= ~this ~that) 0
                          (instance? ~name-sym ~that)
                          (if (< (hash ~this)
                                 (hash ~that))
                            -1
                            1)
                          :else (if (< (hash ~name-sym) (hash (class ~that)))
                                  -1
                                  1))))

     ~@methods*))

(defn emit-deftype [original-ns def-kind name-sym fields invariants methods*]
  (assert (symbol? name-sym))
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." name-sym)) (meta name-sym))
        ->ctor (symbol (str "->" name-sym))
        maker (symbol (str name-sym "-maker"))
        qmaker (symbol (-> *ns* ns-name str) (name maker))
        pred (symbol (str name-sym "?"))
        qpred (symbol (-> *ns* ns-name str) (name pred))
        that (gensym)
        gs (gensym)
        type-hash (hash classname)
        meta-field '_meta
        hash-field '_hash]
    `(do
       (declare ~maker)
       ~(inner-deftype fields hash-field meta-field that name-sym type-hash gs
                       maker methods*)

       (swap! ~(symbol (str original-ns) (str "all-" def-kind "s")) conj '~classname)

       (alter-meta! (var ~->ctor) assoc :private true)

       (if uvs/dev-mode?
         (let [self# (promise)]
           (defn ~pred [a#]
             (let [new-f# @(resolve '~qpred)]
               (if (identical? @self# new-f#)
                 (instance? ~name-sym a#)
                 (new-f# a#))))
           (deliver self# ~pred))
         (defn ~pred [a#]
           (instance? ~name-sym a#)))

       (declare ~maker)
       ; (Atom1 (Map t/Any Number))
       (if uvs/dev-mode?
         (let [self# (promise)]
           (defn ~maker
             ([~@fields]
              {:pre ~invariants}
              (let [new-f# @(resolve '~qmaker)]
                (if (identical? @self# new-f#)
                  (~->ctor ~@fields nil nil)
                  (new-f# ~@fields))))
             ([~@fields meta#]
              {:pre ~invariants}
              (let [new-f# @(resolve '~qmaker)]
                (if (identical? @self# new-f#)
                  (~->ctor ~@fields nil meta#)
                  (new-f# ~@fields meta#)))))
           (deliver self# ~maker))
         (defn ~maker
           ([~@fields]
            {:pre ~invariants}
            (~->ctor ~@fields nil nil))
           ([~@fields meta#]
            {:pre ~invariants}
            (~->ctor ~@fields nil meta#)))))))

(defmacro mk [original-ns def-kind name-sym fields invariants & {:keys [methods]}]
  (when-not (resolve name-sym)
    `(t/tc-ignore
       (invalidate-parsed-types!)
       ~(emit-deftype original-ns def-kind name-sym fields invariants methods))))

(defmacro defspecial [name]
  (let [all-entries (symbol (str "all-" name "s"))]
    `(do (defn ~(symbol (str name "="))
           [t1# t2#]
           (= t1# t2#))
         (defn ~(symbol (str name "<"))
           [t1# t2#]
           (neg? (compare t1# t2#)))
         (defn ~(symbol (str name "-comparator"))
           [t1# t2#]
           (compare t1# t2#))
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

;(t/ann tc-warning [t/Any * -> nil])
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

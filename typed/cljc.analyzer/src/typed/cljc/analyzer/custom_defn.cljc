(ns typed.cljc.analyzer.custom-defn
  (:refer-clojure :exclude [defn defn- fn assert destructure let])
  (:require [clojure.core :as core]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as core-specs]))

(defmacro assert [& args]
  (when (= (System/getProperty "typed-clojure.assertions") "true")
    `(core/assert ~@args)))

(core/defn- destructure [bindings]
  (letfn [(pvec [bvec b val]
            (core/let [gvec (gensym "vec__")
                       gseq (gensym "seq__")
                       gfirst (gensym "first__")
                       has-rest (some #{'&} b)]
              (loop [ret (core/let [ret (conj bvec gvec val)]
                           (if has-rest
                             (conj ret gseq (list `seq gvec))
                             ret))
                     n 0
                     bs b
                     seen-rest? false]
                (if (seq bs)
                  (core/let [firstb (first bs)]
                    (cond
                      (= firstb '&) (recur (pb ret (second bs) gseq)
                                           n
                                           (nnext bs)
                                           true)
                      (= firstb :as) (pb ret (second bs) gvec)
                      :else (if seen-rest?
                              (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                              (recur (pb (if has-rest
                                           (conj ret
                                                 gfirst `(first ~gseq)
                                                 gseq `(next ~gseq))
                                           ret)
                                         firstb
                                         (if has-rest
                                           gfirst
                                           (list `nth gvec n nil)))
                                     (inc n)
                                     (next bs)
                                     seen-rest?))))
                  ret))))
          (pmap [bvec b v]
            (core/let [gmap (gensym "map__")
                       gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
                       defaults (:or b)]
              (loop [ret (-> bvec
                             (conj gmap v)
                             (conj gmap `(if (seq? ~gmap)
                                           (if (next ~gmapseq)
                                             (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array ~gmapseq))
                                             (if (seq ~gmapseq) (first ~gmapseq) clojure.lang.PersistentArrayMap/EMPTY))
                                           ~gmap))
                             (into (when (:as b)
                                     [(:as b) gmap])))
                     bes (core/let [transforms
                                    (reduce
                                     (core/fn [transforms mk]
                                       (if (keyword? mk)
                                         (core/let [mkns (namespace mk)
                                                    mkn (name mk)]
                                           (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
                                                 (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
                                                 (= mkn "strs") (assoc transforms mk str)
                                                 :else transforms))
                                         transforms))
                                     {}
                                     (keys b))]
                           (reduce-kv
                            (core/fn [bes mk xform]
                              (reduce #(assoc %1 %2 (xform %2))
                                      (dissoc bes mk)
                                      (get bes mk)))
                            (dissoc b :as :or)
                            transforms))]
                (if (seq bes)
                  (core/let [bb (key (first bes))
                             bk (val (first bes))
                             local (if (instance? clojure.lang.Named bb) (with-meta (symbol nil (name bb)) (meta bb)) bb)
                             bv (if (contains? defaults local)
                                  (if (keyword? bk)
                                    (list bk gmap (defaults local))
                                    (list `get gmap bk (defaults local)))
                                  (if (keyword? bk)
                                    (list bk gmap)
                                    (list `get gmap bk)))]
                    (recur (if (ident? bb)
                             (-> ret (conj local bv))
                             (pb ret bb bv))
                           (next bes)))
                  ret))))
          (pb [bvec b v]
            (cond
              (symbol? b) (-> bvec (conj b) (conj v))
              (vector? b) (pvec bvec b v)
              (map? b) (pmap bvec b v)
              :else (throw (new Exception (str "Unsupported binding form: " b)))))]
    (core/let [bents (partition 2 bindings)]
      (if (every? symbol? (map first bents))
        bindings
        (reduce (core/fn [bvec [b v]] (pb bvec b v)) [] bents)))))

(core/defn- expand-pre-post-conditions [arity-body]
  (if (and (map? (first arity-body)) (next arity-body))
    (core/let [body (rest arity-body)
               {:keys [pre post]} (first arity-body)]
      (concat
       (dissoc (first arity-body) :pre :post)
       (map (core/fn [assertion] (list `assert assertion)) pre)
       (if post
         `((core/let [~'% ~(if (< 1 (count body))
                             `(do ~@body)
                             (first body))]
             ~@(map (core/fn [c] `(assert ~c)) post)
             ~'%))
         body)))
    arity-body))

(core/defn- replace-fn-tail [tail]
  (letfn [(expand-one [[arglist & body]]
            (core/let [body (expand-pre-post-conditions body)]
              (if (every? symbol? arglist)
                (list* arglist body)
                (core/let [simple-args (mapv #(symbol (str "arg" %)) (range (count arglist)))]
                  `(~simple-args
                    (core/let [~@(destructure (mapcat vector arglist simple-args))]
                      ~@body))))))]
    (if (vector? (first tail))
      (expand-one tail)
      (map expand-one tail))))

(defmacro fn [& args]
  (core/let [[fn-name fn-tail] (if (symbol? (first args))
                                 [(take 1 args) (rest args)]
                                 [[] args])]
    `(core/fn ~@fn-name ~@(replace-fn-tail fn-tail))))
(alter-meta! #'fn merge (select-keys (meta #'core/fn) [:arglists :doc :forms]))

(defmacro defn [& args]
  (core/let [{:keys [name fn-name docstring meta] :as parsed} (s/conform ::core-specs/defn-args args)
             prefix (remove nil? [(or name fn-name) docstring meta])
             fn-tail (drop (count prefix) args)]
    `(core/defn ~@prefix ~@(replace-fn-tail fn-tail))))
(alter-meta! #'defn merge (select-keys (meta #'core/defn) [:arglists :doc :forms]))

(defmacro defn- [& args]
  (core/let [{:keys [name fn-name docstring meta]} (s/conform ::core-specs/defn-args args)
             prefix (remove nil? [(or name fn-name) docstring meta])
             fn-tail (drop (count prefix) args)]
    `(core/defn- ~@prefix ~@(replace-fn-tail fn-tail))))
(alter-meta! #'defn- merge (select-keys (meta #'core/defn-) [:arglists :doc :forms]))

(defmacro let [bindings & body]
  `(core/let ~(destructure bindings) ~@body))

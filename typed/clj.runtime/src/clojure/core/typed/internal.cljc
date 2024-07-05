;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; note: this file is copied into resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime
;; via ./script/regen-kondo.sh
;; the canonical version is in the src folder
(ns ^:no-doc clojure.core.typed.internal
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  #?(:clj (:require [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

(defn take-when
  "When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optinal typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))

(defn parse-keyword-flat-map [forms]
  (loop [opts []
         forms forms]
    (cond 
      (keyword? (first forms))
      (let [[kv forms] (split-at 2 forms)]
        (assert (= 2 (count kv))
                (str "Missing keyword argument to: " (pr-str (first kv))))
        (recur (apply conj opts kv)
               forms))
      :else [opts forms])))

(defn parse-keyword-map [forms]
  (let [[flatopts forms] (parse-keyword-flat-map forms)]
    [(apply hash-map flatopts) forms]))

(defn visit-fn-tail
  [forms visitor]
  (let [[nme forms] (take-when symbol? forms)
        single-arity-syntax? (vector? (first forms))
        methods (cond-> forms
                  single-arity-syntax? list)
        visited-methods (doall
                          (map-indexed
                            (fn [method-pos arity]
                              (assert (seq arity))
                              (let [info {:method-pos method-pos}]
                                (-> (map-indexed
                                      (fn [i form]
                                        (case (int i)
                                          0 (let [_ (assert (vector? form))
                                                  has-rest? (= '& (nth form (- (count form) 3) nil))
                                                  info (assoc info
                                                              :nfixed (cond-> (count form)
                                                                        has-rest? (- 2))
                                                              :has-rest? has-rest?)
                                                  form (into (empty form) ;preserves meta
                                                             (map-indexed (fn [arg-pos maybe-destructuring]
                                                                            (if (= '& maybe-destructuring)
                                                                              maybe-destructuring
                                                                              (let [rest? (and has-rest? (= (inc arg-pos) (count form)))]
                                                                                (visitor (cond-> (assoc info
                                                                                                        :form maybe-destructuring
                                                                                                        :type (if rest? :rest :fixed))
                                                                                           (not rest?) (assoc :fixed-pos arg-pos)))))))
                                                             form)]
                                              (visitor (assoc info :form form :type :argv)))
                                          ;else
                                          form))
                                      arity)
                                    doall
                                    (with-meta (meta arity)))))
                            methods))]
    (concat
      (when nme
        [(visitor {:form nme
                   :type :name})])
      (cond-> visited-methods
        single-arity-syntax? first))))

(defn visit-fn-tail-destructuring
  [forms visitor]
  {:pre [(seq forms)]
   :post [(= % forms)]}
  (visit-fn-tail forms (fn [{:keys [type form]}]
                         {:pre [form]}
                         (case type
                           (:fixed :rest) (visitor form)
                           form))))

(defn visit-fn
  [[op & forms :as form] visitor]
  {:pre [(= "fn" (name op))]
   :post [(= % form)]}
  (-> (list* op (visit-fn-tail forms visitor))
      (with-meta (meta form))))

(defn visit-fn-destructuring
  "Call visitor on all destructuring forms in first arg, a clojure.core/fn form."
  [form visitor]
  {:pre [(= "fn" (name (first form)))]
   :post [(= % form)]}
  (visit-fn form (fn [{:keys [type form]}]
                   {:pre [form]}
                   (case type
                     (:fixed :rest) (visitor form)
                     form))))

(defn visit-defn-destructuring
  "Call visitor on all destructuring forms in first arg, a clojure.core/defn form."
  [[op & forms :as form] visitor]
  {:pre [(= "defn" (name op))]
   :post [(= % form)]}
  (let [[nme forms] (take-when symbol? forms)
        _ (assert (symbol? nme) (str "Missing name symbol in defn: " form))
        [doc forms] (take-when string? forms)
        [leading-map forms] (take-when map? forms)
        [trailing-map forms] (let [trailing-map (last forms)]
                               (if (map? trailing-map)
                                 [trailing-map (butlast forms)]
                                 [nil forms]))
        form (-> (list* op
                        nme
                        (concat
                          (some-> doc list)
                          (some-> leading-map list)
                          ;; a strict subset of a fn-tail
                          (visit-fn-tail-destructuring
                            forms
                            visitor)
                          (some-> trailing-map list)))
                 (with-meta (meta form)))]
    form))

(defn visit-defmethod-destructuring
  "Call visitor on all destructuring forms in first arg, a clojure.core/defmethod form."
  [[op nme dispatch & fn-tail :as form] visitor]
  {:pre [(= "defmethod" (name op))]
   :post [(= % form)]}
  (let [_ (assert (<= 4 (count form)) (str "Insufficient number of arguments to defmethod: " form))
        form (-> (list* op
                        nme
                        dispatch
                        (visit-fn-tail-destructuring
                          fn-tail
                          visitor))
                 (with-meta (meta form)))]
    form))

(defn add-fn-destructure-blame-form
  "Add destructuring blame forms to the provided clojure.core/fn form."
  ([fn-form] (add-fn-destructure-blame-form fn-form fn-form))
  ([fn-form blame-form]
   (-> fn-form
       (visit-fn-destructuring #((requiring-resolve 'clojure.core.typed.internal.add-destructure-blame-form/add-destructure-blame-form) % blame-form)))))

(defn add-defn-destructure-blame-form
  "Add destructuring blame forms to the provided clojure.core/defn form."
  ([defn-form] (add-defn-destructure-blame-form defn-form defn-form))
  ([defn-form blame-form]
   (-> defn-form
       (visit-defn-destructuring #((requiring-resolve 'clojure.core.typed.internal.add-destructure-blame-form/add-destructure-blame-form) % blame-form)))))

(defn add-defmethod-destructure-blame-form
  "Add destructuring blame forms to the provided clojure.core/defmethod form."
  ([defmethod-form] (add-defmethod-destructure-blame-form defmethod-form defmethod-form))
  ([defmethod-form blame-form]
   (-> defmethod-form
       (visit-defmethod-destructuring #((requiring-resolve 'clojure.core.typed.internal.add-destructure-blame-form/add-destructure-blame-form) % blame-form)))))

(defn- reassembled-fn-type [{:keys [parsed-methods name poly ann]}]
  (let [reassembled-fn-type `(typed.clojure/IFn
                               ~@(map (fn [{:keys [rest drest dom rng] :as method-ann}]
                                        {:pre [(map? method-ann)]
                                         :post [(vector? %)]}
                                        (vec
                                          (concat
                                            (map :type dom)
                                            (cond
                                              rest [(:type rest) :*]
                                              drest [(-> drest :pretype :type) :.. (:bound drest)])
                                            [:-> (:type rng)])))
                                      (map :ann parsed-methods)))
        reassembled-fn-type (if-let [forall poly]
                              `(typed.clojure/All ~forall ~reassembled-fn-type)
                              reassembled-fn-type)]
    reassembled-fn-type))

(defn parse-fn*
  "(fn name? [[param :- type]* & [param :- type *]?] :- type? exprs*)
  (fn name? ([[param :- type]* & [param :- type *]?] :- type? exprs*)+)"
  [[_fn_ & forms :as form]]
  {:pre [(symbol? _fn_)
         #_(= "fn" (name _fn_))]}
  (let [[{poly :forall :as opts} forms] (parse-keyword-map forms)
        [name forms] (take-when symbol? forms)
        _ (assert (not (keyword? (first forms))))
        single-arity-syntax? (vector? (first forms))
        methods (cond-> forms
                  single-arity-syntax? list)
        parsed-methods (for [method methods]
                         (merge-with merge
                                     (let [ann-params (first method)]
                                       (assert (vector? ann-params))
                                       {:ann-params ann-params
                                        :original-method (vary-meta method #(merge (meta form)
                                                                                   (meta ann-params)
                                                                                   %))})
                                     (loop [ann-params (first method)
                                            pvec (empty (first method)) ; an empty param vector with same metadata
                                            ann-info []]
                                       (cond
                                         (empty? ann-params)
                                         (let [[dom [amp rst]] (split-with #(not= '& %) ann-info)]
                                           {:pvec pvec
                                            :ann (-> (select-keys rst [:rest :drest])
                                                     (assoc :dom dom))})

                                         ;rest param
                                         (= '& (first ann-params))
                                         (let [[amp & ann-params] ann-params]
                                           (if (= :- (second ann-params))
                                             (let [[p colon & rst-params] ann-params]
                                               (cond
                                                 (#{:* '*} (second rst-params))
                                                 (let [[t star & after-rst] rst-params]
                                                   (recur after-rst
                                                          (conj pvec amp p)
                                                          (conj ann-info amp {:rest {:type t}})))

                                                 (#{:.. '...} (second rst-params))
                                                 (let [[pretype dots bound & after-rst] rst-params]
                                                   (when (= '... (second rst-params))
                                                     (println (str "WARNING: ... syntax has changed to :.. in t/fn: " (pr-str form))))
                                                   (recur after-rst
                                                          (conj pvec amp p)
                                                          (conj ann-info amp {:drest {:pretype {:type pretype}
                                                                                      :bound bound}})))

                                                 :else
                                                 (throw (ex-info "Rest annotation must be followed with :* or :.." {:form method}))))
                                             (let [[p & after-rst] ann-params]
                                               (recur after-rst
                                                      (conj pvec amp p)
                                                      (conj ann-info amp {:rest {:type 'clojure.core.typed/Any
                                                                                 :default true}})))))

                                         ;fixed param
                                         :else
                                         (if (= :- (second ann-params))
                                           (let [[p colon t & rest-params] ann-params]
                                             (recur rest-params
                                                    (conj pvec p)
                                                    (conj ann-info {:type t})))
                                           (let [[p & rest-params] ann-params]
                                             (recur rest-params
                                                    (conj pvec p)
                                                    (conj ann-info {:type 'clojure.core.typed/Any
                                                                    :default true}))))))
                                     (if (and (= :- (second method))
                                              (<= 3 (count method)))
                                       (let [[param colon t & body] method]
                                         {:body body
                                          :ann {:rng {:type t}}})
                                       (let [[param & body] method]
                                         {:body body
                                          :ann {:rng {:type 'clojure.core.typed/Any
                                                      :default true}}}))))
        final-ann (mapv :ann parsed-methods)
        #_#_
        _ (assert ((con/vec-c?
                     (con/hmap-c?
                       :dom (con/every-c? (con/hmap-c? :type (constantly true)))
                       (con/optional :rest) (con/hmap-c? :type (constantly true))
                       :rng (some-fn (con/hmap-c? :default #{true})
                                     (con/hmap-c? :type (constantly true)))))
                   final-ann)
                  final-ann)
        res {:fn (-> `(fn ~@(concat
                              (some-> name list)
                              (for [{:keys [body pvec]} parsed-methods]
                                (apply list pvec body))))
                     (add-fn-destructure-blame-form form))
             :ann final-ann
             :poly poly
             :parsed-methods parsed-methods
             :name name
             :single-arity-syntax? single-arity-syntax?}]
    (assoc res :reassembled-fn-type (reassembled-fn-type res))))

(defn parse-defn* [args]
  (let [[flatopt args] (parse-keyword-flat-map args)
        [nme & args] args
        _ (assert (symbol? nme) "defn name should be a symbol")
        [docstring args] (take-when string? args)
        [attr-map args] (take-when map? args)]
    {:name (vary-meta nme merge
                      {:arglists
                       (list 'quote
                             (if (vector? (first args)) ; arity = 1
                               (list (first args))
                               (map first args)))}
                      (when docstring {:doc docstring})
                      attr-map)
     :args (concat flatopt args)}))

(defn parse-loop*
  [forms]
  (let [parsed-loop (assoc
                      (loop [ann-params (first forms)
                             pvec []
                             ann-info []]
                        (cond
                          (empty? ann-params)
                          {:pvec pvec
                           :ann {:params ann-info}}
                          
                          :else
                          (if (= :- (second ann-params))
                            (let [[p colon t init & rest-params] ann-params]
                              (recur rest-params
                                     (conj pvec p init)
                                     (conj ann-info {:type t})))
                            (let [[p init & rest-params] ann-params]
                              (recur rest-params
                                     (conj pvec p init)
                                     (conj ann-info {:type 'clojure.core.typed/Any
                                                     :default true}))))))
                      :body (next forms))]
    {:loop `(clojure.core/loop ~(:pvec parsed-loop) ~@(:body parsed-loop))
     :ann (:ann parsed-loop)}))

(defn binder-names [binder]
  {:post [(every? symbol? %)]}
  (keep (fn [v]
          (if (vector? v)
            (first v)
            (when-not (#{:* :.. '* '...} v)
              v)))
        binder))

(defn gen-ann-protocol [{:keys [name methods binder] :as dp-ann}]
  (let [tvars (set (binder-names binder))
        this-type (if binder
                    `(~name ~@(binder-names binder))
                    name)]
  `(clojure.core.typed/ann-protocol 
     ~@(some-> binder list)
     ~name
     ~@(mapcat (fn [{:keys [name arities poly]}]
                 (let [localtvars (set (binder-names poly))
                       _ (assert (empty? ((requiring-resolve 'clojure.set/intersection)
                                          localtvars
                                          tvars))
                                 "Shadowing a protocol type variable in a method is disallowed")
                       fn-type `(clojure.core.typed/IFn
                                  ~@(map (fn [{:keys [ptypes ret]}]
                                           (let [[provided-this & argts] ptypes
                                                 ; if programmer provides the default 'this' type, use that,
                                                 ; otherwise use the current protocol.
                                                 actual-this (if (:default provided-this)
                                                               this-type
                                                               (:type provided-this))]
                                             `[~actual-this ~@(map :type argts) :-> ~(:type ret)]))
                                         arities))]
                   [name (if poly
                           `(clojure.core.typed/All ~poly ~fn-type)
                           fn-type)]))
               methods))))

(defn parse-defprotocol*
  [forms]
  (let [[binder forms] (take-when vector? forms)
        [pname & typed-decl-methods] forms
        [pdoc typed-decl-methods] (take-when string? typed-decl-methods)
        [opts typed-decl-methods] (let [all-pairs (->> typed-decl-methods
                                                       (partition-all 2))
                                        opts (apply concat
                                                    (take-while (every-pred #(= 2 (count %))
                                                                            (comp keyword? first))
                                                                all-pairs))]
                                    [opts (drop (count opts) typed-decl-methods)])
        parse-pvec (fn [pvec] ; parse parameter vectors
                     {:pre [(vector? pvec)]
                      :post [(((requiring-resolve 'clojure.core.typed.contract-utils/hmap-c?)
                               :actual vector?
                               :ptypes vector?)
                              %)]}
                     (loop [pvec pvec
                            actual (empty pvec) ; empty vector with same metadata as pvec
                            ptypes []]
                       (assert (every? vector? [actual ptypes]))
                       (cond
                         (empty? pvec) {:ptypes ptypes :actual actual}
                         :else (if (= :- (second pvec))
                                 (let [_ (assert (= 3 (count (take 3 pvec)))
                                                 "Missing type annotation after :-")
                                       [b colon t & rst] pvec]
                                   (recur rst 
                                          (conj actual b)
                                          (conj ptypes {:type t})))
                                 (let [_ (assert (seq pvec))
                                       [b & rst] pvec]
                                   (recur rst 
                                          (conj actual b)
                                          (conj ptypes {:type 'clojure.core.typed/Any
                                                        :default true})))))))
        actual-decl-methods (for [m typed-decl-methods]
                              (let [[poly rst] (take-when vector? m)
                                    [name & dvecs] rst]
                                (assert (symbol? name) (str "defprotocol method name must be a symbol: " pname))
                                (loop [dvecs dvecs
                                       arities []]
                                  (cond 
                                    (or (empty? dvecs)
                                        (string? (first dvecs)))
                                    (merge {:poly poly
                                            :name name
                                            :arities arities}
                                           (when (string? (first dvecs))
                                             {:doc (first dvecs)}))

                                    :else (if (= :- (second dvecs))
                                            (let [_ (assert (= 3 (count (take 3 dvecs)))
                                                            "Missing type annotation after :-")
                                                  [v colon t & rst] dvecs
                                                  {:keys [ptypes actual]} (parse-pvec v)]
                                              (recur rst
                                                     (conj arities {:ret {:type t}
                                                                    :ptypes ptypes
                                                                    :actual actual})))
                                            (let [_ (assert (seq dvecs))
                                                  [v & rst] dvecs
                                                  {:keys [ptypes actual]} (parse-pvec v)]
                                              (recur rst
                                                     (conj arities {:ret {:type 'clojure.core.typed/Any
                                                                          :default true}
                                                                    :ptypes ptypes
                                                                    :actual actual}))))))))
        ann {:binder binder
             :name pname
             :methods (map #(dissoc % :doc) actual-decl-methods)}]
    {:defprotocol `(clojure.core/defprotocol 
                     ~pname 
                     ~@(when pdoc [pdoc])
                     ~@opts
                     ~@(map (fn [{:keys [name arities doc]}] 
                              `(~name ~@(concat ; prefer left-most arities if grouped duplicates
                                                (reduce
                                                  (fn [ret current]
                                                    (if (= (count current) (count (last ret)))
                                                      ret
                                                      (conj ret current)))
                                                  []
                                                  (map :actual arities))
                                                (when doc
                                                  [doc]))))
                            actual-decl-methods))
     :ann-protocol (gen-ann-protocol ann)}))

(defn parse-let*
  [[bvec & forms]]
  (let [actual-bvec (loop [bvec bvec
                           actual-bvec (empty bvec)] ; empty vector with same metadata as bvec
                      (assert (vector? actual-bvec))
                      (cond
                        (empty? bvec) actual-bvec
                        :else (if (= :- (second bvec))
                                (let [_ (assert (= 4 (count (take 4 bvec)))
                                                "Incorrect forms following :-")
                                      [v colon t init & rst] bvec]
                                  (recur rst
                                         ;;FIXME use platform agnostic macro
                                         (conj actual-bvec v `(clojure.core.typed/ann-form ~init ~t))))
                                (let [_ (assert (= 2 (count (take 2 bvec)))
                                                "No init found for local binding")
                                      [v init & rst] bvec]
                                  (recur rst
                                         (conj actual-bvec v init))))))]
    {:let `(clojure.core/let ~actual-bvec ~@forms)}))

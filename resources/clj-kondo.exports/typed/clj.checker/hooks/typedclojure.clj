(ns hooks.typed
  (:require [clj-kondo.hooks-api :as api]))

(defn mask-index
  "Helper function, this creates a mask sequence that indicates which symbols in
  an arg vector are type-related (`:-` or following `:-`)

  [a b :- c d e :- f :-] => [false false true true false false true true true]"
  [coll]
  (let [basic-mask
        (map #(and (api/keyword-node? %)
                   (-> % :k (= :-)))
             coll)

        offset-mask
        (->> basic-mask (into [false]) butlast)]
    (map #(or %1 %2) basic-mask offset-mask)))

(defn typed-defn
  [{:keys [node]}]
  (let [{:keys [children]}
        node

        ;; Thing is the `:-`. Our strategy is to strip types then emit `defn`
        [_fname name args thing _rtype & body]
        children

        ;; args with no typing included
        processed-args
        (->> (map #(and %1 %2)
                  (->> args :children mask-index (map not))
                  (-> args :children))
             (filter identity))

        new-node
        (api/list-node
         (list*
          (api/token-node 'defn)
          (-> name :value)
          (api/vector-node processed-args)
          body))]
    (when-not
      (= (:k thing) :-) ;; :-)
      (api/reg-finding! (assoc (meta thing)
                               :message "Expected first token to be :-"
                               :type :typed.clojure/unannotated)))

    #_(prn (api/sexpr new-node))

    {:node new-node}))

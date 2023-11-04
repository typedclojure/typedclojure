(ns hooks.typed.clj.runtime
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

        ;; Our strategy is to strip types then emit `defn`
        ;; `sugar` is the `:-`.
        ;; `args` has been constructed as a list of tokens, not a token itself
        [_fname name {args :children :as _args-node} sugar _rtype & body]
        children

        ;; args with no typing included
        processed-args
        (->> (map #(and %1 %2)
                  (->> args mask-index (map not))
                  args)
             (filter identity))

        new-node
        (api/list-node
         (list*
          (api/token-node 'clojure.core/defn)
          name
          (api/vector-node processed-args)
          body))]
    (when-not
      (= (:k sugar) :-) ;; :-)
      (api/reg-finding! (assoc (meta sugar)
                               :message "Expected first token to be `:-`"
                               :type :typed.clojure/unannotated)))

    #_(prn (api/sexpr new-node))

    {:node new-node}))

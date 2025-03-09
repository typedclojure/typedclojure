;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc
  typed.cljc.checker.name-utils
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.fold-rep :as fold])
  (:import [typed.cljc.checker.type_rep Name]))

(fold/def-derived-fold IRecursiveNames ^:private erase-names* [should-erase name->f used-f erased-frequencies])

(defn erase-names
  ([t should-erase opts]
   {:post [(map? (second %))
           (every? symbol? (keys (second %)))
           (every? nat-int? (vals (second %)))]}
   (let [erased-frequencies (atom {})
         used-f (atom {})
         erased (erase-names t should-erase {} used-f erased-frequencies opts)
         erased-frequencies @erased-frequencies]
     [(if (empty? erased-frequencies) t erased)
      erased-frequencies]))
  ([t should-erase name->f used-f erased-frequencies opts]
   (let [t (loop [t t
                  seen #{}]
             (assert (not (seen t)))
             (let [rt (cond-> t
                        (r/TApp? t) (c/-resolve opts))]
               (if (= rt t)
                 t
                 (recur rt (conj seen t)))))
         t (letfn [(rec
                     ([ty] (rec ty opts))
                     ([ty opts] (erase-names ty should-erase name->f used-f erased-frequencies opts)))]
             (call-erase-names*
               t opts
               {:type-rec rec
                :should-erase should-erase
                :name->f name->f
                :used-f used-f
                :erased-frequencies erased-frequencies}))]
     t)))

(fold/add-fold-case
  IRecursiveNames erase-names*
  Name
  (fn [{:keys [id] :as t} should-erase name->f used-f erased-frequencies]
    (or (when (should-erase id)
          (swap! erased-frequencies update id (fnil inc 0))
          r/-nothing)
        ;; refer back to Mu
        (when-some [f (name->f id)]
          ; (swap! erased-frequencies update id (fnil inc 0))
          (swap! used-f assoc f true)
          f)
        ;; create a Mu that is like t except erases all instances of Name.
        (let [f (r/make-F (gensym (str "erase-names*__" (name id))))
              opts (free-ops/with-bounded-frees opts {f r/no-bounds})
              body (erase-names
                     (c/-resolve t opts)
                     should-erase
                     (assoc name->f id f)
                     used-f
                     erased-frequencies
                     opts)]
          (if (@used-f f)
            (c/Mu* (:name f) body opts)
            body)))))

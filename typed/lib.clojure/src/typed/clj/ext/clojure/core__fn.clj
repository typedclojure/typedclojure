;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.ext.clojure.core__fn
  "Typing rules for clojure.core/fn"
  (:refer-clojure :exclude [type])
  (:require [clojure.core.typed.internal :as internal]
            [clojure.core.typed.internal.add-destructure-blame-form :refer [add-destructure-blame-form]]
            [typed.cljc.checker.type-rep :as r]
            [clojure.set :as set]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.errors :as err]
            [typed.clojure :as t]
            [typed.cljc.checker.check :as check]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check-below :as below]))

(defn quoted? [v]
  (and (seq? v)
       (= 2 (count v))
       (= 'quote (first v))))

(defn parse-meta-ann [quoted-tsyn opts]
  #_
  (let [_ (when-not (and (quoted? quoted-tsyn)
                         (quoted? (second quoted-tsyn)))
            (prs/with-tsyn-env quoted-tsyn
              (prs/prs-error (str ":typed.clojure/- annotations must be double quoted, convert to ''" (pr-str quoted-tsyn)))))]
    (prs/parse-type (-> quoted-tsyn second second)))
  (prs/parse-type quoted-tsyn opts))

(defn metas->maybe-expected-type [metas opts]
  {:post [((some-fn nil? r/Type?) %)]}
  (when metas
    (let [{[nme :as nme?] :name :as all-meta-groups} (group-by :type metas)
          _ (assert (empty?
                      (set/difference (set (keys all-meta-groups))
                                      #{:fixed :rest :argv :name})))
          _ (assert (<= 0 (count nme?) 1))
          all-ks #{::t/- ::t/* ::t/kv ::t/forall}]
      (cond
        ;; annotation for entire function on name
        (-> nme :form meta (find ::t/-))
        (let [_ (when (or (some :annotated (apply concat (-> all-meta-groups (dissoc :name) vals)))
                          (seq (-> nme :form meta (select-keys all-ks) keys set (disj ::t/-))))
                  (err/int-error "Cannot mix other metadata annotations after placing :typed.clojure/- on fn name." opts))]
          (parse-meta-ann (-> nme :form meta ::t/-) opts))

        :else (let [[_ binder :as poly?] (-> nme :form meta (find ::t/forall))
                    _ (assert (not poly?) "TODO ::t/forall")
                    t (apply r/make-FnIntersection
                             (map (fn [method-metas]
                                    (let [{[argv :as all-argv] :argv [rest :as rest?] :rest :keys [fixed] :as method-metas-groups} (group-by :type method-metas)
                                          _ (assert (= 1 (count all-argv)))
                                          _ (assert (<= 0 (count rest?) 1))
                                          _ (assert (empty?
                                                      (set/difference (set (keys method-metas-groups))
                                                                      #{:fixed :rest :argv})))
                                          _ (assert (not rest?) "TODO rest argument via metadata annotation")
                                          prs-dash-meta (fn [desc]
                                                          (if-some [[_ quoted-tsyn] (-> desc :form meta (find ::t/-))]
                                                            (parse-meta-ann quoted-tsyn opts)
                                                            r/-wild))
                                          dom (->> fixed
                                                   (sort-by :fixed-pos)
                                                   (mapv prs-dash-meta))
                                          rett (prs-dash-meta argv)]
                                      (r/make-Function
                                        dom
                                        rett)))
                                  (vals (group-by :method-pos (apply concat (-> all-meta-groups (dissoc :name) vals))))))]
                t)))))

(defn prep-tc-meta [form]
  (let [atm (atom #{})
        blame-form form
        record! (fn [desc ks]
                  (swap! atm conj
                         (cond-> desc
                           (when-some [mta (not-empty (meta (:form desc)))]
                             (some #(find mta %) ks))
                           (assoc :annotated true))))
        rm-ks (fn [form ks] (vary-meta form #(apply dissoc % ks)))
        form (internal/visit-fn
               form
               (fn [{:keys [form] :as desc}]
                 (case (:type desc)
                   :fixed (let [ks #{::t/-}]
                            (record! desc ks)
                            (-> form
                                (add-destructure-blame-form blame-form)
                                (rm-ks ks)))
                   :rest (let [ks #{::t/* ::t/... ::t/kv}]
                           (record! desc ks)
                           (-> form
                               (add-destructure-blame-form blame-form)
                               (rm-ks ks)))
                   :argv (let [ks #{::t/-}]
                           (record! desc ks)
                           (-> form
                               (rm-ks ks)))
                   :name (let [ks #{::t/- ::t/forall}]
                           (record! desc ks)
                           (-> form
                               (rm-ks ks))))))
        metas @atm]
    [form (when (some :annotated metas)
            metas)]))


(defuspecial defuspecial__fn
  "defuspecial implementation for clojure.core/fn"
  [expr expected {::check/keys [check-expr] :as opts}]
  (let [[form metas] (prep-tc-meta (:form expr))
        annotated-t (metas->maybe-expected-type metas opts)
        expr (-> expr
                 (assoc :form form)
                 ana2/analyze-outer)]
    (if annotated-t
      (-> expr
          (check-expr (r/ret annotated-t))
          (update u/expr-type below/maybe-check-below expected opts))
      (check-expr expr expected))))

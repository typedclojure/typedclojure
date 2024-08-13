;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.jvm.annotate-host-info
(ns typed.clj.analyzer.passes.annotate-host-info
  (:require [typed.cljc.analyzer :as ana]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.passes.cleanup :as cleanup]
            [typed.cljc.analyzer.passes.elide-meta :as elide-meta]
            [typed.cljc.analyzer.utils :as cu]
            [typed.clj.analyzer.utils :as ju]))

#?(
:cljr

;;; Added this to deal with explicit interface implementation.
(defn explicit-implementation-name-matches 
  [impl-method-name interface-method-name]
  (let [member-name (str impl-method-name )
        i (.LastIndexOf member-name ".")] 
    (and (pos? i) (= (subs member-name (inc i)) (str interface-method-name)))))
)

(defn annotate-host-info
  "Adds a :methods key to reify/deftype :methods info representing
   the reflected informations for the required methods, replaces
   (catch :default ..) forms with (catch Throwable ..)"
  {:pass-info {:walk :pre :depends #{} :after #{#'elide-meta/elide-meta}}}
  [ast opts]
  (case (:op ast)
    (:reify :deftype)
    (let [methods (:methods ast)
          interfaces (:interfaces ast)
          class (:class ast)
          env (:env ast)
          all-methods
          (into #{}
                (mapcat (fn [class]
                          (mapv (fn [method]
                                  (dissoc method :exception-types))
                                (filter (fn [{:keys [flags return-type]}]
                                          (and return-type (not-any? #{:final :static} flags)))
                                        (ju/members class))))
                        (conj interfaces Object)))]
      (assoc ast :methods (mapv (fn [ast]
                                  (let [name (:name ast)
                                        argc (count (:params ast))]
                                    (assoc ast :methods
                                           (if (empty? all-methods)
                                             ()
                                             (let [nm? (ju/name-matches? name)]
                                               (filter #(and (= argc (count (:parameter-types %)))
                                                             (or (nm? (:name %))
                                                                 #?(:cljr (explicit-implementation-name-matches name (:name %)))))
                                                       all-methods))))))
                                methods)))


    :catch
    (let [class (:class ast)
          env (:env ast)
          the-class (cond

                     (and (= :const (:op class))
                          (= :default (:form class)))
                     #?(:cljr Exception :default Throwable)

                     (= :maybe-class (:op class))
                     (ju/maybe-class-literal (:class class)))

          ast (if the-class
                (-> ast
                  (assoc :class (assoc (ana/analyze-const the-class env :class opts)
                                  :form  (:form class)
                                  :tag   #?(:cljr Type :default Class)
                                  :o-tag #?(:cljr Type :default Class))))
                ast)]
      (assoc-in ast [:local :tag]  (-> ast :class :val)))


    :method
    ;; this should actually be in validate but it's here since it needs to be prewalked
    ;; for infer-tag purposes
    (let [methods (:methods ast)
          interfaces (:interfaces ast)
          class (:class ast)
          env (:env ast)
          name (:name ast)
          tag (:tag ast)
          form (:form ast)
          params (:params ast)]
      (if interfaces
        (let [tags (mapv (comp ju/maybe-class :tag meta :form) params)
              methods-set (into #{} (map (fn [x] (dissoc x :declaring-class :flags))) methods)]
          (let [[m & rest :as matches] (ju/try-best-match tags methods)]
            (if m
              (let [ret-tag  (ju/maybe-class (:return-type m))
                    i-tag    (ju/maybe-class (:declaring-class m))
                    arg-tags (mapv ju/maybe-class (:parameter-types m))
                    params   (mapv (fn [{:keys [atom] :as arg} tag]
                                     (assoc arg :tag tag :o-tag tag)) params arg-tags)]
                (if (or (empty? rest)
                        (every? (fn [{:keys [return-type parameter-types]}]
                             (and (= (ju/maybe-class return-type) ret-tag)
                                  (= arg-tags (mapv ju/maybe-class parameter-types)))) rest))
                  (assoc (dissoc ast :interfaces :methods)
                    :bridges   (filter #(and (= arg-tags (mapv ju/maybe-class (:parameter-types %)))
                                             (#?(:cljr .IsAssignableFrom :default .isAssignableFrom) (ju/maybe-class (:return-type %)) ret-tag))
                                       (disj methods-set (dissoc m :declaring-class :flags)))
                    :methods   methods
                    :interface i-tag
                    :tag       ret-tag
                    :o-tag     ret-tag
                    :params    params)
                  (throw (ex-info (str "Ambiguous method signature for method: " name)
                                  (merge {:method     name
                                          :interfaces interfaces
                                          :form       form
                                          :params     (mapv (fn [x] (ast/prewalk x cleanup/cleanup)) params)
                                          :matches    matches}
                                         (cu/source-info env))))))
              (throw (ex-info (str "No such method found: " name " with given signature in any of the"
                                   " provided interfaces: " interfaces)
                              (merge {:method     name
                                      :methods    methods
                                      :interfaces interfaces
                                      :form       form
                                      :params     params}
                                     (cu/source-info env)))))))
        ast))
    ast))

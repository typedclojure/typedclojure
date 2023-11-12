;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;TODO reify is better implemented, share with typed.clj.checker.check.reify
(ns typed.clj.checker.check.deftype
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.fn-methods :as fn-methods]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.datatype-env :as dt-env]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defn match-method->expected-ifn [t inst-method]
  {:pre [((some-fn nil? r/Type?) t)]
   :post [((some-fn nil? r/Type?) %)]}
  (some->> (some #(when (and (= (count (:parameter-types inst-method))
                                ; minus the target arg
                                (count (:required-params %)))
                             (= (munge (:name inst-method))
                                (:name %)))
                    %)
                 (:methods inst-method))
           (cu/type->method-expected t)))

(defn check-method [{:keys [env] :as inst-method} {:keys [kind expected-type nme expected] :as _opts}]
  ;; returns a vector of checked methods
  {:pre [(#{:reify :deftype} kind)
         (= :method (:op inst-method))]
   :post [(= :method (:op %))]}
  (when vs/*trace-checker*
    (println "Checking" (name kind) "method:" (:name inst-method)))
  (binding [vs/*current-env* env]
    (let [method-nme (:name inst-method)
          _ (assert (symbol? method-nme))
          ;_ (prn "method-nme" method-nme)
          ;_ (prn "inst-method" inst-method)
          _ (assert (:this inst-method))
          _ (assert (:params inst-method))]
      (if-some [expected-ifn (match-method->expected-ifn expected-type inst-method)]
        (first
          (:methods
            (fn-methods/check-fn-methods
              [inst-method]
              expected-ifn
              {:check-rest-fn (fn [& args] (err/int-error (format "%s method cannot have rest parameter" (name kind))))
               :recur-target-fn
               (fn [{:keys [dom] :as f}]
                 {:pre [(r/Function? f)]
                  :post [(recur-u/RecurTarget? %)]}
                 (recur-u/RecurTarget-maker (rest dom) nil nil nil))
               :validate-expected-fn
               (fn [fin]
                 {:pre [(r/FnIntersection? fin)]}
                 (when (not-every? #(= :fixed (:kind %)) (:types fin))
                   (err/int-error
                     (str "Cannot provide rest arguments to " (name kind) " method: "
                          (prs/unparse-type fin)))))})))
        (err/tc-delayed-error (str "Internal error checking " (name kind) (when nme " " nme) " method: " method-nme)
                              :return inst-method)))))

(defn check-deftype
  [{:keys [fields methods env] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  ;TODO check fields match, handle extra fields in records
  ;TODO check that all protocols are accounted for
  (binding [vs/*current-env* env]
    (let [compiled-class (:class-name expr)
          ;; jana2/validate turns :class-name into a class.
          ;; might not be run at this point.
          compiled-class (cond-> compiled-class
                           (symbol? compiled-class) coerce/symbol->Class)
          _ (assert (class? compiled-class) (class compiled-class))
          nme (coerce/Class->symbol compiled-class)
          field-syms (map :name fields)
          _ (assert (every? symbol? field-syms))
          ; unannotated datatypes are handled below
          dtp (dt-env/get-datatype nme)
          [nms bbnds dt] (if (r/TypeFn? dtp)
                           (let [nms (c/TypeFn-fresh-symbols* dtp)]
                             [nms (c/TypeFn-bbnds* nms dtp) (c/TypeFn-body* nms dtp)])
                           [nil nil dtp])
          expected-fields (some-> dt c/DataType-fields*)
          expected-field-syms (vec (keys expected-fields))
          ret-expr (assoc expr
                          u/expr-type (below/maybe-check-below
                                        (r/ret (c/RClass-of Class))
                                        expected))]
      (cond
        (not dtp)
        (err/tc-delayed-error (str "deftype " nme " must have corresponding annotation. "
                                   "See ann-datatype and ann-record")
                              :return ret-expr)

        (not ((some-fn r/DataType? r/Record?) dt))
        (err/tc-delayed-error (str "deftype " nme " cannot be checked against: " (prs/unparse-type dt))
                              :return ret-expr)

        (if (r/Record? dt)
          (c/isa-DataType? compiled-class)
          (c/isa-Record? compiled-class))
        (let [datatype? (c/isa-DataType? compiled-class)]
          (err/tc-delayed-error (str (if datatype? "Datatype " "Record ") nme 
                                     " is annotated as a " (if datatype? "record" "datatype") 
                                     ", should be a " (if datatype? "datatype" "record") ". "
                                     "See ann-datatype and ann-record")
                                :return ret-expr))

        (not= expected-field-syms 
              ; remove implicit __meta and __extmap fields
              (if (c/isa-Record? compiled-class)
                (remove cu/record-hidden-fields field-syms)
                field-syms))
        (err/tc-delayed-error (str (if (c/isa-Record? compiled-class) "Record " "Datatype ")
                                   nme " fields do not match annotation. "
                                   " Expected: " (vec expected-field-syms)
                                   ", Actual: " (vec field-syms))
                              :return ret-expr)

        :else
        (let [check-method? (fn [inst-method]
                              (not (and (r/Record? dt)
                                        (cu/record-implicits (symbol (:name inst-method))))))]
          (update ret-expr
                  :methods (fn [methods]
                             (lex/with-locals expected-fields
                               (free-ops/with-free-mappings 
                                 (into {}
                                       (map (fn [nm bnd]
                                              [(-> nm r/make-F r/F-original-name)
                                               {:F (r/make-F nm) :bnds bnd}])
                                            nms bbnds))
                                 (into []
                                       (map #(cond-> %
                                               (check-method? %) (check-method {:kind :deftype
                                                                                :expected-type dt
                                                                                :nme nme})))
                                       methods))))))))))

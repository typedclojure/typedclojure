;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.cli
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-rep :as r]
            [clojure.string :as str]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.constant-type :as const]
            [typed.cljc.checker.type-ctors :as c]))

(defn parse-fn-return-type [parse-fn-type opts]
  (let [subst-in (free-ops/with-free-symbols 
                   #{'a} (prs/parse-type '[String -> a] opts))] 
    (-> (cgen/cs-gen #{} {'a r/no-bounds} {} parse-fn-type subst-in) 
        (cgen/subst-gen #{} subst-in {} opts)
        (get-in ['a :type]))))

(defn vector-args [expr]
  (case (:op expr)
    :constant (when (vector? (:val expr))
                (map (fn [f] [f nil]) (:val expr)))
    :vector (doall
              (map (fn [arg-expr]
                     [(ast-u/emit-form-fn arg-expr) arg-expr])
                   (:args expr)))
    nil))

; some code taken from tools.cli
; (All [x]
;   [CliSpec -> (U nil '[Value Type])])
(defn parse-cli-spec [check-fn spec-expr opts]
  (letfn [(opt? [^String x]
            (.startsWith x "-"))
          (name-for [k]
            (str/replace k #"^--no-|^--\[no-\]|^--|^-" "")) 
          (flag? [^String x]
              (.startsWith x "--[no-]"))]

  (let [; (U nil (Seqable '[Form (U nil Expr)]))
        raw-spec (vector-args spec-expr)]
    (cond
      (not raw-spec) (do
                       ;(prn "cli: not vector " spec-expr)
                       nil)
      :else
      (let [; each seq and map entry is a pair of [form expr]
            [switches raw-spec] (split-with (fn [[frm _]] (and (string? frm) (opt? frm))) raw-spec)
            [docs raw-spec]     (split-with (fn [[frm _]] (string? frm)) raw-spec)
            ; keys are [kw expr]
            options             (apply hash-map raw-spec)
            ; keys are keywords
            ; (Map Keyword [Form Expr])
            options             (into {}
                                      (for [[[kfrm _] v] options]
                                        [kfrm v]))
            ; (Seqable Form)
            aliases             (map (fn [[frm _]] (name-for frm)) switches)
            ; assume we fail later if there is anything ambiguous
            flag                (or (if (seq switches)
                                      (flag? (first (last switches)))
                                      :unknown)
                                    (when (contains? options :flag)
                                      (let [flg-form (first (:flag options))]
                                        (if (boolean? flg-form)
                                          flg-form
                                          :unknown)))
                                    false)]
        (cond
          ;not accurate enough, return nil
          (not-every? keyword? (keys options)) (do
                                                 ;(prn "cli: not every option key was keyword" options)
                                                 nil)
          (#{:unknown} flag) (do 
                               ;(prn "cli: flag unknown")
                               nil)
          (not
            (and (#{0 1} (count docs))
                 ((some-fn nil? string?) (-> docs first first)))) (do
                                                                    ;(prn "cli: docs" docs) 
                                                                    nil)
          (empty? aliases) (do
                             ;(prn "cli: empty aliases")
                             nil)
          :else
          (let [name (r/-val (keyword (last aliases)))
                default-type (when-let [[frm default-expr] (:default options)]
                               (if default-expr
                                 (-> (check-fn default-expr)
                                     u/expr-type
                                     r/ret-t)
                                 (const/constant-type frm false opts)))
                parse-fn-type (when-let [[pfrm parse-fn-expr] (:parse-fn options)]
                                (if parse-fn-expr
                                  (-> (check-fn parse-fn-expr (r/ret (prs/parse-type
                                                                       '[String -> Any]
                                                                       opts)))
                                      u/expr-type
                                      r/ret-t)
                                  (const/constant-type pfrm false opts)))
                parse-fn-ret (when parse-fn-type
                               (parse-fn-return-type parse-fn-type opts))
                type (cond
                       (and parse-fn-type
                            (not parse-fn-ret)) (do
                                                  ;(prn "cli: parse-fn")
                                                  nil)
                       flag (c/RClass-of Boolean opts)
                       :else
                       (c/Un (concat (when default-type
                                       [default-type])
                                     (if parse-fn-type
                                       [parse-fn-ret]
                                       [(c/RClass-of String opts)]))
                             opts))]
            (when type
              [name type]))))))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.fn-method-utils
  (:require [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.abo :as abo]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  (:import (typed.cljc.checker.type_rep DottedPretype KwArgs TCResult)))

;lam-result in TR
(u/def-type FnResult [args :- (t/SequentialColl '[t/Sym r/Type])
                      kws :- (t/Nilable '[t/Sym KwArgs])
                      rest :- (t/Nilable '[t/Sym r/Type])
                      drest :- (t/Nilable '[t/Sym r/Type])
                      prest :- (t/Nilable '[t/Sym DottedPretype])
                      pdot :- (t/Nilable '[t/Sym DottedPretype])
                      body :- TCResult]
  "Results of checking a fn method"
  [(every? symbol? (map first args))
   (every? r/Type? (map second args))
   ((some-fn nil? (con/hvector-c? symbol? r/KwArgs?)) kws)
   ((some-fn nil? (con/hvector-c? symbol? r/Type?)) rest)
   ((some-fn nil? (con/hvector-c? symbol? r/Type?)) prest)
   ((some-fn nil? (con/hvector-c? symbol? r/DottedPretype?)) drest)
   ((some-fn nil? (con/hvector-c? symbol? r/DottedPretype?)) pdot)
   (r/TCResult? body)])

;[FnResult -> Function]
(defn FnResult->Function [{:keys [args kws rest drest prest pdot body] :as fres} opts]
  {:pre [(FnResult? fres)]
   :post [(r/Function? %)]}
  (let [; names of formal parameters to abstract from result type
        rest-param-name ((some-fn first)
                         rest
                         drest
                         prest
                         kws
                         pdot)
        arg-names (concat (map first args)
                          (some-> rest-param-name list))]
    (r/make-Function
      (mapv second args)
      (abo/abstract-result body arg-names opts)
      :rest (some-> rest second)
      :drest (some-> drest second)
      :kws (some-> kws second)
      :prest (some-> prest second)
      :pdot (some-> pdot second))))

; takes the current rest or drest argument (only one is non-nil) and returns
; the type to assign the rest parameter
(defn check-rest-fn [remain-dom {:keys [rest drest kws prest pdot kind] :as ts*} opts]
  {:pre [((some-fn nil? r/Type?) rest)
         ((some-fn nil? r/Type?) prest)
         ((some-fn nil? r/DottedPretype?) drest)
         ((some-fn nil? r/Type?) pdot)
         ((some-fn nil? r/KwArgs?) kws)
         (keyword? kind)
         (every? r/Type? remain-dom)]
   :post [(r/Type? %)]}
  (when-not (<= 0 (count (filter some? (vals (dissoc ts* :kind)))) 1)
    (err/nyi-error (str "Checking rest function with: " ts*) opts))
  (cond
    (or rest drest (= :fixed kind))
    ; rest argument is nilable non-empty seq, refine further based on remaining fixed domain
    (cond-> (c/In [(r/-hseq remain-dom
                            :rest rest
                            :drest drest)
                   ;; in addition to hseq's count knowledge
                   (r/make-CountRange 1)]
                  opts)
      ;TODO what about `drest`?
      (and rest (zero? (count remain-dom))) (as-> t (c/Un [t r/-nil] opts)))

    (seq remain-dom) (err/nyi-error "Rest parameter with remaining fixed domain for prest/post/KwArgs" opts)

    (or prest pdot) (c/Un [r/-nil (or prest pdot)] opts)

    kws (c/KwArgs->Type kws opts)
    
    :else (err/nyi-error (str "Function :kind " kind) opts)))

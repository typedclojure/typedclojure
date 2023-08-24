;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.fn-method-utils
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.abo :as abo]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

;lam-result in TR
(u/def-type FnResult [args kws rest drest prest pdot body]
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
(defn FnResult->Function [{:keys [args kws rest drest prest pdot body] :as fres}]
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
      (map second args)
      (abo/abstract-result body arg-names)
      :rest (some-> rest second)
      :drest (some-> drest second)
      :kws (some-> kws second)
      :prest (some-> prest second)
      :pdot (some-> pdot second))))

; takes the current rest or drest argument (only one is non-nil) and returns
; the type to assign the rest parameter
(defn check-rest-fn [remain-dom {:keys [rest drest kws prest pdot kind] :as ts*}]
  {:pre [((some-fn nil? r/Type?) rest)
         ((some-fn nil? r/Type?) prest)
         ((some-fn nil? r/DottedPretype?) drest)
         ((some-fn nil? r/Type?) pdot)
         ((some-fn nil? r/KwArgs?) kws)
         (#{0 1} (count (filter some? (vals ts*))))
         (every? r/Type? remain-dom)]
   :post [(r/Type? %)]}
  (cond
    (or rest drest (= :fixed kind))
    ; rest argument is nilable non-empty seq, refine further based on remaining fixed domain
    (cond-> (c/In (r/-hseq remain-dom
                           :rest rest
                           :drest drest)
                  ;; in addition to hseq's count knowledge
                  (r/make-CountRange 1))
      ;TODO what about `drest`?
      (and rest (zero? (count remain-dom))) (c/Un r/-nil))

    (seq remain-dom) (err/nyi-error "Rest parameter with remaining fixed domain for prest/post/KwArgs")

    (or prest pdot) (c/Un r/-nil (or prest pdot))

    kws (c/KwArgs->Type kws)
    
    :else (err/nyi-error (str "Function :kind " kind))))

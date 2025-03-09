;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.check.recur-utils
  (:require [typed.clojure :as t]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-rep :as r])
  (:import (typed.cljc.checker.type_rep DottedPretype)))

;;TODO add `kind` field
(u/def-type RecurTarget [dom :- (t/SequentialColl '[t/Sym r/Type])
                         rest :- (t/Nilable r/Type)
                         drest :- (t/Nilable DottedPretype)
                         kws :- nil]
  "A target for recur"
  [(every? r/Type? dom)
   ((some-fn nil? r/Type?) rest)
   ((some-fn nil? r/DottedPretype?) drest)
   (nil? kws)]) ;TODO

(defmacro ^:private set-validator-doc! [var val-fn]
  `(set-validator! ~var (fn [a#] (assert (~val-fn a#)
                                         (str "Invalid reference state: " ~var
                                              " with value: "
                                              (pr-str a#)))
                          true)))

(defmacro with-recur-target [opts tgt]
  `(assoc ~opts ::recur-target ~tgt))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.path-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.def-utils :as du]
            [clojure.core.typed.contract-utils :as con]
            [typed.clojure :as t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(t/ann-protocol IPathElem)
(du/defprotocol IPathElem)

(t/ann ^:no-check PathElem? (t/Pred IPathElem))
(defn PathElem? [a]
  (satisfies? IPathElem a))

(t/ann ^:no-check declare-path-elem [Class -> t/Any])
(defn declare-path-elem [c]
  (extend c IPathElem {}))

(u/def-object NthPE [;; More specific?
                     idx :- Number]
  "A path accessing an indexed member, as by clojure.core/first, second, nth"
  [(integer? idx)
   (not (neg? idx))])

(u/def-object NextPE []
  "A path calling clojure.core/next"
  [])

(u/def-object ClassPE []
  "A path calling clojure.core/class"
  [])

(u/def-object CountPE []
  "A path calling clojure.core/count"
  [])

(u/def-object KeyPE [val :- t/Kw]
  "A key in a hash-map"
  [(keyword? val)])

(t/ann ^:no-check -kpe [t/Kw -> KeyPE])
(def -kpe KeyPE-maker)

(u/def-object KeysPE []
  "Calling clojure.core/keys"
  [])

(u/def-object ValsPE []
  "Calling clojure.core/vals"
  [])

(u/def-object KeywordPE []
  "Calling clojure.core/keyword with a single argument."
  [])

(u/def-object SeqPE []
  "Result of clojure.core/seq."
  [])

(declare-path-elem NthPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)
(declare-path-elem KeysPE)
(declare-path-elem ValsPE)
(declare-path-elem KeywordPE)
(declare-path-elem SeqPE)

(def path-elems? (every-pred (some-fn nil? seq)
                             (con/every-c? PathElem?)))

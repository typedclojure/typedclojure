;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure
  ^{:doc
    "Type annotations for clj-http.client functions."}
  typed.ann.clj-http.client
  (:require [typed.clojure :refer [ann defalias] :as t]))

;;=======================
;; Type aliases

(defalias HttpMethod
  (t/U ':get ':post ':put ':patch ':delete ':head ':options))

(defalias ResponseMap
  (t/Map t/Any t/Any))

;;=======================
;; Annotations

; HTTP verb functions - support both (url) and (url request-map) arities
(ann clj-http.client/get
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

(ann clj-http.client/post
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

(ann clj-http.client/put
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

(ann clj-http.client/patch
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

(ann clj-http.client/delete
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

(ann clj-http.client/head
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

(ann clj-http.client/options
     (t/IFn [t/Str :-> ResponseMap]
            [t/Str (t/Map t/Keyword t/Any) :-> ResponseMap]))

; Main request function
(ann clj-http.client/request
     [(t/Map t/Keyword t/Any) :-> ResponseMap])

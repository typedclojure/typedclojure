;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:doc 
    "This namespace helper macros for type checking core.async code.

    Annotations are (automatically) provided by typed.ann.clojure.core.async, however
    all type aliases are qualified under typed.lib.clojure.core.async,
    so users should only depend on this namespace.
    
    chan
      use chan

    buffer
      use buffer (similar for other buffer constructors)
    "}
  typed.lib.clojure.core.async
  (:require [clojure.core.async :as async]
            [typed.clojure :as-alias t]))

;;;;;;;;;;;;;;;;;;;;
;; Typed wrappers

^::t/ignore
(defn- ^:no-doc maybe-annotation [args]
  (let [t? (#{:-} (first args))
        t (when t? (second args))
        args (if t? 
               (drop 2 args)
               args)]
    [t? t args]))

;; use async/go-loop
#_
(defmacro go-loop
  "Like (go (t/loop ...))"
  [& body]
  (let [[t? t body] (maybe-annotation body)]
    (if t?
      `(go :- ~t (t/loop ~@body))
      `(go (t/loop ~@body)))))

(defmacro chan
  "Like chan but with optional type annotations.

  (chan :- t ...) creates a channel that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/chan."
  [& args]
  (let [[t? t args] (maybe-annotation args)]
    (cond-> `(async/chan ~@args)
      t? (vary-meta assoc ::t/- `(Chan ~t)))))

(defmacro buffer
  "Like buffer but with optional type annotations.

  (buffer :- t ...) creates a buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/buffer."
  [& args]
  (let [[t? t args] (maybe-annotation args)
        frm `(async/buffer ~@args)]
    (if t?
      `(t/ann-form ~frm (Buffer ~t))
      frm)))

(defmacro sliding-buffer
  "Like sliding-buffer but with optional type annotations.

  (sliding-buffer :- t ...) creates a sliding buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/sliding-buffer."
  [& args]
  (let [[t? t args] (maybe-annotation args)
        frm `(async/sliding-buffer ~@args)]
    (if t?
      `(t/ann-form ~frm (Buffer ~t))
      frm)))


(defmacro dropping-buffer
  "Like dropping-buffer but with optional type annotations.

  (dropping-buffer :- t ...) creates a dropping buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/dropping-buffer.

  Note: (dropping-buffer :- t ...) is the same as (t/ann-form (dropping-buffer ...) (Buffer t))"
  [& args]
  (let [[t? t args] (maybe-annotation args)
        frm `(async/dropping-buffer ~@args)]
    (if t?
      `(t/ann-form ~frm (Buffer ~t))
      frm)))

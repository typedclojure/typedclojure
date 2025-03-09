;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; in its own namespace so we can make another version of this for
;; clj-kondo hooks, which doesn't recognize .cljc files.
(ns ^:typed.clojure ^:no-doc clojure.core.typed.platform-case)

;https://github.com/cgrand/macrovich/blob/master/src/net/cgrand/macrovich.cljc
(defmacro platform-case
  "Dispatch based on the compiler in charge of macroexpanding this form.
  If called directly, must be inside a defmacro form."
  [& {:keys [cljs clj]}]
  (cond
    ;; is this platform-case form expanding directly inside a defmacro?
    (contains? &env '&env)
    ;; then reuse the &env from the surrounding defmacro call
    `(if (:ns ~'&env) ~cljs ~clj)

    :else
    (if #?(;; :ns is added by cljs.analyzer, not Compiler.java
           :clj (:ns &env)
           ;; don't need to bother checking
           :cljs true)
      cljs
      clj)))


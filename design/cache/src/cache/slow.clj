(ns cache.slow
  (:require [typed.clojure :as t]))

(defmacro slow-macro
  "Takes 1 second to type check via cache.rules__slow-macro/defuspecial__slow-macro"
  [])

(t/ann a [t/Int :-> t/Bool])
(defn a
  "A docstring for a"
  [x]
  ^{::t/dbg "checking a"}
  (slow-macro)
  (boolean x))

(t/ann b [t/Int :-> t/Bool])
(defn b
  "A docstring for b"
  [x]
  ^{::t/dbg "checking b"}
  (slow-macro)
  (boolean x))

(t/ann c [t/Int :-> t/Bool])
(defn c
  "A docstring for c"
  [x]
  ^{::t/dbg "checking c"}
  (slow-macro)
  (boolean x))

(comment
  (t/check-ns-clj)
  ;; Start checking cache.slow
  ;; ::t/dbg id=G__41651 checking a
  ;; ::t/dbg id=G__41651 (slow-macro)
  ;; Checking slow-macro...
  ;; Checked slow-macro.
  ;; ::t/dbg id=G__41651 result: t/Any
  ;; ::t/dbg id=G__41657 checking b
  ;; ::t/dbg id=G__41657 (slow-macro)
  ;; Checking slow-macro...
  ;; Checked slow-macro.
  ;; ::t/dbg id=G__41657 result: t/Any
  ;; ::t/dbg id=G__41663 checking c
  ;; ::t/dbg id=G__41663 (slow-macro)
  ;; Checking slow-macro...
  ;; Checked slow-macro.
  ;; ::t/dbg id=G__41663 result: t/Any
  ;; Checked cache.slow in 3128.400128 msecs
  :ok
  )

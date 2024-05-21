(ns cache.slow
  {:typed.clojure {:experimental #{:cache}}}
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
  ;Start checking cache.slow
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (ns cache.slow {:typed.clojure {:experimental #{:cache}}} (:require [typed.clojure :as t]))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/ns {}},
  ;  :clojure.core.typed.current-impl/current-impl {}},
  ; :typed.cljc.checker.check.cache/vars #{#'clojure.core/ns},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defmacro slow-macro "Takes 1 second to type check via cache.rules__slow-macro/defuspecial__slow-macro" [])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defmacro {}},
  ;  :clojure.core.typed.current-impl/current-impl {}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'cache.slow/slow-macro
  ;   #'clojure.core/fn
  ;   #'clojure.core/defn
  ;   #'clojure.core/defmacro},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann a [t/Int :-> t/Bool])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore {}},
  ;  :clojure.core.typed.current-impl/current-impl {}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'typed.clojure/ann
  ;   #'clojure.core.typed/ann*
  ;   #'clojure.core.typed/ann
  ;   #'clojure.core.typed/tc-ignore},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__156323 checking a
  ;::t/dbg id=G__156323 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__156323 result: t/Any
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn a "A docstring for a" [x] (slow-macro) (boolean x))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn {}, clojure.core/fn {}, cache.slow/slow-macro {}},
  ;  :clojure.core.typed.current-impl/current-impl {},
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.slow/a {}, clojure.core/boolean {}},
  ;  :clojure.core.typed.current-impl/current-nocheck-var? {},
  ;  :clojure.core.typed.current-impl/current-used-vars {},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   java.lang.Integer {},
  ;   java.math.BigInteger {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn {},
  ;   java.lang.Long {},
  ;   java.lang.Byte {},
  ;   clojure.lang.BigInt {},
  ;   typed.clojure/Int {},
  ;   typed.clojure/AnyInteger {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.lang.Comparable {}}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'clojure.core/boolean
  ;   #'cache.slow/slow-macro
  ;   #'clojure.core/fn
  ;   #'clojure.core/defn},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.RT/booleanCast}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann b [t/Int :-> t/Bool])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore {}},
  ;  :clojure.core.typed.current-impl/current-impl {}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'typed.clojure/ann
  ;   #'clojure.core.typed/ann*
  ;   #'clojure.core.typed/ann
  ;   #'clojure.core.typed/tc-ignore},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__156341 checking b
  ;::t/dbg id=G__156341 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__156341 result: t/Any
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn b "A docstring for b" [x] (slow-macro) (boolean x))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn {}, clojure.core/fn {}, cache.slow/slow-macro {}},
  ;  :clojure.core.typed.current-impl/current-impl {},
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.slow/b {}, clojure.core/boolean {}},
  ;  :clojure.core.typed.current-impl/current-nocheck-var? {},
  ;  :clojure.core.typed.current-impl/current-used-vars {},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   java.lang.Integer {},
  ;   java.math.BigInteger {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn {},
  ;   java.lang.Long {},
  ;   java.lang.Byte {},
  ;   clojure.lang.BigInt {},
  ;   typed.clojure/Int {},
  ;   typed.clojure/AnyInteger {}}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'clojure.core/boolean
  ;   #'cache.slow/slow-macro
  ;   #'clojure.core/fn
  ;   #'clojure.core/defn},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.RT/booleanCast}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann c [t/Int :-> t/Bool])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore {}},
  ;  :clojure.core.typed.current-impl/current-impl {}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'typed.clojure/ann
  ;   #'clojure.core.typed/ann*
  ;   #'clojure.core.typed/ann
  ;   #'clojure.core.typed/tc-ignore},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__156347 checking c
  ;::t/dbg id=G__156347 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__156347 result: t/Any
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn c "A docstring for c" [x] (slow-macro) (boolean x))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn {}, clojure.core/fn {}, cache.slow/slow-macro {}},
  ;  :clojure.core.typed.current-impl/current-impl {},
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.slow/c {}, clojure.core/boolean {}},
  ;  :clojure.core.typed.current-impl/current-nocheck-var? {},
  ;  :clojure.core.typed.current-impl/current-used-vars {},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   java.lang.Integer {},
  ;   java.math.BigInteger {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn {},
  ;   java.lang.Long {},
  ;   java.lang.Byte {},
  ;   clojure.lang.BigInt {},
  ;   typed.clojure/Int {},
  ;   typed.clojure/AnyInteger {}}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'clojure.core/boolean
  ;   #'cache.slow/slow-macro
  ;   #'clojure.core/fn
  ;   #'clojure.core/defn},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.RT/booleanCast}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (comment (t/check-ns-clj) :ok)
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl {}},
  ; :typed.cljc.checker.check.cache/vars #{#'clojure.core/comment},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;Checked cache.slow in 3107.389875 msecs
  :ok
  )

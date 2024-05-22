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
  ;  {clojure.core/ns
  ;   typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars {ns #'clojure.core/ns},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defmacro slow-macro "Takes 1 second to type check via cache.rules__slow-macro/defuspecial__slow-macro" [])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defmacro
  ;   typed.clj.ext.clojure.core__defmacro/defuspecial__defmacro},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {defmacro #'clojure.core/defmacro,
  ;  clojure.core/defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  slow-macro #'cache.slow/slow-macro},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann a [t/Int :-> t/Bool])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/ann #'typed.clojure/ann,
  ;  clojure.core.typed/ann #'clojure.core.typed/ann,
  ;  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
  ;  clojure.core.typed/ann* #'clojure.core.typed/ann*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__179582 checking a
  ;::t/dbg id=G__179582 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__179582 result: t/Any
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn a "A docstring for a" [x] (slow-macro) (boolean x))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn,
  ;   cache.slow/slow-macro
  ;   cache.rules__slow-macro/defuspecial__slow-macro},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.slow/a [typed.clojure/Int :-> typed.clojure/Bool],
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars #{},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   java.lang.Integer {},
  ;   java.math.BigInteger {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   java.lang.Long {},
  ;   java.lang.Byte {},
  ;   clojure.lang.BigInt {},
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long)},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.lang.Comparable
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.lang.Comparable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  slow-macro #'cache.slow/slow-macro,
  ;  boolean #'clojure.core/boolean,
  ;  clojure.lang.RT clojure.lang.RT},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.RT/booleanCast}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann b [t/Int :-> t/Bool])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/ann #'typed.clojure/ann,
  ;  clojure.core.typed/ann #'clojure.core.typed/ann,
  ;  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
  ;  clojure.core.typed/ann* #'clojure.core.typed/ann*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__179589 checking b
  ;::t/dbg id=G__179589 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__179589 result: t/Any
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn b "A docstring for b" [x] (slow-macro) (boolean x))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn,
  ;   cache.slow/slow-macro
  ;   cache.rules__slow-macro/defuspecial__slow-macro},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.slow/b [typed.clojure/Int :-> typed.clojure/Bool],
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   java.lang.Integer {},
  ;   java.math.BigInteger {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   java.lang.Long {},
  ;   java.lang.Byte {},
  ;   clojure.lang.BigInt {},
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long)}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  slow-macro #'cache.slow/slow-macro,
  ;  boolean #'clojure.core/boolean,
  ;  clojure.lang.RT clojure.lang.RT},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.RT/booleanCast}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann c [t/Int :-> t/Bool])
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/ann #'typed.clojure/ann,
  ;  clojure.core.typed/ann #'clojure.core.typed/ann,
  ;  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
  ;  clojure.core.typed/ann* #'clojure.core.typed/ann*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__179595 checking c
  ;::t/dbg id=G__179595 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__179595 result: t/Any
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn c "A docstring for c" [x] (slow-macro) (boolean x))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn,
  ;   cache.slow/slow-macro
  ;   cache.rules__slow-macro/defuspecial__slow-macro},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.slow/c [typed.clojure/Int :-> typed.clojure/Bool],
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   java.lang.Integer {},
  ;   java.math.BigInteger {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   java.lang.Long {},
  ;   java.lang.Byte {},
  ;   clojure.lang.BigInt {},
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long)}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  slow-macro #'cache.slow/slow-macro,
  ;  boolean #'clojure.core/boolean,
  ;  clojure.lang.RT clojure.lang.RT},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.RT/booleanCast}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (comment (t/check-ns-clj) :ok)
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars {comment #'clojure.core/comment},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;Checked cache.slow in 3114.307141 msecs
  :ok
  )

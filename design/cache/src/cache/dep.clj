(ns cache.dep
  {:typed.clojure {:experimental #{:cache}}}
  (:require [cache.dep1 :as dep1]
            [typed.clojure :as t]))

(t/ann foo [:-> t/Any])
(defn foo [] (inc (dep1/a)))

(comment
  (t/check-ns-clj *ns* :check-config {:check-ns-dep :recheck})
  ;Start checking cache.slow
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (ns cache.slow {:typed.clojure {:experimental #{:cache}}} (:require [typed.clojure :as t]))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/ns
  ;   typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars #{#'clojure.core/ns},
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
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'typed.clojure/ann
  ;   #'clojure.core.typed/ann*
  ;   #'clojure.core.typed/ann
  ;   #'clojure.core.typed/tc-ignore},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__163104 checking a
  ;::t/dbg id=G__163104 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__163104 result: t/Any
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
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'typed.clojure/ann
  ;   #'clojure.core.typed/ann*
  ;   #'clojure.core.typed/ann
  ;   #'clojure.core.typed/tc-ignore},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__163111 checking b
  ;::t/dbg id=G__163111 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__163111 result: t/Any
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
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'typed.clojure/ann
  ;   #'clojure.core.typed/ann*
  ;   #'clojure.core.typed/ann
  ;   #'clojure.core.typed/tc-ignore},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__163117 checking c
  ;::t/dbg id=G__163117 (slow-macro)
  ;Checking slow-macro...
  ;Checked slow-macro.
  ;::t/dbg id=G__163117 result: t/Any
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
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars #{#'clojure.core/comment},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;Checked cache.slow in 3101.15662 msecs
  :ok
  )

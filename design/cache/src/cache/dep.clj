(ns cache.dep
  {:typed.clojure {:experimental #{:cache}}}
  (:require [cache.dep1 :as dep1]
            [typed.clojure :as t]))

(t/ann foo [:-> t/Any])
(defn foo [] (inc (dep1/a)))

(comment
  (t/check-ns-clj *ns* :check-config {:check-ns-dep :recheck})
  ;Not checking typed.clojure (tagged with :typed.clojure/ignore metadata)
  ;Not checking clojure.core.typed (tagged with :typed.clojure/ignore metadata)
  ;Start checking cache.dep1
  ;Checked cache.dep1 in 19.371123 msecs
  ;Start checking cache.dep
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (ns cache.dep {:typed.clojure {:experimental #{:cache}}} (:require [cache.dep1 :as dep1] [typed.clojure :as t]))
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
  ;cache: dependencies for form: (t/ann foo [:-> t/Any])
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
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn foo [] (inc (dep1/a)))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.dep/foo [:-> typed.clojure/Any],
  ;   clojure.core/inc
  ;   (typed.clojure/IFn
  ;    [java.lang.Long :-> java.lang.Long]
  ;    [java.lang.Double :-> java.lang.Double]
  ;    [typed.clojure/AnyInteger :-> typed.clojure/AnyInteger]
  ;    [typed.clojure/Num :-> typed.clojure/Num]),
  ;   cache.dep1/a [:-> typed.clojure/Int]},
  ;  :clojure.core.typed.current-impl/current-used-vars #{},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Long {},
  ;   java.lang.Short {},
  ;   java.lang.Double {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   clojure.lang.Fn {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.lang.Comparable
  ;   (typed.clojure/TFn
  ;    [[a161144 :variance :invariant]]
  ;    (java.lang.Comparable a161144))}},
  ; :typed.cljc.checker.check.cache/vars
  ; #{#'clojure.core/fn
  ;   #'clojure.core/inc
  ;   #'clojure.core/defn
  ;   #'cache.dep1/a},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop
  ; {:static-call #{clojure.lang.Numbers/inc}}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (comment (t/check-ns-clj *ns* :check-config {:check-ns-dep :recheck}) :ok)
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars #{#'clojure.core/comment},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;Checked cache.dep in 35.070917 msecs
  :ok
  )

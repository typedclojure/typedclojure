(ns ^:typed.clojure typed-example.zero-deps
  "We want to distribute this ns in a jar that does not depend
  on Typed Clojure, but we need to add local annotations."
  (:require ;; alias typed.clojure as t without loading in Clojure
            [typed.clojure :as-alias t]
            ;; we want to refer to our MyStr type alias, but we can't require
            ;; our annotation namespace... :as-alias to the rescue!
            [typed-example.zero-deps.typedclojure-ann :as-alias ann]))

;; annotated in src/typed_example/zero_deps/typedclojure_ann.cljc, which is automatically
;; loaded by the type checker via resources/typedclojure_config.cljc
(defn foo [coll]
  (reduce (fn [;; the keyword :typed.clojure/- is used to annotate function arguments.
               ;; This works because non-:tag metadata is not evaluated by the Clojure
               ;; compiler, and is thrown away at runtime. Try evaluating (fn [^{:foo (nil)} a])
               ;; and notice no "Cannot invoke nil" error.
               ^{:typed.clojure/- (t/Vec ann/MyStr)}
               acc
               ;; aliases can make the key shorter if you can stomach it.
               ^{::t/- ann/MyStr}
               e]
            (conj acc e))
          [] coll))

(comment
  ((requiring-resolve `t/check-ns-clj))
  ;; FIXME DataType <: Protocol subtyping case
  ((requiring-resolve `t/check-ns-cljs))
  )

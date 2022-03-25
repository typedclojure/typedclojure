;; this namespace is implicitly loaded via `src/typedclojure_config.cljc` by the type checker
(ns typed-example.register-malli-extensions
  (:require [typed.malli.schema-to-type :refer [register-malli->type-extension]]))

(register-malli->type-extension :typed-example.malli-extensible/over [m opts]
  ;; Do whatever you want to convert the schema to Typed Clojure syntax.
  ;; Mostly, you'd just want to return a type.
  ;; More involved ideas include guessing the type by generating values until `:pred` succeeds.
  ;; If the type is different based on platform, use impl/impl-case to dispatch (used here
  ;; just for illustration purposes).
  #?(:clj `t/AnyInteger
     :cljs `t/AnyInteger))

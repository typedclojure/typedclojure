;; this namespace is implicitly loaded via `src/typedclojure_config.cljc` by the type checker
(ns typed-example.register-spec1-extensions
  (:require [typed.spec1.spec-to-type :refer [register-spec-syntax->type-extension]]))

(register-spec-syntax->type-extension :typed-example.spec1-extensible/over3 [s opts]
  ;; Do whatever you want to convert the schema to Typed Clojure syntax.
  ;; Mostly, you'd just want to return a type.
  ;; More involved ideas include guessing the type by generating values until `:pred` succeeds.
  `t/AnyInteger)

(register-spec-syntax->type-extension :typed-example.spec1-extensible/over4 [s opts] `t/AnyInteger)

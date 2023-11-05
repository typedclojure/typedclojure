#!/bin/bash

set -e

cd typed/clj.runtime
mkdir -p resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed

cp src/clojure/core/typed/macros.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed
cp src/clojure/core/typed/internal.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed
cp src/clojure/core/typed/contract_utils.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed
cp src/clojure/core/typed/special_form.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed

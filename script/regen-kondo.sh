#!/bin/bash

set -e

bb -f ./script/regen_kondo_config.clj

cd typed/clj.runtime
mkdir -p resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed
mkdir -p resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/typed/cljc/runtime

cp src/clojure/core/typed/macros.cljc resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed/macros.clj
cp src/clojure/core/typed/internal.cljc resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed/internal.clj
cp src/clojure/core/typed/contract_utils.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed
cp src/clojure/core/typed/special_form.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed
cp src/typed/cljc/runtime/perf_utils.clj resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/typed/cljc/runtime

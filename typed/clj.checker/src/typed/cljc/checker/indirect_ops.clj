;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.indirect-ops
  (:refer-clojure :exclude [requiring-resolve])
  (:require [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.clojure :as t]))

(defmacro ^:private make-indirection [& vs]
  (assert (apply distinct? (map name vs)) (vec vs))
  `(do 
     ~@(map (fn [v]
              (assert (qualified-symbol? v) (pr-str v))
              `(do 
                 (t/ann ~(-> v name symbol) (t/TypeOf ~v))
                 (defn ~(-> v name symbol)
                    [& args#]
                    (apply (requiring-resolve '~v) args#))))
            vs)))

(make-indirection
  typed.clj.checker.parse-unparse/unparse-type
  typed.clj.checker.parse-unparse/parse-type
  typed.cljc.checker.check.funapp/check-funapp
  typed.clj.checker.assoc-utils/assoc-pairs-noret
  typed.clj.checker.subtype/subtype?
  typed.clj.checker.subtype/has-kind?
  typed.cljc.checker.filter-ops/-FS
  typed.cljc.checker.filter-rep/-top-fn
  typed.cljc.checker.object-rep/-empty-fn
  typed.cljc.checker.cs-gen/infer
  typed.cljc.checker.cs-gen/solve
  typed.cljc.checker.lex-env/PropEnv?
  typed.cljc.checker.filter-ops/-or
  typed.cljc.checker.filter-ops/-and
  typed.cljc.checker.var-env/type-of-nofail)

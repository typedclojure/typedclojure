{:ann [typed.cljc.runtime.env-utils-annotations
       ;; for indirect-ops t/TypeOf resolution
       ;; otherwise fails when running clojure.core.typed/envs
       typed.clj.checker.assoc-utils
       typed.clj.checker.parse-unparse
       typed.clj.checker.subtype
       typed.cljc.checker.check
       typed.cljc.checker.check.funapp
       typed.cljc.checker.cs-gen
       typed.cljc.checker.filter-ops
       typed.cljc.checker.filter-rep
       typed.cljc.checker.lex-env
       typed.cljc.checker.object-rep
       typed.cljc.checker.var-env]}

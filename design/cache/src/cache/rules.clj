(ns cache.rules
  (:require [typed.clj.checker.check.unanalyzed :as un-clj]))

(un-clj/install-defuspecial
  'cache.slow/slow-macro
  'cache.rules__slow-macro/defuspecial__slow-macro)

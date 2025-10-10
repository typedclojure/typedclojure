(ns hooks.reprod-hooks)

;; Minimal macro hook that uses requiring-resolve
(defmacro reprod []
  (requiring-resolve 'clojure.core/identity)
  nil)

(ns reprod.test)

;; Minimal reproduction: macro hook uses requiring-resolve
(defmacro reprod []
  (requiring-resolve 'clojure.core/identity))

(reprod)

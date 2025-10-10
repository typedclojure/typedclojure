(ns reprod.test)

;; Minimal reproduction: macro hook uses requiring-resolve
(defmacro reprod [])

(reprod)

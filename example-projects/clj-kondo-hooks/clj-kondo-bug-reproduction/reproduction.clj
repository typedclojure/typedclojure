(ns reprod.test)

;; Minimal reproduction: macro hook uses requiring-resolve
(defmacro typed-def [name & args]
  (let [[_ type body] (if (= :- (first args))
                        args
                        [nil nil (first args)])
        ;; This requiring-resolve call requires SCI context
        _ ((requiring-resolve 'clojure.core/identity) type)]
    `(def ~name ~body)))

;; Usage - should work without SCI context errors
(typed-def x :- Number 1)

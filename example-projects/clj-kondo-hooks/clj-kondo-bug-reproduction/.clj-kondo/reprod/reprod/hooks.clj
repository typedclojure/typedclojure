(ns hooks.reprod-hooks)

;; Minimal macro hook that uses requiring-resolve
(defmacro typed-def [name & args]
  (let [[_ type body] (if (= :- (first args))
                        args
                        [nil nil (first args)])
        ;; This requiring-resolve requires SCI context to work
        _ ((requiring-resolve 'clojure.core/identity) type)]
    `(def ~name ~body)))

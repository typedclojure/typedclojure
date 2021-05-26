(ns typed.clj.spec.test-utils
  "Helpers for unit testing typed.clj.spec definitions
  with clojure.test."
  (:require [clojure.alpha.spec :as s]
            [clojure.test :as test]))

(defmacro is-valid [s v]
  `(let [s# ~s
         v# ~v
         valid# (s/valid? s# v#)]
     (test/testing ~(str (list 's/valid? s v))
       (test/is
         valid#
         (when-not valid#
           (str "Not valid: " ~(str (list 's/valid? s v)) "\n"
                "Explain:\n"
                (with-out-str
                  (s/explain s# v#))))))))

(defmacro is-invalid [s v]
  `(test/is
     (not (s/valid? ~s ~v))))

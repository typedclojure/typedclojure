(ns ^:typed.clojure clojure.core.typed.test.recursive
  (:require [typed.clojure :as t :refer [ann-record ann-protocol]]))

(t/declare-protocols IValidator)

(ann-record ValidationError [validator :- IValidator
                             value :- t/Any])

(t/tc-ignore
(defrecord ValidationError [validator value])
)

(ann-record ValidationResult
            [status :- (t/U ':ok ':error)
             result :- t/Any
             errors :- (t/U nil (t/Seq ValidationError))
             input :- t/Any])
 
(t/tc-ignore
(defrecord ValidationResult [status result errors input])
)

(t/defprotocol IValidator
  "Validator abstraction"
  (validate- [this value] :- ValidationResult
             "Evaluates the validator."))
 
(t/ann-form validate- [IValidator t/Any -> ValidationResult])

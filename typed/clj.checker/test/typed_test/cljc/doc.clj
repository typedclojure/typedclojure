(ns typed-test.cljc.doc
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as str]
            [typed.cljc.doc :as sut]))

(deftest type-doc-test
  (is (str/starts-with? (sut/type-doc-clj* nil) "Special type"))
  (is (str/starts-with? (sut/type-doc-clj* 'Integer) "Class java.lang.Integer"))
  (is (nil? (sut/type-doc-cljs* 'Integer)))
  (is (str/starts-with? (sut/type-doc-clj* 'clojure.lang.IPersistentMap) "Class annotation for clojure.lang.IPersistentMap"))
  (is (str/starts-with? (sut/type-doc-cljs* nil) "Special type"))
  (is (str/starts-with? (sut/type-doc-clj* 'clojure.core/+) "Printing the annotation for var clojure.core/+"))
  (is (str/starts-with? (sut/type-doc-cljs* 'clojure.core/+) "Printing the annotation for var cljs.core/+"))
  (is (str/starts-with? (sut/type-doc-clj* 'typed.clojure/Transducer) "Type alias typed.clojure/Transducer"))
  (is (str/starts-with? (sut/type-doc-cljs* 'typed.clojure/Transducer) "Type alias typed.clojure/Transducer")))

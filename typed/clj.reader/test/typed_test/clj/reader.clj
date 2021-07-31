;; based on clojure.tools.reader-test
(ns typed-test.clj.reader
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *data-readers*])
  (:use [typed.clj.reader.impl.utils :exclude [char]])
  (:require [typed.clj.reader :refer [read read-string *default-data-reader-fn* *data-readers*
                                      read-string+ast]]
            [typed.clj.reader.reader-types :refer [string-push-back-reader
                                                   indexing-push-back-reader]]
            [clojure.test :refer [deftest is are testing]]
            ;[typed.clj.reader.edn :as tre];; NYI
            )
  (:import clojure.lang.BigInt
           (java.io StringReader BufferedReader)
           clojure.lang.LineNumberingPushbackReader))

;; copied common_tests from tools.reader
;; here, instead of `load`ing the file, we inline it
;; to please eastwood.
;(load "common_tests")
;; -- Start common_tests

(deftest read-integer
  (is (== 42 (read-string "42")))
  (is (== +42 (read-string "+42")))
  (is (== -42 (read-string "-42")))

  (is (== 42 (read-string "42N")))
  (is (== +42 (read-string "+42N")))
  (is (== -42 (read-string "-42N")))

  (is (== 0 (read-string "0")))
  (is (== 0N (read-string "0N")))

  (is (== 042 (read-string "042")))
  (is (== +042 (read-string "+042")))
  (is (== -042 (read-string "-042")))

  (is (== 0x42e (read-string "0x42e")))
  (is (== +0x42e (read-string "+0x42e")))
  (is (== -0x42e (read-string "-0x42e")))

  (is (instance? Long (read-string "2147483647")))
  (is (instance? Long (read-string "+1")))
  (is (instance? Long (read-string "1")))
  (is (instance? Long (read-string "+0")))
  (is (instance? Long (read-string "0")))
  (is (instance? Long (read-string "-0")))
  (is (instance? Long (read-string "-1")))
  (is (instance? Long (read-string "-2147483648")))

  (is (instance? Long (read-string "2147483648")))
  (is (instance? Long (read-string "-2147483649")))
  (is (instance? Long (read-string "9223372036854775807")))
  (is (instance? Long (read-string "-9223372036854775808")))

  (is (instance? BigInt (read-string "9223372036854775808")))
  (is (instance? BigInt (read-string "-9223372036854775809")))
  (is (instance? BigInt (read-string "10000000000000000000000000000000000000000000000000")))
  (is (instance? BigInt (read-string "-10000000000000000000000000000000000000000000000000"))))

(deftest read-floating
  (is (== 42.23 (read-string "42.23")))
  (is (== +42.23 (read-string "+42.23")))
  (is (== -42.23 (read-string "-42.23")))

  (is (== 42.23M (read-string "42.23M")))
  (is (== +42.23M (read-string "+42.23M")))
  (is (== -42.23M (read-string "-42.23M")))

  (is (== 42.2e3 (read-string "42.2e3")))
  (is (== +42.2e+3 (read-string "+42.2e+3")))
  (is (== -42.2e-3 (read-string "-42.2e-3")))

  (is (== 42.2e3M (read-string "42.2e3M")))
  (is (== +42.2e+3M (read-string "+42.2e+3M")))
  (is (== -42.2e-3M (read-string "-42.2e-3M")))

  (is (instance? Double (read-string "+1.0e+1")))
  (is (instance? Double (read-string "+1.e+1")))
  (is (instance? Double (read-string "+1e+1")))

  (is (instance? Double (read-string "+1.0e+1")))
  (is (instance? Double (read-string "+1.e+1")))
  (is (instance? Double (read-string "+1e+1")))

  (is (instance? Double (read-string "+1.0e1")))
  (is (instance? Double (read-string "+1.e1")))
  (is (instance? Double (read-string "+1e1")))

  (is (instance? Double (read-string "+1.0e-1")))
  (is (instance? Double (read-string "+1.e-1")))
  (is (instance? Double (read-string "+1e-1")))

  (is (instance? Double (read-string "1.0e+1")))
  (is (instance? Double (read-string "1.e+1")))
  (is (instance? Double (read-string "1e+1")))

  (is (instance? Double (read-string "1.0e-1")))
  (is (instance? Double (read-string "1.e-1")))
  (is (instance? Double (read-string "1e-1")))

  (is (instance? Double (read-string "-1.0e+1")))
  (is (instance? Double (read-string "-1.e+1")))
  (is (instance? Double (read-string "-1e+1")))

  (is (instance? Double (read-string "-1.0e1")))
  (is (instance? Double (read-string "-1.e1")))
  (is (instance? Double (read-string "-1e1")))

  (is (instance? Double (read-string "-1.0e-1")))
  (is (instance? Double (read-string "-1.e-1")))
  (is (instance? Double (read-string "-1e-1")))

  (is (instance? Double (read-string "+1.0")))
  (is (instance? Double (read-string "+1.")))

  (is (instance? Double (read-string "1.0")))
  (is (instance? Double (read-string "1.")))

  (is (instance? Double (read-string "+0.0")))
  (is (instance? Double (read-string "+0.")))

  (is (instance? Double (read-string "0.0")))
  (is (instance? Double (read-string "0.")))

  (is (instance? Double (read-string "-0.0")))
  (is (instance? Double (read-string "-0.")))

  (is (instance? Double (read-string "-1.0")))
  (is (instance? Double (read-string "-1.")))

  (is (instance? BigDecimal (read-string "9223372036854775808M")))
  (is (instance? BigDecimal (read-string "-9223372036854775809M")))
  (is (instance? BigDecimal (read-string "2147483647M")))
  (is (instance? BigDecimal (read-string "+1M")))
  (is (instance? BigDecimal (read-string "1M")))
  (is (instance? BigDecimal (read-string "+0M")))
  (is (instance? BigDecimal (read-string "0M")))
  (is (instance? BigDecimal (read-string "-0M")))
  (is (instance? BigDecimal (read-string "-1M")))
  (is (instance? BigDecimal (read-string "-2147483648M")))

  (is (instance? BigDecimal (read-string "+1.0e+1M")))
  (is (instance? BigDecimal (read-string "+1.e+1M")))
  (is (instance? BigDecimal (read-string "+1e+1M")))

  (is (instance? BigDecimal (read-string "+1.0e1M")))
  (is (instance? BigDecimal (read-string "+1.e1M")))
  (is (instance? BigDecimal (read-string "+1e1M")))

  (is (instance? BigDecimal (read-string "+1.0e-1M")))
  (is (instance? BigDecimal (read-string "+1.e-1M")))
  (is (instance? BigDecimal (read-string "+1e-1M")))

  (is (instance? BigDecimal (read-string "1.0e+1M")))
  (is (instance? BigDecimal (read-string "1.e+1M")))
  (is (instance? BigDecimal (read-string "1e+1M")))

  (is (instance? BigDecimal (read-string "1.0e1M")))
  (is (instance? BigDecimal (read-string "1.e1M")))
  (is (instance? BigDecimal (read-string "1e1M")))

  (is (instance? BigDecimal (read-string "1.0e-1M")))
  (is (instance? BigDecimal (read-string "1.e-1M")))
  (is (instance? BigDecimal (read-string "1e-1M")))

  (is (instance? BigDecimal (read-string "-1.0e+1M")))
  (is (instance? BigDecimal (read-string "-1.e+1M")))
  (is (instance? BigDecimal (read-string "-1e+1M")))

  (is (instance? BigDecimal (read-string "-1.0e1M")))
  (is (instance? BigDecimal (read-string "-1.e1M")))
  (is (instance? BigDecimal (read-string "-1e1M")))

  (is (instance? BigDecimal (read-string "-1.0e-1M")))
  (is (instance? BigDecimal (read-string "-1.e-1M")))
  (is (instance? BigDecimal (read-string "-1e-1M")))

  (is (instance? BigDecimal (read-string "+1.0M")))
  (is (instance? BigDecimal (read-string "+1.M")))

  (is (instance? BigDecimal (read-string "1.0M")))
  (is (instance? BigDecimal (read-string "1.M")))

  (is (instance? BigDecimal (read-string "+0.0M")))
  (is (instance? BigDecimal (read-string "+0.M")))

  (is (instance? BigDecimal (read-string "0.0M")))
  (is (instance? BigDecimal (read-string "0.M")))

  (is (instance? BigDecimal (read-string "-0.0M")))
  (is (instance? BigDecimal (read-string "-0.M")))

  (is (instance? BigDecimal (read-string "-1.0M")))
  (is (instance? BigDecimal (read-string "-1.M"))))

(deftest read-ratio
  (is (== 4/2 (read-string "4/2")))
  (is (== 4/2 (read-string "+4/2")))
  (is (== -4/2 (read-string "-4/2"))))


(deftest read-symbol
  (is (= 'foo (read-string "foo")))
  (is (= 'foo/bar (read-string "foo/bar")))
  (is (= '*+!-_? (read-string "*+!-_?")))
  (is (= 'abc:def:ghi (read-string "abc:def:ghi")))
  (is (= 'abc.def/ghi (read-string "abc.def/ghi")))
  (is (= 'abc/def.ghi (read-string "abc/def.ghi")))
  (is (= 'abc:def/ghi:jkl.mno (read-string "abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Symbol (read-string "alphabet")))
  (is (= "foo//" (str (read-string "foo//")))) ;; the clojure reader can't read this
  (is (= (str 'NaN) (str (read-string "##NaN"))))
  (is (= Double/POSITIVE_INFINITY (read-string "##Inf")))
  (is (= Double/NEGATIVE_INFINITY (read-string "##-Inf"))))

(deftest read-specials
  (is (= 'nil nil))
  (is (= 'false false))
  (is (= 'true true)))

(deftest read-char
  (is (= \f (read-string "\\f")))
  (is (= \u0194 (read-string "\\u0194")))
  (is (= \o123 (read-string "\\o123")))
  (is (= \newline (read-string "\\newline")))
  (is (= (char 0) (read-string "\\o0")))
  (is (= (char 0) (read-string "\\o000")))
  (is (= (char 0377) (read-string "\\o377")))
  (is (= \A (read-string "\\u0041")))
  (is (= \@ (read-string "\\@")))
  (is (= (char 0xd7ff) (read-string "\\ud7ff")))
  (is (= (char 0xe000) (read-string "\\ue000")))
  (is (= (char 0xffff) (read-string "\\uffff"))))

(deftest read-string*
  (is (= "foo bar" (read-string "\"foo bar\"")))
  (is (= "foo\\bar" (read-string "\"foo\\\\bar\"")))
  (is (= "foo\000bar" (read-string "\"foo\\000bar\"")))
  (is (= "foo\u0194bar" (read-string "\"foo\\u0194bar\"")))
  (is (= "foo\123bar" (read-string "\"foo\\123bar\"")))
  (is (= "\060" (read-string "\"\\060\"")))
  (is (= "\340" (read-string "\"\\340\"")))
  (is (= "\377" (read-string "\"\\377\""))))

(deftest read-list
  (is (= '() (read-string "()")))
  (is (= '(foo bar) (read-string "(foo bar)")))
  (is (= '(foo (bar) baz) (read-string "(foo (bar) baz)"))))

(deftest read-vector
  (is (= '[] (read-string "[]")))
  (is (= '[foo bar] (read-string "[foo bar]")))
  (is (= '[foo [bar] baz] (read-string "[foo [bar] baz]"))))

(deftest read-map
  (is (= '{} (read-string "{}")))
  (is (= '{foo bar} (read-string "{foo bar}")))
  (is (= '{foo {bar baz}} (read-string "{foo {bar baz}}"))))

(deftest read-set
  (is (= '#{} (read-string "#{}")))
  (is (= '#{foo bar} (read-string "#{foo bar}")))
  (is (= '#{foo #{bar} baz} (read-string "#{foo #{bar} baz}"))))

(deftest read-metadata
  (is (= {:foo true} (meta (read-string "^:foo 'bar"))))
  (is (= {:foo 'bar} (meta (read-string "^{:foo bar} 'baz"))))
  (is (= {:tag "foo"} (meta (read-string "^\"foo\" 'bar"))))
  (is (= {:tag 'String} (meta (read-string "^String 'x")))))

(deftest read-namespaced-map
  (is (= {:foo/bar 1 :baz 2} (read-string "#:foo{:bar 1 :_/baz 2}")))
  (is (= '{foo/bar 1 :baz 2} (read-string "#:foo{bar 1 :_/baz 2}"))))

;; -- End common_tests

(deftest read-keyword
  (is (= :foo-bar (read-string ":foo-bar")))
  (is (= :foo/bar (read-string ":foo/bar")))
  (is (= :user/foo-bar (binding [*ns* (the-ns 'user)]
                         (read-string "::foo-bar"))))
  (is (= :clojure.core/foo-bar
         (do (alias 'core 'clojure.core)
             (read-string "::core/foo-bar"))))
  (is (= :*+!-_? (read-string ":*+!-_?")))
  (is (= :abc:def:ghi (read-string ":abc:def:ghi")))
  (is (= :abc.def/ghi (read-string ":abc.def/ghi")))
  (is (= :abc/def.ghi (read-string ":abc/def.ghi")))
  (is (= :abc:def/ghi:jkl.mno (read-string ":abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword (read-string ":alphabet"))) )

(deftest read-regex
  (is (= (str #"\[\]?(\")\\")
         (str (read-string "#\"\\[\\]?(\\\")\\\\\"")))))

(deftest read-quote
  (is (= ''foo (read-string "'foo"))))

(deftest read-syntax-quote
  (is (= '`user/foo (binding [*ns* (the-ns 'user)]
                      (read-string "`foo"))))
  (is (= () (eval (read-string "`(~@[])"))))
  (is (= '`+ (read-string "`+")))
  (is (= '`foo/bar (read-string "`foo/bar")))
  (is (= '`1 (read-string "`1")))
  (is (= `(1 (~2 ~@'(3))) (eval (read-string "`(1 (~2 ~@'(3)))")))))

(deftest read-deref
  (is (= '@foo (read-string "@foo"))))

(deftest read-var
  (is (= '(var foo) (read-string "#'foo"))))

(deftest read-fn
  (is (= '(fn* [] (foo bar baz)) (read-string "#(foo bar baz)"))))

(deftest read-arg
  (is (= 14 ((eval (read-string "#(apply + % %1 %3 %&)")) 1 2 3 4 5)))
  (is (= 4 ((eval (read-string "#(last %&)")) 1 2 3 4))))

(deftest read-eval
  (is (= 3 (read-string "#=(+ 1 2)"))))

(deftest read-tagged
  (is (= #inst "2010-11-12T13:14:15.666"
         (read-string "#inst \"2010-11-12T13:14:15.666\"")))
  (is (= #inst "2010-11-12T13:14:15.666"
         (read-string "#inst\"2010-11-12T13:14:15.666\"")))
  (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
         (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (java.util.UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (java.util.UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
                  (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (when *default-data-reader-fn*
    (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})]
      (is (= {:unknown-tag 'foo :value 'bar}
             (binding [*default-data-reader-fn* my-unknown]
               (read-string "#foo bar")))))))

(defrecord foo [])
(defrecord bar [baz buz])

;; TODO roundtrip
(deftest read-record
  (is (= (foo.) (read-string "#typed_test.clj.reader.foo[]")))
  (is (= (foo.) (read-string "#typed_test.clj.reader.foo []"))) ;; not valid in clojure
  (is (= (foo.) (read-string "#typed_test.clj.reader.foo{}")))
  (is (= (assoc (foo.) :foo 'bar) (read-string "#typed_test.clj.reader.foo{:foo bar}")))

  (is (= (map->bar {}) (read-string "#typed_test.clj.reader.bar{}")))
  (is (= (bar. 1 nil) (read-string "#typed_test.clj.reader.bar{:baz 1}")))
  (is (= (bar. 1 nil) (read-string "#typed_test.clj.reader.bar[1 nil]")))
  (is (= (bar. 1 2) (read-string "#typed_test.clj.reader.bar[1 2]"))))

(deftest read-ctor
  (is (= "foo" (read-string "#java.lang.String[\"foo\"]"))))

(defrecord JSValue [v])

(deftest reader-conditionals
  (let [opts {:read-cond :allow :features #{:clj}}]
    (are [out s opts] (= out (read-string opts s))
         ;; basic read-cond
         '[foo-form] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:foo}}
         '[bar-form] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:bar}}
         '[foo-form] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:foo :bar}}
         '[] "[#?(:foo foo-form :bar bar-form)]" {:read-cond :allow :features #{:baz}}
         'nil "#?(:default nil)" opts

         ;; environmental features
         "clojure" "#?(:clj \"clojure\" :cljs \"clojurescript\" :default \"default\")"  opts

         ;; default features
         "default" "#?(:cljr \"clr\" :cljs \"cljs\" :default \"default\")" opts

         ;; splicing
         [] "[#?@(:clj [])]" opts
         [:a] "[#?@(:clj [:a])]" opts
         [:a :b] "[#?@(:clj [:a :b])]" opts
         [:a :b :c] "[#?@(:clj [:a :b :c])]" opts

         ;; nested splicing
         [:a :b :c :d :e] "[#?@(:clj [:a #?@(:clj [:b #?@(:clj [:c]) :d]):e])]" opts
         '(+ 1 (+ 2 3)) "(+ #?@(:clj [1 (+ #?@(:clj [2 3]))]))" opts
         '(+ (+ 2 3) 1) "(+ #?@(:clj [(+ #?@(:clj [2 3])) 1]))" opts
         [:a [:b [:c] :d] :e] "[#?@(:clj [:a [#?@(:clj [:b #?@(:clj [[:c]]) :d])] :e])]" opts

         ;; bypass unknown tagged literals
         [1 2 3] "#?(:cljs #js [1 2 3] :clj [1 2 3])" opts
         :clojure "#?(:foo #some.nonexistent.Record {:x 1} :clj :clojure)" opts)

    (are [re s opts] (is (thrown-with-msg? RuntimeException re (read-string opts s)))
         #"Features must be keywords" "#?((+ 1 2) :a)" opts
         #"even number of forms" "#?(:cljs :a :clj)" opts
         #"read-cond-splicing must implement" "(#?@(:clj :a))" opts
         #"is reserved" "(#?@(:foo :a :else :b))" opts
         #"must be a list" "#?[:foo :a :else :b]" opts
         #"Conditional read not allowed" "#?[:clj :a :default nil]" {:read-cond :BOGUS}
         #"Conditional read not allowed" "#?[:clj :a :default nil]" {}))
  (binding [*data-readers* {'js (fn [v] (JSValue. v) )}]
    (is (= (JSValue. [1 2 3])
           (read-string {:features #{:cljs} :read-cond :allow} "#?(:cljs #js [1 2 3] :foo #foo [1])")))))

(deftest preserve-read-cond
  (is (= 1 (binding [*data-readers* {'foo (constantly 1)}]
             (read-string {:read-cond :preserve} "#foo []"))))

  (let [x (read-string {:read-cond :preserve} "#?(:clj foo :cljs bar)")]
    (is (reader-conditional? x))
    (is (= x (reader-conditional '(:clj foo :cljs bar) false)))
    (is (not (:splicing? x)))
    (is (= :foo (get x :no-such-key :foo)))
    (is (= (:form x) '(:clj foo :cljs bar))))
  (let [x (first (read-string {:read-cond :preserve} "(#?@(:clj [foo]))"))]
    (is (reader-conditional? x))
    (is (= x (reader-conditional '(:clj [foo]) true)))
    (is (:splicing? x))
    (is (= :foo (get x :no-such-key :foo)))
    (is (= (:form x) '(:clj [foo]))))
  (is (thrown-with-msg? RuntimeException #"No reader function for tag"
                        (read-string {:read-cond :preserve} "#js {:x 1 :y 2}" )))
  (let [x (read-string {:read-cond :preserve} "#?(:cljs #js {:x 1 :y 2})")
        [platform tl] (:form x)]
    (is (reader-conditional? x))
    (is (tagged-literal? tl))
    (is (= tl (tagged-literal 'js {:x 1 :y 2})))
    (is (= 'js (:tag tl)))
    (is (= {:x 1 :y 2} (:form tl)))
    (is (= :foo (get tl :no-such-key :foo))))
  (testing "print form roundtrips"
    (doseq [s ["#?(:clj foo :cljs bar)"
               "#?(:cljs #js {:x 1, :y 2})"
               "#?(:clj #clojure.test_clojure.reader.TestRecord [42 85])"]]
      (is (= s (pr-str (read-string {:read-cond :preserve} s)))))))

(alias 'c.c 'clojure.core)

(deftest read-namespaced-map-extra-tests
  (binding [*ns* (the-ns 'typed-test.clj.reader)]
    (is (= {::foo 1} (read-string "#::{:foo 1}")))
    (is (= {::foo 1 :bar 2} (read-string "#::{:foo 1 :_/bar 2}")))
    (is (= {:a/foo 1 :bar 2} (read-string "#:a{:foo 1 :_/bar 2}")))
    (is (= {:clojure.core/foo 2} (read-string "#::c.c{:foo 2}")))))

(defn multiple-reader-variants-from-string [s filename]
  [(-> (StringReader. s)
       (LineNumberingPushbackReader.)
       (indexing-push-back-reader 1 filename))
   (-> (StringReader. s)
       (BufferedReader.)
       (indexing-push-back-reader 1 filename))])

;; commented because it depends on an edn reader which might not be implemented
;; in typed.clj.reader
;;
;; (defn first-reads-from-multiple-readers [s]
;;   (for [rdr (multiple-reader-variants-from-string s "file.edn")]
;;     (tre/read rdr)))
;; 
;; (deftest trdr-54
;;   (let [read-vals (mapcat first-reads-from-multiple-readers
;;                           ["[a\rb]" "[a\r b]" "[a \rb]"])]
;;     (doseq [pairs (partition 2 1 read-vals)]
;;       (is (= (first pairs) (second pairs))))))

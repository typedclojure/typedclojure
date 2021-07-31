;; based on clojure.tools.reader-test
(ns typed-test.clj.reader.ast
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *data-readers*])
  (:require [typed.clj.reader :as ast
             :refer [read-string+ast ast->string]]
            [clojure.test :refer [deftest is are testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]])
  (:import clojure.lang.BigInt))

(defn- ppdiff [a b]
  (pprint (diff a b)))

(defn roundtrip
  ([s] (roundtrip {} true s))
  ([opt s] (roundtrip opt true s))
  ([opt eof-error? s]
   (doseq [f (keep identity
                   [identity
                    #(str \space %)
                    #(str ";comment\n" %)
                    #(str "#!shebang\n" %)
                    ;; don't quote if expecting eof
                    (when eof-error?
                      #(str \' %))
                    #(str "#_ 1 " %)])]
     (let [s (f s)]
       (is (= s (ast->string (read-string+ast opt eof-error? s)))
           s)))))

(deftest read-forms
  (is (= {:op ::ast/symbol,
          :top-level true
          :string "a",
          :val 'a}
         (read-string+ast "a b")))
  (is (= {:op ::ast/forms,
          :top-level true
          :val 'a
          :forms [{:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "a",
                   :val 'a}]}
         (read-string+ast " a b"))))

(deftest read-keyword
  (is (= {:op ::ast/keyword
          :top-level true
          :string "foo-bar"
          :val :foo-bar}
         (read-string+ast ":foo-bar")))
  (is (= {:op ::ast/keyword
          :top-level true
          :string "foo/bar"
          :val :foo/bar}
         (read-string+ast ":foo/bar")))
  (is (= {:op ::ast/auto-keyword
          :top-level true
          :string "foo-bar"
          :val :user/foo-bar}
         (binding [*ns* (the-ns 'user)]
           (read-string+ast "::foo-bar"))))
  (is (= {:op ::ast/auto-keyword
          :top-level true
          :string "core/foo-bar"
          :val :clojure.core/foo-bar}
         (do (alias 'core 'clojure.core)
             (read-string+ast "::core/foo-bar"))))
  (is (= {:op ::ast/keyword
          :top-level true
          :string "*+!-_?"
          :val :*+!-_?}
         (read-string+ast ":*+!-_?")))
  (is (= {:op ::ast/keyword
          :top-level true
          :string "abc:def:ghi"
          :val :abc:def:ghi}
         (read-string+ast ":abc:def:ghi")))
  (is (= {:op ::ast/keyword
          :top-level true
          :string "abc.def/ghi"
          :val :abc.def/ghi}
         (read-string+ast ":abc.def/ghi")))
  (is (= {:op ::ast/keyword
          :top-level true
          :string "abc/def.ghi"
          :val :abc/def.ghi}
         (read-string+ast ":abc/def.ghi")))
  (is (= {:op ::ast/keyword
          :top-level true
          :string "abc:def/ghi:jkl.mno"
          :val :abc:def/ghi:jkl.mno}
         (read-string+ast ":abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword (:val (read-string+ast ":alphabet")))))

(defn number-test-case [v string]
  (let [m (read-string+ast string)]
    (is (= {:op ::ast/number
            :top-level true
            :string string
            :val v}
           m))
    (is (== (:val m) v))
    (roundtrip string)))

(defn check-instance [cls string]
  (let [m (read-string+ast string)]
    (is (instance? cls (:val m)))
    (roundtrip string)))

(deftest read-integer
  (number-test-case 42 "42")
  (number-test-case +42 "+42")
  (number-test-case -42 "-42")

  (number-test-case 42 "42N")
  (number-test-case +42 "+42N")
  (number-test-case -42 "-42N")

  (number-test-case 0 "0")
  (number-test-case 0N "0N")

  (number-test-case 042 "042")
  (number-test-case +042 "+042")
  (number-test-case -042 "-042")

  (number-test-case 0x42e "0x42e")
  (number-test-case +0x42e "+0x42e")
  (number-test-case -0x42e "-0x42e")

  (check-instance Long "2147483647")
  (check-instance Long "+1")
  (check-instance Long "1")
  (check-instance Long "+0")
  (check-instance Long "0")
  (check-instance Long "-0")
  (check-instance Long "-1")
  (check-instance Long "-2147483648")

  (check-instance Long "2147483648")
  (check-instance Long "-2147483649")
  (check-instance Long "9223372036854775807")
  (check-instance Long "-9223372036854775808")

  (check-instance BigInt "9223372036854775808")
  (check-instance BigInt "-9223372036854775809")
  (check-instance BigInt "10000000000000000000000000000000000000000000000000")
  (check-instance BigInt "-10000000000000000000000000000000000000000000000000"))

(deftest read-floating
  (number-test-case 42.23 "42.23")
  (number-test-case +42.23 "+42.23")
  (number-test-case -42.23 "-42.23")

  (number-test-case 42.23M "42.23M")
  (number-test-case +42.23M "+42.23M")
  (number-test-case -42.23M "-42.23M")

  (number-test-case 42.2e3 "42.2e3")
  (number-test-case +42.2e+3 "+42.2e+3")
  (number-test-case -42.2e-3 "-42.2e-3")

  (number-test-case 42.2e3M "42.2e3M")
  (number-test-case +42.2e+3M "+42.2e+3M")
  (number-test-case -42.2e-3M "-42.2e-3M")

  (check-instance Double "+1.0e+1")
  (check-instance Double "+1.e+1")
  (check-instance Double "+1e+1")

  (check-instance Double "+1.0e+1")
  (check-instance Double "+1.e+1")
  (check-instance Double "+1e+1")

  (check-instance Double "+1.0e1")
  (check-instance Double "+1.e1")
  (check-instance Double "+1e1")

  (check-instance Double "+1.0e-1")
  (check-instance Double "+1.e-1")
  (check-instance Double "+1e-1")

  (check-instance Double "1.0e+1")
  (check-instance Double "1.e+1")
  (check-instance Double "1e+1")

  (check-instance Double "1.0e-1")
  (check-instance Double "1.e-1")
  (check-instance Double "1e-1")

  (check-instance Double "-1.0e+1")
  (check-instance Double "-1.e+1")
  (check-instance Double "-1e+1")

  (check-instance Double "-1.0e1")
  (check-instance Double "-1.e1")
  (check-instance Double "-1e1")

  (check-instance Double "-1.0e-1")
  (check-instance Double "-1.e-1")
  (check-instance Double "-1e-1")

  (check-instance Double "+1.0")
  (check-instance Double "+1.")

  (check-instance Double "1.0")
  (check-instance Double "1.")

  (check-instance Double "+0.0")
  (check-instance Double "+0.")

  (check-instance Double "0.0")
  (check-instance Double "0.")

  (check-instance Double "-0.0")
  (check-instance Double "-0.")

  (check-instance Double "-1.0")
  (check-instance Double "-1.")

  (check-instance BigDecimal "9223372036854775808M")
  (check-instance BigDecimal "-9223372036854775809M")
  (check-instance BigDecimal "2147483647M")
  (check-instance BigDecimal "+1M")
  (check-instance BigDecimal "1M")
  (check-instance BigDecimal "+0M")
  (check-instance BigDecimal "0M")
  (check-instance BigDecimal "-0M")
  (check-instance BigDecimal "-1M")
  (check-instance BigDecimal "-2147483648M")

  (check-instance BigDecimal "+1.0e+1M")
  (check-instance BigDecimal "+1.e+1M")
  (check-instance BigDecimal "+1e+1M")

  (check-instance BigDecimal "+1.0e1M")
  (check-instance BigDecimal "+1.e1M")
  (check-instance BigDecimal "+1e1M")

  (check-instance BigDecimal "+1.0e-1M")
  (check-instance BigDecimal "+1.e-1M")
  (check-instance BigDecimal "+1e-1M")

  (check-instance BigDecimal "1.0e+1M")
  (check-instance BigDecimal "1.e+1M")
  (check-instance BigDecimal "1e+1M")

  (check-instance BigDecimal "1.0e1M")
  (check-instance BigDecimal "1.e1M")
  (check-instance BigDecimal "1e1M")

  (check-instance BigDecimal "1.0e-1M")
  (check-instance BigDecimal "1.e-1M")
  (check-instance BigDecimal "1e-1M")

  (check-instance BigDecimal "-1.0e+1M")
  (check-instance BigDecimal "-1.e+1M")
  (check-instance BigDecimal "-1e+1M")

  (check-instance BigDecimal "-1.0e1M")
  (check-instance BigDecimal "-1.e1M")
  (check-instance BigDecimal "-1e1M")

  (check-instance BigDecimal "-1.0e-1M")
  (check-instance BigDecimal "-1.e-1M")
  (check-instance BigDecimal "-1e-1M")

  (check-instance BigDecimal "+1.0M")
  (check-instance BigDecimal "+1.M")

  (check-instance BigDecimal "1.0M")
  (check-instance BigDecimal "1.M")

  (check-instance BigDecimal "+0.0M")
  (check-instance BigDecimal "+0.M")

  (check-instance BigDecimal "0.0M")
  (check-instance BigDecimal "0.M")

  (check-instance BigDecimal "-0.0M")
  (check-instance BigDecimal "-0.M")

  (check-instance BigDecimal "-1.0M")
  (check-instance BigDecimal "-1.M"))

(deftest read-ratio
  (number-test-case 4/2 "4/2")
  (number-test-case 4/2 "+4/2")
  (number-test-case -4/2 "-4/2"))

(defn symbol-test-case [v string]
  (let [m (read-string+ast string)]
    (is (= {:op ::ast/symbol
            :top-level true
            :string string
            :val v}
           m))
    (is (instance? clojure.lang.Symbol (:val m)))))

(defn symbolic-value-test-case [v string]
  (let [m (read-string+ast string)]
    (is (= {:op ::ast/symbolic-value
            :top-level true
            :string (subs string 2)
            :val v}
           m))
    (roundtrip string)))

(deftest read-symbol
  (symbol-test-case 'foo "foo")
  (symbol-test-case 'foo/bar "foo/bar")
  (symbol-test-case '*+!-_? "*+!-_?")
  (symbol-test-case 'abc:def:ghi "abc:def:ghi")
  (symbol-test-case 'abc.def/ghi "abc.def/ghi")
  (symbol-test-case 'abc/def.ghi "abc/def.ghi")
  (symbol-test-case 'abc:def/ghi:jkl.mno "abc:def/ghi:jkl.mno")
  (symbol-test-case 'alphabet "alphabet")
  (symbol-test-case (symbol "foo" "/") "foo//") ;; the clojure reader can't read this
  ;; workaround NaN equality
  (let [m (read-string+ast "##NaN")]
    (is (= {:op ::ast/symbolic-value
            :top-level true
            :string "NaN"}
           (dissoc m :val)))
    (is (Double/isNaN (:val m))))

  (symbolic-value-test-case Double/POSITIVE_INFINITY "##Inf")
  (symbolic-value-test-case Double/NEGATIVE_INFINITY "##-Inf"))

(deftest read-specials
  (is (= {:op ::ast/nil
          :top-level true
          :string "nil"
          :val nil}
         (read-string+ast "nil")))
  (is (= {:op ::ast/boolean
          :top-level true
          :string "false"
          :val false}
         (read-string+ast "false")))
  (is (= {:op ::ast/boolean
          :top-level true
          :string "true"
          :val true}
         (read-string+ast "true"))))

(defn character-test-case [v string]
  (let [m (read-string+ast string)]
    (is (= {:op ::ast/character
            :top-level true
            :string (subs string 1)
            :val v}
           m))
    (is (instance? Character (:val m)))
    (roundtrip string)))

(deftest read-char
  (character-test-case \f "\\f")
  (character-test-case \u0194 "\\u0194")
  (character-test-case \o123 "\\o123")
  (character-test-case \newline "\\newline")
  (character-test-case \space "\\space")
  (character-test-case \tab "\\tab")
  (character-test-case \backspace "\\backspace")
  (character-test-case \formfeed "\\formfeed")
  (character-test-case \return "\\return")
  (character-test-case (char 0) "\\o0")
  (character-test-case (char 0) "\\o000")
  (character-test-case (char 0377) "\\o377")
  (character-test-case \A "\\u0041")
  (character-test-case \@ "\\@")
  (character-test-case (char 0xd7ff) "\\ud7ff")
  (character-test-case (char 0xe000) "\\ue000")
  (character-test-case (char 0xffff) "\\uffff"))

(defn string-test-case [v string]
  (let [m (read-string+ast string)]
    (is (= {:op ::ast/string
            :top-level true
            :string (subs string 1 (dec (count string)))
            :val v}
           m))
    (is (instance? String (:val m)))
    (roundtrip string)))

(deftest read-string*
  (string-test-case "foo bar" "\"foo bar\"")
  (string-test-case "foo\\bar" "\"foo\\\\bar\"")
  (string-test-case "foo\000bar" "\"foo\\000bar\"")
  (string-test-case "foo\u0194bar" "\"foo\\u0194bar\"")
  (string-test-case "foo\123bar" "\"foo\\123bar\"")
  (string-test-case "\060" "\"\\060\"")
  ;; workaround https://github.com/jonase/eastwood/issues/413
  (string-test-case (str \o340) "\"\\340\"")
  (string-test-case (str \o377) "\"\\377\""))

(deftest read-list
  (is (= {:op ::ast/list
          :top-level true
          :val '()
          :forms []}
         (read-string+ast "()")))
  (is (seq? (:val (read-string+ast "()"))))
  (is (= {:op ::ast/list,
          :top-level true
          :val '(foo bar)
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "bar",
                   :val 'bar}]}
         (read-string+ast "(foo bar)")))
  (is (= {:op ::ast/list,
          :top-level true
          :val '(foo (bar) baz)
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/list,
                   :val '(bar)
                   :forms [{:op ::ast/symbol,
                            :string "bar",
                            :val 'bar}]}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "baz",
                   :val 'baz}]}
         (read-string+ast "(foo (bar) baz)"))))

(deftest read-vector
  (is (= {:op ::ast/vector
          :top-level true
          :val '[]
          :forms []}
         (read-string+ast "[]")))
  (is (vector? (:val (read-string+ast "[]"))))
  (is (= {:op ::ast/vector,
          :top-level true
          :val '[foo bar]
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "bar",
                   :val 'bar}]}
         (read-string+ast "[foo bar]")))
  (is (= {:op ::ast/vector,
          :top-level true
          :val '[foo [bar] baz]
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/vector,
                   :val '[bar]
                   :forms [{:op ::ast/symbol,
                            :string "bar",
                            :val 'bar}]}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "baz",
                   :val 'baz}]}
        (read-string+ast "[foo [bar] baz]"))))

(deftest read-map
  (is (= {:op ::ast/map
          :top-level true
          :val '{}
          :forms []}
         (read-string+ast "{}")))
  (is (= {:op ::ast/map
          :top-level true
          :val '{foo bar}
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "bar",
                   :val 'bar}]}
         (read-string+ast "{foo bar}")))
  (is (= {:op ::ast/map,
          :top-level true
          :val '{foo {bar baz}}
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/map,
                   :val '{bar baz}
                   :forms [{:op ::ast/symbol,
                            :string "bar",
                            :val 'bar}
                           {:op ::ast/whitespace,
                            :string " "}
                           {:op ::ast/symbol,
                            :string "baz",
                            :val 'baz}]}]}
         (read-string+ast "{foo {bar baz}}"))))

(deftest read-set
  (is (= {:op ::ast/set
          :top-level true
          :val '#{}
          :forms []}
         (read-string+ast "#{}")))
  (is (= {:op ::ast/set
          :top-level true
          :val '#{foo bar}
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "bar",
                   :val 'bar}]}
         (read-string+ast "#{foo bar}")))
  (is (= {:op ::ast/set
          :top-level true
          :val '#{foo #{bar} baz}
          :forms [{:op ::ast/symbol,
                   :string "foo",
                   :val 'foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/set
                   :val '#{bar}
                   :forms [{:op ::ast/symbol,
                            :string "bar",
                            :val 'bar}]}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/symbol,
                   :string "baz",
                   :val 'baz}]}
         (read-string+ast "#{foo #{bar} baz}"))))

(deftest read-metadata
  (is (= {:op ::ast/meta
          :top-level true
          :forms [{:op ::ast/keyword,
                   :string "foo",
                   :val :foo}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/quote
                   :val '(quote bar)
                   :forms [{:op ::ast/symbol
                            :string "bar"
                            :val 'bar}]}]
          :val '(quote bar)}
         (read-string+ast "^:foo 'bar")))
  (is (= {:foo true} (meta (:val (read-string+ast "^:foo 'bar")))))
  (is (= {:op ::ast/meta
          :top-level true
          :forms [{:op ::ast/map,
                   :forms [{:op ::ast/keyword
                            :string "foo"
                            :val :foo}
                           {:op ::ast/whitespace,
                            :string " "}
                           {:op ::ast/symbol
                            :string "bar"
                            :val 'bar}]
                   :val {:foo 'bar}}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/quote
                   :val '(quote baz)
                   :forms [{:op ::ast/symbol
                            :string "baz"
                            :val 'baz}]}]
          :val '(quote baz)}
         (read-string+ast "^{:foo bar} 'baz")))
  (is (= {:foo 'bar} (meta (:val (read-string+ast "^{:foo bar} 'baz")))))
  (is (= {:op ::ast/meta
          :top-level true
          :forms [{:op ::ast/string
                   :string "foo"
                   :val "foo"}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/quote
                   :val '(quote bar)
                   :forms [{:op ::ast/symbol
                            :string "bar"
                            :val 'bar}]}]
          :val '(quote bar)}
         (read-string+ast "^\"foo\" 'bar")))
  (is (= {:tag "foo"} (meta (:val (read-string+ast "^\"foo\" 'bar")))))
  (is (= {:op ::ast/meta
          :top-level true
          :forms [{:op ::ast/symbol
                   :string "String"
                   :val 'String}
                  {:op ::ast/whitespace,
                   :string " "}
                  {:op ::ast/quote
                   :val '(quote x)
                   :forms [{:op ::ast/symbol
                            :string "x"
                            :val 'x}]}]
          :val '(quote x)}
         (read-string+ast "^String 'x")))
  (is (= {:tag 'String} (meta (:val (read-string+ast "^String 'x"))))))

(deftest read-namespaced-map
  (is (= {:op ::ast/namespaced-map
          :top-level true
          :forms [{:op ::ast/symbol :string "foo" :val 'foo}
                  {:op ::ast/map
                   :forms [{:op ::ast/keyword
                            :string "bar"
                            :val :bar}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number
                            :string "1"
                            :val 1}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/keyword
                            :string "_/baz"
                            :val :_/baz}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number, :string "2", :val 2}]
                   :val {:bar 1, :_/baz 2}}]
          :val {:foo/bar 1, :baz 2}}
        (read-string+ast "#:foo{:bar 1 :_/baz 2}")))
  (is (= {:op ::ast/namespaced-map
          :top-level true
          :forms [{:op ::ast/symbol :string "foo" :val 'foo}
                  {:op ::ast/whitespace, :string " "}
                  {:op ::ast/map
                   :forms [{:op ::ast/keyword
                            :string "bar"
                            :val :bar}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number
                            :string "1"
                            :val 1}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/keyword
                            :string "_/baz"
                            :val :_/baz}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number, :string "2", :val 2}]
                   :val {:bar 1, :_/baz 2}}]
          :val {:foo/bar 1, :baz 2}}
        (read-string+ast "#:foo {:bar 1 :_/baz 2}")))
  (is (= {:op ::ast/namespaced-map
          :top-level true
          :forms [{:op ::ast/symbol :string "foo" :val 'foo}
                  {:op ::ast/whitespace, :string ",,"}
                  {:op ::ast/map
                   :forms [{:op ::ast/symbol
                            :string "bar"
                            :val 'bar}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number
                            :string "1"
                            :val 1}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/keyword
                            :string "_/baz"
                            :val :_/baz}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number, :string "2", :val 2}]
                   :val '{bar 1 :_/baz 2}}]
          :val '{foo/bar 1, :baz 2}}
         (read-string+ast "#:foo,,{bar 1 :_/baz 2}")))
  (is (= {:op ::ast/auto-namespaced-map
          :top-level true
          :forms [{:op ::ast/map
                   :forms [{:op ::ast/symbol
                            :string "bar"
                            :val 'bar}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number
                            :string "1"
                            :val 1}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/keyword
                            :string "_/baz"
                            :val :_/baz}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number, :string "2", :val 2}]
                   :val '{bar 1 :_/baz 2}}]
          :val '{user/bar 1, :baz 2}}
         (binding [*ns* (the-ns 'user)]
           (read-string+ast "#::{bar 1 :_/baz 2}"))))
  (is (= {:op ::ast/auto-namespaced-map
          :top-level true
          :forms [{:op ::ast/symbol
                   :string "core"
                   :val 'clojure.core}
                  {:op ::ast/map
                   :forms [{:op ::ast/symbol
                            :string "bar"
                            :val 'bar}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number
                            :string "1"
                            :val 1}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/keyword
                            :string "_/baz"
                            :val :_/baz}
                           {:op ::ast/whitespace, :string " "}
                           {:op ::ast/number, :string "2", :val 2}]
                   :val '{bar 1 :_/baz 2}}]
          :val '{clojure.core/bar 1, :baz 2}}
         (binding [*ns* (the-ns 'user)]
           (alias 'core 'clojure.core)
           (read-string+ast "#::core{bar 1 :_/baz 2}")))))

(deftest read-comment
  (is (= {:op ::ast/comment
          :top-level true
          :string "asdf"
          :eof true}
         (read-string+ast {} false ";asdf")))
  (is (= {:op ::ast/forms
          :top-level true
          :eof true
          :forms [{:op ::ast/comment
                   :string "asdf\n"}
                  {:op ::ast/forms
                   :eof true
                   :forms []}]}
         (read-string+ast {} false ";asdf\n")))
  (is (= {:op ::ast/comment
          :top-level true
          :eof true
          :string "asdf\\n"}
         (read-string+ast {} false ";asdf\\n")))
  (is (= {:op ::ast/forms
          :top-level true
          :eof true
          :forms [{:op ::ast/comment
                   :string "asdf\n"}
                  {:op ::ast/whitespace
                   :string "\n"
                   :eof true}]}
         (read-string+ast {} false ";asdf\n\n")))
  (is (= {:op ::ast/vector
          :top-level true
          :forms [{:op ::ast/whitespace
                   :string " "}
                  {:op ::ast/comment
                   :string "asdf\n"}
                  {:op ::ast/number
                   :string "1"
                   :val 1}]
          :val [1]}
         (read-string+ast "[ ;asdf\n1]"))))

(deftest read-discard-test
  (is (= {:op ::ast/forms
          :top-level true
          :forms [{:op ::ast/discard
                   :forms [{:op ::ast/whitespace, :string " "}
                           {:op ::ast/number
                            :string "1"
                            :val 1}]}
                  {:op ::ast/whitespace, :string " "}
                  {:op ::ast/number
                   :string "2"
                   :val 2}]
          :val 2}
         (read-string+ast "#_ 1 2")))
  (is (= {:op ::ast/forms
          :top-level true
          :forms [{:op ::ast/discard
                   :forms [{:op ::ast/number
                            :string "1"
                            :val 1}]}
                  {:op ::ast/forms
                   :forms []
                   :eof true}]
          :eof true}
        (read-string+ast {} false "#_1"))))

(deftest read-syntax-quote-test
  (is (= {:op ::ast/syntax-quote
          :top-level true
          :forms [{:op ::ast/symbol
                   :string "a"
                   :val 'a}]
          :val '(quote user/a)}
         (binding [*ns* (the-ns 'user)]
           (read-string+ast "`a"))))
  (is (= {:op ::ast/syntax-quote
          :top-level true
          :forms [{:op ::ast/list
                   :forms [{:op ::ast/unquote-splicing
                            :forms [{:op ::ast/vector
                                     :forms [{:op ::ast/symbol
                                              :string "a"
                                              :val 'a}]
                                     :val '[a]}]
                            :val '~@[a]}
                           {:op ::ast/whitespace
                            :string " "}
                           {:op ::ast/unquote
                            :forms [{:op ::ast/number
                                     :string "1"
                                     :val 1}]
                            :val '~1}]
                   :val '(~@[a] ~1)}]
          :val '(clojure.core/sequence (clojure.core/seq (clojure.core/concat [a] (clojure.core/list 1))))}
         (binding [*ns* (the-ns 'user)]
           (read-string+ast "`(~@[a] ~1)")))))

(defn regex-test-case [e s]
  (let [{:keys [val] :as m} (read-string+ast s)]
    (is (= (dissoc e :val)
           (dissoc m :val)))
    (is (instance? java.util.regex.Pattern val))
    (is (= (str val)
           (str (:val m))))
    (roundtrip s)))

(deftest read-regex-test
  (regex-test-case {:op ::ast/regex
                    :top-level true
                    :string "a"
                    :val #"a"}
                   "#\"a\"")
  (regex-test-case {:op ::ast/regex
                    :top-level true
                    :string "a \n \n"
                    :val #"a"}
                   "#\"a \n \n\""))

(defn cond-test-case [opt e string]
  (let [m (read-string+ast opt string)]
    (is (= e m)
        (with-out-str (ppdiff e m)))
    (roundtrip opt string)))

(deftest read-cond-test
  (cond-test-case {:read-cond :allow :features #{:clj}}
                  {:op ::ast/cond
                   :top-level true
                   :forms [{:op ::ast/list
                            :val '(:clj 1)
                            :forms [{:op ::ast/keyword
                                     :matched-feature-key true
                                     :string "clj"
                                     :val :clj}
                                    {:op ::ast/whitespace :string " "}
                                    {:op ::ast/number
                                     :matched-feature-val true
                                     :string "1"
                                     :val 1}]}]
                   :val 1}
                  "#?(:clj 1)")
  (cond-test-case {:read-cond :allow :features #{:clj}}
                  {:op ::ast/cond
                   :top-level true
                   :forms [{:op ::ast/list
                            :val '(:foo 2 :clj 1)
                            :forms [{:op ::ast/keyword
                                     :string "foo"
                                     :val :foo}
                                    {:op ::ast/whitespace :string " "}
                                    {:op ::ast/number
                                     :string "2"
                                     :val 2}
                                    {:op ::ast/whitespace :string " "}
                                    {:op ::ast/keyword
                                     :matched-feature-key true
                                     :string "clj"
                                     :val :clj}
                                    {:op ::ast/whitespace :string " "}
                                    {:op ::ast/number
                                     :matched-feature-val true
                                     :string "1"
                                     :val 1}]}]
                   :val 1}
                  "#?(:foo 2 :clj 1)")
  (cond-test-case {:read-cond :allow :features #{:clj}}
                  {:op ::ast/cond
                   :top-level true
                   :forms [{:op ::ast/whitespace :string " "}
                           {:op ::ast/list
                            :val '(:clj 1)
                            :forms [{:op ::ast/keyword
                                     :matched-feature-key true
                                     :string "clj"
                                     :val :clj}
                                    {:op ::ast/whitespace :string " "}
                                    {:op ::ast/number
                                     :matched-feature-val true
                                     :string "1"
                                     :val 1}]}]
                   :val 1}
                  "#? (:clj 1)")
  (cond-test-case {:read-cond :allow :features #{:clj}}
                  {:op ::ast/list
                   :top-level true
                   :forms [{:op ::ast/cond-splicing
                            :forms [{:op ::ast/whitespace
                                     :string " "}
                                    {:op ::ast/list
                                     :forms [{:op ::ast/keyword
                                              :matched-feature-key true
                                              :string "clj"
                                              :val :clj}
                                             {:op ::ast/whitespace :string " "}
                                             {:op ::ast/vector
                                              :matched-feature-val true
                                              :val [1]
                                              :forms [{:op ::ast/number
                                                       :string "1"
                                                       :val 1}]}]
                                     :val '(:clj [1])}]
                            :val [1]}]
                   :val '(1)}
                  "(#?@ (:clj [1]))"))


(deftest ast->string-test
  (roundtrip "1")
  (roundtrip ":a")
  (roundtrip ":a/b")
  (roundtrip "::b")
  (do (alias 'core 'clojure.core)
      (roundtrip "::core/b"))
  (roundtrip "a")
  (do (alias 'core 'clojure.core)
      (roundtrip "core/a"))
  (roundtrip "clojure.core/a")
  (roundtrip "blah/a")
  (roundtrip "{}")
  (roundtrip "{:a 1}")
  (roundtrip "{:a     1}")
  (roundtrip "{:a     1, :b 2, 1M 2}")
  (roundtrip "[]")
  (roundtrip "[:a 1]")
  (roundtrip "[:a     1]")
  (roundtrip "[:a     1, :b 2, 1M 2]")
  (roundtrip "()")
  (roundtrip "(:a 1)")
  (roundtrip "(:a     1)")
  (roundtrip "(:a     1, :b 2, 1M 2)")
  (roundtrip "nil")
  (roundtrip "true")
  (roundtrip "false")
  (roundtrip "/")
  (roundtrip "[/]")
  (roundtrip "#_1 2")
  (roundtrip {} false "#_1")
  (roundtrip "^:a 'a")
  (roundtrip "^:a a")
  (roundtrip "^\"a\" a")
  (roundtrip "^String a")
  (roundtrip "^{:a 1} a")
  (roundtrip "@a")
  (roundtrip "~a")
  (roundtrip "~@a")
  (roundtrip "`a")
  (roundtrip "`(~@a)")
  (roundtrip "#\"a\"")
  (roundtrip "#\"a \n \n [1]\"")
  (roundtrip "\\c")
  (roundtrip "\\u1232")
  (roundtrip "\\newline")
  (roundtrip "\\space")
  (roundtrip "\\tab")
  (roundtrip "\\backspace")
  (roundtrip "\\return")
  (roundtrip "\\o343")
  (roundtrip "#{}")
  (roundtrip "#{1 2}")
  (roundtrip "#{1 #{2}}")
  (roundtrip "#{1 {2 1 4 2} (1 [4 2])}")
  (roundtrip {:read-cond :allow :features #{:clj}} "#?(:clj 1)")
  (roundtrip {:read-cond :allow :features #{:clj}} "#? (:clj 1)")
  (roundtrip "#(a)")
  (roundtrip "#(%)")
  (roundtrip "#(%1 %2)")
  (roundtrip "#(%1 %&)")
  (roundtrip "#:a{}")
  (roundtrip "#:a{a 1}")
  (roundtrip "#:a{:a 1}")
  (roundtrip "#:a{:a 1 :_/b 2}")
  (roundtrip "#::{}")
  (roundtrip "#::{a 1}")
  (roundtrip "#::{:a 1}")
  (roundtrip "#::{:a 1 :_/b 2}")
  (roundtrip "#inst \"2010-11-12T13:14:15.666-00:00\"")
  (do (alias 'core 'clojure.core)
      (roundtrip "#::core{:a 1 :_/b 2}"))
  (roundtrip "#'a")
  ;; shebang
  (roundtrip "[1 #!\n2]")
  )

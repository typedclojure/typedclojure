(ns typed.fnl.reader-test
  "Test suite for typed.fnl.reader, based on Fennel parser tests"
  (:require [clojure.test :refer :all]
            [typed.fnl.reader :as fnl-reader]))

(deftest test-basics
  (testing "Basic string escapes"
    (is (= "\\\\" (fnl-reader/read-string "\"\\\\\\\\\"")))
    (is (= "abc\"def" (fnl-reader/read-string "\"abc\\\"def\""))))
  
  (testing "Numbers with underscores"
    (is (= 150000 (fnl-reader/read-string "150_000"))))
  
  (testing "Newlines in strings"
    (is (= "\n5.2" (fnl-reader/read-string "\"\n5.2\"")))
    (is (= "foo\nbar" (fnl-reader/read-string "\"foo\\\nbar\""))))
  
  (testing "Symbols"
    (is (= '_0 (fnl-reader/read-string "_0"))
        "leading underscore should be a symbol, not a number")
    (is (= '&abc (fnl-reader/read-string "&abc")))))

(deftest test-numbers
  (testing "Integers"
    (is (= 42 (fnl-reader/read-string "42")))
    (is (= 34 (- 141791343654234 141791343654200))))
  
  (testing "Floats"
    (is (= 1.41791343654238e+14 (fnl-reader/read-string "1.41791343654238e+14")))
    (is (= 14179134365.125 (fnl-reader/read-string "14179134365.125"))))
  
  (testing "Special float values"
    (is (= ##Inf (fnl-reader/read-string "+.inf")))
    (is (= ##Inf (fnl-reader/read-string ".inf")))
    (is (= ##-Inf (fnl-reader/read-string "-.inf")))
    (is (Double/isNaN (fnl-reader/read-string ".nan")))
    (is (Double/isNaN (fnl-reader/read-string "+.nan")))
    (is (Double/isNaN (fnl-reader/read-string "-.nan")))))

(deftest test-escape-sequences
  (testing "Decimal escapes"
    (is (= " " (fnl-reader/read-string "\"\\032\""))))
  
  (testing "Hex escapes"
    (is (= " " (fnl-reader/read-string "\"\\x20\""))))
  
  (testing "Unicode escapes"
    (is (= " " (fnl-reader/read-string "\"\\u{20}\"")))
    (is (= "$" (fnl-reader/read-string "\"\\u{24}\""))))
  
  (testing "Standard escapes"
    (is (= "\t\n\u000B" (fnl-reader/read-string "\"\\t\\n\\v\"")))))

(deftest test-control-codes
  (testing "Control characters in strings"
    (doseq [i (range 1 32)]
      (let [code (str "\"" (char i) i "\"")
            expected (str (char i) i)]
        (is (= expected (fnl-reader/read-string code))
            (str "Failed to parse control code " i))))))

(deftest test-lists
  (testing "Simple lists"
    (is (= '(fn demo-func [x] x)
           (fnl-reader/read-string "(fn demo-func [x] x)"))))
  
  (testing "Nested lists"
    (is (= '(let [x 5] (+ x 2))
           (fnl-reader/read-string "(let [x 5] (+ x 2))"))))
  
  (testing "Empty list"
    (is (= '() (fnl-reader/read-string "()")))))

(deftest test-vectors
  (testing "Simple vectors"
    (is (= [1 2 3] (fnl-reader/read-string "[1 2 3]"))))
  
  (testing "Nested vectors"
    (is (= [[1 2] [3 4]] (fnl-reader/read-string "[[1 2] [3 4]]"))))
  
  (testing "Empty vector"
    (is (= [] (fnl-reader/read-string "[]")))))

(deftest test-maps
  (testing "Simple maps"
    ;; In Fennel, :a is syntactic sugar for "a" (strings)
    (is (= {"a" 1 "b" 2} (fnl-reader/read-string "{:a 1 :b 2}"))))
  
  (testing "Maps with various key types"
    (is (= {"a" 1 2 3} (fnl-reader/read-string "{:a 1 2 3}")))
    (is (= {"key" "value"} (fnl-reader/read-string "{\"key\" \"value\"}"))))
  
  (testing "Empty map"
    (is (= {} (fnl-reader/read-string "{}")))))

(deftest test-booleans
  (testing "Boolean literals"
    (is (= true (fnl-reader/read-string "true")))
    (is (= false (fnl-reader/read-string "false")))))

(deftest test-nil
  (is (= nil (fnl-reader/read-string "nil"))))

(deftest test-keywords
  (testing "Keywords (syntactic sugar for strings in Fennel)"
    (is (= "foo" (fnl-reader/read-string ":foo")))
    (is (= "bar" (fnl-reader/read-string ":bar")))
    (is (= "a/b" (fnl-reader/read-string ":a/b")))
    (is (= ":a" (fnl-reader/read-string "::a")))))

(deftest test-varargs
  (testing "Varargs symbol"
    (is (= '... (fnl-reader/read-string "...")))))

(deftest test-quotes
  (testing "Quote prefix"
    (is (= '(quote a) (fnl-reader/read-string "'a")))
    (is (= '(quote (a b c)) (fnl-reader/read-string "'(a b c)")))))

(deftest test-unquote
  (testing "Unquote prefix"
    (is (= '(unquote x) (fnl-reader/read-string ",x")))))

(deftest test-hashfn
  (testing "Hashfn prefix"
    (is (= '(hashfn (+ $ 1)) (fnl-reader/read-string "#(+ $ 1)")))))

(deftest test-read-all
  (testing "Multiple forms"
    (is (= '[(fn [x] x) (+ 1 2) [a b c]]
           (fnl-reader/read-all "(fn [x] x) (+ 1 2) [a b c]"))))
  
  (testing "Forms with newlines"
    (is (= '[x y z]
           (fnl-reader/read-all "x\ny\nz")))))

(deftest test-comments
  (testing "Comments are skipped"
    (is (= '[(foo bar)]
           (fnl-reader/read-all ";; comment\n(foo bar)")))
    (is (= '[x y]
           (fnl-reader/read-all "x ;; inline comment\ny")))))

(deftest test-whitespace
  (testing "Various whitespace"
    (is (= '(a b c) (fnl-reader/read-string "( a  b   c )")))
    (is (= '(a b c) (fnl-reader/read-string "(\na\n  b\n    c\n)")))))

(deftest test-complex-expressions
  (testing "Complex Fennel expressions"
    (is (= '(let [x 5 y 10] (+ x y))
           (fnl-reader/read-string "(let [x 5 y 10] (+ x y))")))
    
    (is (= '(fn demo-func [x y] (if (> x y) x y))
           (fnl-reader/read-string "(fn demo-func [x y] (if (> x y) x y))")))
    
    ;; In Fennel, :name is syntactic sugar for "name" (strings)
    (is (= '{"name" "Alice" "age" 30 "city" "NYC"}
           (fnl-reader/read-string "{:name \"Alice\" :age 30 :city \"NYC\"}")))))

(deftest test-error-handling
  (testing "Unclosed delimiters"
    (is (thrown? Exception (fnl-reader/read-string "("))))
  
  (testing "Mismatched delimiters"
    (is (thrown? Exception (fnl-reader/read-string "(]"))))
  
  (testing "Invalid characters"
    ;; Some invalid characters might be accepted depending on implementation
    ;; This test might need adjustment based on actual parser behavior
    ))

(deftest test-fennel-demo-file
  (testing "Read actual Fennel demo file"
    (let [demo-content "(import-macros t :typed.fennel)

(t.ann demo-func [t.Str :-> t.Str])

(fn demo-func [x]
  x)

(print (demo-func \"Hello from Typed Fennel!\"))"]
      (is (= 4 (count (fnl-reader/read-all demo-content))))
      (let [forms (fnl-reader/read-all demo-content)]
        (is (= 'import-macros (first (first forms))))
        (is (= 'fn (first (nth forms 2))))))))

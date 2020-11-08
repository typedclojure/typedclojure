;; based on clojure.tools.metadata-test
(ns typed-test.clj.reader.metadata-test
  (:refer-clojure :exclude [read *default-data-reader-fn* read-string])
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [typed.clj.reader :refer [read+ast]]
            [typed.clj.reader.reader-types :as reader-types])
  (:import java.nio.charset.Charset
           (java.io StringReader BufferedReader)
           clojure.lang.LineNumberingPushbackReader))

(defn compare-forms-with-meta [expected-form actual-form]
  (let [comparisons (map vector (tree-seq coll? identity expected-form)
                         (tree-seq coll? identity actual-form))]
    (doseq [[expected actual] comparisons]
      (is (= [expected (meta expected)] [actual (meta actual)])))))

(def test-contents
  "Contents of a file stream for testing."
  "(ns clojure.tools.reader.haiku)\n\n(defn haiku
    \"It will read the form
    but will the form metadata be
    or never become?\"
    [first-five middle-seven last-five]
    (- (apply +
              ^{:last last-five} [1 2 3])
       first-five middle-seven))")

(defn test-reader
  "Return a fresh byte array input stream reading off test-bytes"
  [test-contents]
  (StringReader. test-contents))

(defn replace-newlines [s replacement]
  (str/replace s "\n" replacement))

(def expected-haiku-ns
  (with-meta  '(^{:line 1 :column 2 :end-line 1 :end-column 4 :file "haiku.clj"} ns
                ^{:line 1 :column 5 :end-line 1 :end-column 31 :file "haiku.clj"} clojure.tools.reader.haiku)
    {:line 1 :column 1 :end-line 1 :end-column 32 :file "haiku.clj"}))

(def expected-haiku-defn
  (with-meta (list
              '^{:line 3 :column 2 :end-line 3 :end-column 6 :file "haiku.clj"} defn
              '^{:line 3 :column 7 :end-line 3 :end-column 12 :file "haiku.clj"} haiku
              "It will read the form\n    but will the form metadata be\n    or never become?"
              (with-meta ['^{:line 7 :column 6 :end-line 7 :end-column 16 :file "haiku.clj"} first-five
                          '^{:line 7 :column 17 :end-line 7 :end-column 29 :file "haiku.clj"} middle-seven
                          '^{:line 7 :column 30 :end-line 7 :end-column 39 :file "haiku.clj"} last-five]
                {:line 7 :column 5 :end-line 7 :end-column 40 :file "haiku.clj"})
              (with-meta (list '^{:line 8 :column 6 :end-line 8, :end-column 7 :file "haiku.clj"} -
                               (with-meta (list '^{:line 8 :column 9 :end-line 8 :end-column 14 :file "haiku.clj"} apply
                                                '^{:line 8 :column 15 :end-line 8 :end-column 16 :file "haiku.clj"} +
                                                ^{:last 'last-five :line 9 :column 34 :end-line 9 :end-column 41 :file "haiku.clj"}
                                                [1 2 3])
                                 {:line 8 :column 8 :end-line 9 :end-column 42 :file "haiku.clj"})
                               '^{:line 10 :column 8 :end-line 10 :end-column 18 :file "haiku.clj"} first-five
                               '^{:line 10 :column 19 :end-line 10 :end-column 31 :file "haiku.clj"} middle-seven)
                {:line 8 :column 5 :end-line 10 :end-column 32 :file "haiku.clj"}))
    {:line 3 :column 1 :end-line 10 :end-column 33 :file "haiku.clj"}))

(defn multiple-reader-variants-from-string [s filename]
  [(-> (test-reader s)
       (LineNumberingPushbackReader.)
       (reader-types/indexing-push-back-reader 1 filename))
   (-> (test-reader s)
       (BufferedReader.)
       (reader-types/indexing-push-back-reader 1 filename))])

(defn read-metadata-helper [reader]
  (let [{first-form :val} (read+ast reader)
        {second-form :val} (read+ast reader)
        {third-form :val :as third} (read+ast {} false reader)]
    (is (= {:line 1 :column 1 :end-line 1 :end-column 32 :file "haiku.clj"} (meta first-form)))
    (compare-forms-with-meta expected-haiku-ns first-form)
    (compare-forms-with-meta expected-haiku-defn second-form)
    (is (:eof third))))

(deftest read-metadata
  (doseq [s [test-contents
             (replace-newlines test-contents "\r")
             (replace-newlines test-contents "\r\n")]
          rdr (multiple-reader-variants-from-string s "haiku.clj")]
    (read-metadata-helper rdr)))

(def expected-haiku-ns-with-source
  (with-meta  '(^{:line 1 :column 2 :end-line 1 :end-column 4 :source "ns" :file "haiku.clj"} ns
                ^{:line 1 :column 5 :end-line 1 :end-column 31 :source "clojure.tools.reader.haiku" :file "haiku.clj"} clojure.tools.reader.haiku)
    {:line 1 :column 1 :end-line 1 :end-column 32 :source "(ns clojure.tools.reader.haiku)" :file "haiku.clj"}))

(def expected-haiku-defn-with-source
  (with-meta (list
              '^{:line 3 :column 2 :end-line 3 :end-column 6 :source "defn" :file "haiku.clj"} defn
              '^{:line 3 :column 7 :end-line 3 :end-column 12 :source "haiku" :file "haiku.clj"} haiku
              "It will read the form\n    but will the form metadata be\n    or never become?"
              (with-meta ['^{:line 7 :column 6 :end-line 7 :end-column 16 :source "first-five" :file "haiku.clj"} first-five
                          '^{:line 7 :column 17 :end-line 7 :end-column 29 :source "middle-seven" :file "haiku.clj"} middle-seven
                          '^{:line 7 :column 30 :end-line 7 :end-column 39 :source "last-five" :file "haiku.clj"} last-five]
                {:line 7 :column 5 :end-line 7 :end-column 40 :source "[first-five middle-seven last-five]" :file "haiku.clj"})
              (with-meta (list '^{:line 8 :column 6 :end-line 8, :end-column 7 :source "-" :file "haiku.clj"} -
                               (with-meta (list '^{:line 8 :column 9 :end-line 8 :end-column 14 :source "apply" :file "haiku.clj"} apply
                                                '^{:line 8 :column 15 :end-line 8 :end-column 16 :source "+" :file "haiku.clj"} +
                                                ^{:last 'last-five :line 9 :column 34 :end-line 9 :end-column 41 :source "^{:last last-five} [1 2 3]" :file "haiku.clj"}
                                                [1 2 3])
                                 {:line 8 :column 8 :end-line 9 :end-column 42 :source "(apply +
              ^{:last last-five} [1 2 3])" :file "haiku.clj"})
                               '^{:line 10 :column 8 :end-line 10 :end-column 18 :source "first-five" :file "haiku.clj"} first-five
                               '^{:line 10 :column 19 :end-line 10 :end-column 31 :source "middle-seven" :file "haiku.clj"} middle-seven)
                {:line 8 :column 5 :end-line 10 :end-column 32 :source "(- (apply +
              ^{:last last-five} [1 2 3])
       first-five middle-seven)" :file "haiku.clj"}))
    {:line 3 :column 1 :end-line 10 :end-column 33 :source "(defn haiku
    \"It will read the form
    but will the form metadata be
    or never become?\"
    [first-five middle-seven last-five]
    (- (apply +
              ^{:last last-five} [1 2 3])
       first-five middle-seven))" :file "haiku.clj"}))

(defn read-metadata-with-source-helper [rdr]
  (let [reader (-> rdr
                   (LineNumberingPushbackReader.)
                   (reader-types/source-logging-push-back-reader 1 "haiku.clj"))
        {first-form :val} (read+ast reader)
        {second-form :val} (read+ast reader)]
    (is (= {:line 1 :column 1 :end-line 1 :end-column 32 :source "(ns clojure.tools.reader.haiku)" :file "haiku.clj"} (meta first-form)))
    (compare-forms-with-meta expected-haiku-ns-with-source first-form)
    (compare-forms-with-meta expected-haiku-defn-with-source second-form)))

(deftest read-metadata-with-source
  (doseq [s [test-contents
             (replace-newlines test-contents "\r")
             (replace-newlines test-contents "\r\n")]]
    (read-metadata-with-source-helper (test-reader s))))

(comment
  (require 'clojure.pprint)
  (doseq [s ["[first-five]"
             "^{:last last-five} [1 2 3]"
             "^{} []"
             test-contents]]
    (let [reader (-> s
                     test-reader
                     LineNumberingPushbackReader.
                     (reader-types/source-logging-push-back-reader 1 "haiku.clj"))
          {:keys [val]} (read+ast reader)]
      (binding [*print-meta* true]
        (clojure.pprint/pprint val)))))

(def test2-contents
  (str/join "\n"
            ["[ +42 -42 0N +042 +0x42e -0x42e -36rCRAZY -42.2e-3M 0.314e+1"
             "  true false :kw :ns/kw 'foo/bar nil"
             "  \\f \\u0194 \\newline \\o377 \\ud7ff "
             " () [7] #{8 9} '^{:meta []} bar  "
             ;;" () [7] #{8 9}                   "
             "  #inst \"2010-11-12T13:14:15.666\""
             " ]"]))

(def expected-vector
  (with-meta
    (vector
     42 -42 0N 34 1070 -1070 -21429358 -0.0422M 3.14
     true false :kw :ns/kw
     (list
      'quote
      (with-meta
        'foo/bar
        {:line 2, :column 26, :end-line 2, :end-column 33, :file "vector.clj"}))
     nil
     \f \Ɣ \newline \ÿ \퟿
     (with-meta
       '()
       {:line 4, :column 2, :end-line 4, :end-column 4, :file "vector.clj"})
     '^{:line 4, :column 5, :end-line 4, :end-column 8, :file "vector.clj"} [7]
     '^{:line 4, :column 9, :end-line 4, :end-column 15, :file "vector.clj"} #{9 8}
     ^{:source "'^{:meta []} bar"}
     (list
      'quote
      (with-meta
        'bar
        {:meta
         ^{:line 4, :column 25, :end-line 4, :end-column 27, :file "vector.clj"}
         [],
         :line 4, :column 29, :end-line 4, :end-column 32, :file "vector.clj"}))
     (clojure.core/read-string "#inst \"2010-11-12T13:14:15.666-00:00\""))
    {:line 1 :column 1 :end-line 6 :end-column 3 :file "vector.clj"}))

(deftest read-metadata2
  (let [reader (-> (StringReader. test2-contents)
                   (LineNumberingPushbackReader.)
                   (reader-types/indexing-push-back-reader 1 "vector.clj"))
        {first-form :val} (read+ast reader)]
    (compare-forms-with-meta expected-vector first-form)))

(defn test-string [n linesep]
  (apply str (concat ["a "] (repeat n linesep) [" b"])))

(deftest many-consecutive-lineseps
  ;; With older versions of tools.reader, consecutive-lineseps of
  ;; 10,000, linesep "\r", and one of the variants of reader, would
  ;; cause a StackOverflowError exception.
  (doseq [consecutive-lineseps [1 10 10000]
          linesep ["\n" "\r" "\r\n"]
          reader (multiple-reader-variants-from-string
                  (test-string consecutive-lineseps linesep) "foo.clj")]
    (let [{first-form :val} (read+ast reader)
          {second-form :val} (read+ast reader)
          {third-form :val :as third} (read+ast {} false reader)]
      (is (= {:line 1 :column 1 :end-line 1 :end-column 2 :file "foo.clj"} (meta first-form)))
      (is (= {:line (inc consecutive-lineseps) :column 2
              :end-line (inc consecutive-lineseps) :end-column 3 :file "foo.clj"}
             (meta second-form)))
      (is (:eof third)))))

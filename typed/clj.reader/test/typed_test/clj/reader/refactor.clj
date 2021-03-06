(ns typed-test.clj.reader.refactor
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *data-readers*])
  (:require [typed.clj.reader :as ast
             :refer [read+ast read-string+ast ast->string]]
            [typed.clj.reader.reader-types :refer [source-logging-push-back-reader]]
            [clojure.test :refer [deftest is are testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.data :refer [diff]]
            [clojure.walk :as walk :refer [prewalk]])
  (:import clojure.lang.BigInt))

(defn reverse-vector [s]
  (let [ast (read-string+ast s)]
    (ast->string
      (case (:op ast)
        ::ast/vector (update ast :forms (comp vec rseq))
        ast))))

(deftest reverse-vector-test
  (is (= (reverse-vector "[1 2]")
         "[2 1]"))
  (is (= (reverse-vector "[1 #_1.5 2 3]")
         "[3 2 #_1.5 1]"))
  (is (= (reverse-vector "[1    2 3 4 5]")
         "[5 4 3 2    1]"))
  ;; interesting, newline is part of comment so the preceding
  ;; newline is swapped
  (is (= (reverse-vector "[1\n;comment\n2]")
         "[2;comment\n\n1]")))

(defn lt->gt [s]
  (let [ast (read-string+ast s)]
    (ast->string
      (case (:op ast)
        ::ast/list (let [forms (:forms ast)
                         nforms (count forms)
                         [op-pos & args-poss] (keep #(when (contains? (nth forms %) :val)
                                                       %)
                                                    (range (count forms)))
                         op-ast (nth forms op-pos)]
                     (assoc ast :forms (into [(let [rsym (case (:op op-ast)
                                                           ::ast/symbol
                                                           (case (:val op-ast)
                                                             < '>
                                                             > '<
                                                             <= '>=
                                                             >= '<=))]
                                                {:op ::ast/symbol
                                                 :string (name rsym)
                                                 :val rsym})]
                                             (let [after-op-pos (inc op-pos)
                                                   arg1-pos (first args-poss)]
                                               (concat
                                                 (when (< after-op-pos nforms)
                                                   (subvec forms after-op-pos (or arg1-pos nforms)))
                                                 (when arg1-pos
                                                   (rseq (subvec forms arg1-pos))))))))))))

(deftest lt->gt-test
  (is (= (lt->gt "(> 1 2)")
         "(< 2 1)"))
  (is (= (lt->gt "(< 1 2)")
         "(> 2 1)"))
  (is (= (lt->gt "(<= 1 2)")
         "(>= 2 1)"))
  (is (= (lt->gt "(>= 1 2)")
         "(<= 2 1)")))

;; TODO
(def prewalk-ast prewalk)
;; TODO
(def postwalk-ast walk/postwalk)

(defn indent-lines [ast line-prefix]
  (prewalk-ast (fn [v]
                 (cond-> v
                   (and (map? v)
                        (#{::ast/whitespace} (:op v)))
                   (update :string str/replace (str \newline) (str \newline line-prefix))))
               ast))

; (map f c) => (sequence (map f) c)
(defn map-seq->xform [s]
  (let [ast (read+ast {:read-cond :allow :features #{:clj}}
                      (source-logging-push-back-reader s))]
    (ast->string
      (postwalk-ast
        (fn [ast]
          (case (:op ast)
            ::ast/list (let [forms (:forms ast)
                             [op-pos & args-poss] (keep #(when (contains? (nth forms %) :val)
                                                           %)
                                                        (range (count forms)))
                             op-ast (nth forms op-pos)]
                         (cond
                           (and (#{::ast/symbol} (:op op-ast))
                                (= 'map (:val op-ast)))
                           (let [{:keys [line end-line
                                         column end-column]} (:pos ast)]
                             (assert (= 2 (count args-poss))
                                     [forms args-poss])
                             {:op ::ast/list
                              :forms (into
                                       [{:op ::ast/symbol
                                         :string "sequence"
                                         :val 'sequence}
                                        {:op ::ast/whitespace
                                         :string (if (= line end-line)
                                                   " "
                                                   (apply str "\n"
                                                          (repeat (inc column) " ")))}
                                        (indent-lines
                                          {:op ::ast/list
                                           :forms (subvec forms 0 (inc (first args-poss)))}
                                          "  ")]
                                       (subvec forms (inc (first args-poss))))})
                           :else ast))
            ast))
        ast))))

(deftest map-seq->xform-test
  (is (= "(sequence (map identity) [1 2 3])"
         (map-seq->xform "(map identity [1 2 3])")))
  (is (= "(sequence (map #(+ 1 2)) [1 2 3])"
         (map-seq->xform "(map #(+ 1 2) [1 2 3])")))
  (is (= "(sequence (map #?(:clj identity :cljs inc)) [1 2 3])"
         (map-seq->xform "(map #?(:clj identity :cljs inc) [1 2 3])")))
  (is (= (str/join
           "\n"
           ["(sequence"
            "  (map"
            "    identity)"
            "  [1 2 3])"])
         (map-seq->xform (str/join
                           "\n"
                           ["(map"
                            "  identity"
                            "  [1 2 3])"]))))
  (is (= (str/join
           "\n"
           ["(do"
            "  (sequence"
            "    (map"
            "      identity)"
            "    [1 2 3]))"])
         (map-seq->xform (str/join
                           "\n"
                           ["(do"
                            "  (map"
                            "    identity"
                            "    [1 2 3]))"]))))
  (is (= (str/join
           "\n"
           ["(sequence"
            "  (map"
            "    (if 1"
            "      identity"
            "      inc))"
            "  [1 2 3])"])
         (map-seq->xform (str/join
                           "\n"
                           ["(map"
                            "  (if 1"
                            "    identity"
                            "    inc)"
                            "  [1 2 3])"])))))

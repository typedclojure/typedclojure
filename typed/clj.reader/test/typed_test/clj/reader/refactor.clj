(ns typed-test.clj.reader.refactor
  (:refer-clojure :exclude [read read-string *default-data-reader-fn* *data-readers*])
  (:require [typed.clj.reader :as ast
             :refer [read-string+ast ast->string]]
            [clojure.test :refer [deftest is are testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.data :refer [diff]])
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
         "[5    4 3 2 1]"))
  (is (= (reverse-vector "[1\n;comment\n2]")
         "[2\n;comment\n1]")))

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

; (map f c) => (sequence (map f) c)
(defn map-seq->xform [s]
  (let [ast (read-string+ast {:read-cond :allow :features #{:clj}} s)]
    (ast->string
      (case (:op ast)
        ::ast/list (let [forms (:forms ast)
                         nforms (count forms)
                         [op-pos & args-poss] (keep #(when (contains? (nth forms %) :val)
                                                       %)
                                                    (range (count forms)))
                         op-ast (nth forms op-pos)]
                     (assert (= 2 (count args-poss))
                             args-poss)
                     {:op ::ast/list
                      :forms (into
                               [{:op ::ast/symbol
                                 :string "sequence"
                                 :val 'sequence}
                                {:op ::ast/whitespace
                                 :string " "}
                                {:op ::ast/list
                                 :forms (subvec forms 0 (inc (first args-poss)))}]
                               (subvec forms (inc (first args-poss))))})))))

(deftest map-seq->xform-test
  (is (= "(sequence (map identity) [1 2 3])"
         (map-seq->xform "(map identity [1 2 3])")))
  (is (= "(sequence (map #(+ 1 2)) [1 2 3])"
         (map-seq->xform "(map #(+ 1 2) [1 2 3])")))
  (is (= "(sequence (map #?(:clj identity :cljs inc)) [1 2 3])"
         (map-seq->xform "(map #?(:clj identity :cljs inc) [1 2 3])")))
  ;; indentation?
  (is (= (str/join
           "\n"
           ["(sequence (map"
            "  (if 1"
            "    identity"
            "    inc))"
            "  [1 2 3])"])
         (map-seq->xform (str/join
                           "\n"
                           ["(map"
                            "  (if 1"
                            "    identity"
                            "    inc)"
                            "  [1 2 3])"])))))

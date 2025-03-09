(ns ^:typed.clojure typed-test.cljc.checker.name-utils
  (:require [typed.clojure :as t]
            [clojure.test :refer [deftest is]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :refer [is-clj clj is-tc-e clj-opts]]
            [typed.cljc.checker.name-utils :refer [erase-names]]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]))

(declare Pong)
(t/defalias Ping
  (t/U nil '[Pong]))
(t/defalias Pong
  (t/U nil '[Ping]))

(deftest erase-names-ping-pong-test
  (is-tc-e 1)
  (is (= [(prs/parse-clj `Ping) {}]
         (erase-names (prs/parse-clj `Ping)
                      #{}
                      (clj-opts))))
  (is (= [r/-nothing {`Ping 1}]
         (erase-names (prs/parse-clj `Ping)
                      #{`Ping}
                      (clj-opts))))
  (is (= [(prs/parse-clj `nil) {`Ping 1}]
         (erase-names (c/-resolve (prs/parse-clj `Pong) (clj-opts))
                      #{`Ping}
                      (clj-opts))))
  (is (= [(prs/parse-clj `(t/U '[nil] nil)) {`Pong 1}]
         (erase-names (c/-resolve (prs/parse-clj `Pong) (clj-opts))
                      #{`Pong}
                      (clj-opts))))
  
  ;; TFn's expand before occurrences are counted
  (is (= [(prs/parse-clj `'[nil nil]) {`Pong 2}]
         (clj (erase-names (prs/parse-clj `((t/TFn [[x# :variance :covariant]]
                                                   '[x# x#])
                                            Ping))
                           #{`Pong}
                           (clj-opts))))))

(declare T P)
(t/defalias E
  "Expressions"
  (t/U '{:E ':var, :name t/Sym}
       '{:E ':if, :test E, :then E, :else E}
       '{:E ':lambda, :arg t/Sym, :arg-type T, :body E}
       '{:E ':app, :fun E, :args (t/Vec E)}
       '{:E ':false}
       '{:E ':n?}
       '{:E ':add1}))
(t/defalias T
  "Types"
  (t/U '{:T ':fun, :params (t/Vec '{:name t/Sym :type T}), :return T}
       '{:T ':not, :type T}
       '{:T ':union, :types (t/Set T)}
       '{:T ':intersection, :types (t/Set T)}
       '{:T ':false}
       '{:T ':num}
       '{:T ':refine, :name t/Sym, :prop P}))
(t/defalias P
  "Propositions"
  (t/U '{:P ':is, :exp E, :type T}
       '{:P ':=, :exps (t/Set E)}
       '{:P ':or, :ps (t/Set P)}
       '{:P ':and, :ps (t/Set P)}
       '{:P ':not, :p P}))

(deftest erase-names-mini-occ-test
  (is (= {`T 8}
         (second (erase-names (c/-resolve (prs/parse-clj `T) (clj-opts))
                              #{`T} (clj-opts)))))
  (is (= {`E 5}
         (second (erase-names (c/-resolve (prs/parse-clj `E) (clj-opts))
                              #{`E} (clj-opts)))))
  (is (= {`P 6}
         (second (erase-names (c/-resolve (prs/parse-clj `P) (clj-opts))
                              #{`P} (clj-opts))))))

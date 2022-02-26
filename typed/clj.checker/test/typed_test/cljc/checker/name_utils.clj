(ns typed-test.cljc.checker.name-utils
  (:require [clojure.core.typed :as t]
            [clojure.test :refer [deftest is]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :refer [is-clj clj]]
            [typed.cljc.checker.name-utils :refer [erase-names]]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]))

(declare Pong)
(t/defalias Ping
  (t/U nil '[Pong]))
(t/defalias Pong
  (t/U nil '[Ping]))

(deftest erase-names-ping-pong-test
  (is-clj (= [(prs/parse-clj `Ping) {}]
             (clj (erase-names (prs/parse-clj `Ping)
                               #{}))))
  (is-clj (= [r/-nothing {`Ping 1}]
             (erase-names (prs/parse-clj `Ping)
                          #{`Ping})))
  (is-clj (= [(prs/parse-clj `nil) {`Ping 1}]
             (erase-names (c/-resolve (prs/parse-clj `Pong))
                          #{`Ping})))
  (is-clj (= [(prs/parse-clj `(t/U '[nil] nil)) {`Pong 1}]
             (clj (erase-names (c/-resolve (prs/parse-clj `Pong))
                               #{`Pong}))))
  
  ;; TFn's expand before occurrences are counted
  (is-clj (= [(prs/parse-clj `'[nil nil]) {`Pong 2}]
             (clj (erase-names (prs/parse-clj `((t/TFn [[x :variance :covariant]]
                                                       '[x x])
                                                Ping))
                               #{`Pong})))))

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
  (is-clj (= {`T 8}
             (clj (second (erase-names (c/-resolve (prs/parse-clj `T))
                                       #{`T})))))
  (is-clj (= {`E 8}
             (clj (second (erase-names (c/-resolve (prs/parse-clj `E))
                                       #{`E})))))
  (is-clj (= {`P 6}
             (clj (second (erase-names (c/-resolve (prs/parse-clj `P))
                                       #{`P}))))))

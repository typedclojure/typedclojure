(ns clojure.core.typed.test.repl-new
  (:require [typed.clojure :as t]
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest apropos-test
  (is-tc-e #(apropos "clojure") [-> (t/Seq t/Sym)]
           :requires [[clojure.repl :refer [apropos]]])
  (is-tc-e #(apropos #"") [-> (t/Seq t/Sym)]
           :requires [[clojure.repl :refer [apropos]]])
  (is-tc-err #(apropos "clojure") [-> (t/Seq t/Str)]
             :requires [[clojure.repl :refer [apropos]]])
  (is-tc-err #(apropos 'clojure) [-> (t/Seq t/Str)]
             :requires [[clojure.repl :refer [apropos]]]))
             
(deftest demunge-test
  (is-tc-e #(demunge "clojure.repl$demunge") [-> t/Str]
           :requires [[clojure.repl :refer [demunge]]])
  (is-tc-err #(demunge "clojure.repl$demunge") [-> (t/Vec t/Any)]
             :requires [[clojure.repl :refer [demunge]]])
  (is-tc-err #(demunge 'clojure.repl$demunge) [-> t/Str]
             :requires [[clojure.repl :refer [demunge]]]))
             
(deftest source-fn-test
  (is-tc-e #(source-fn 'source) [-> (t/U nil t/Str)]
           :requires [[clojure.repl :refer [source-fn]]])
  (is-tc-err #(source-fn 'source) [-> (t/Vec t/Any)]
             :requires [[clojure.repl :refer [source-fn]]])
  (is-tc-err #(source-fn "source") [-> t/Str]
             :requires [[clojure.repl :refer [source-fn]]]))

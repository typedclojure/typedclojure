(ns typed-test.lib.clojure.core.async
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t]))

; wrap all these tests in thunks to prevent side effects

(deftest async-test
  (is-tc-e 
    #(let [c (chan :- t/Str)]
       (a/go (a/>! c "hello"))
       (prn (a/<!! (a/go (a/<! c))))
       (a/close! c))
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async :refer [chan]]])
  (is-tc-e 
    #(let [c1 (chan :- t/Str)
           c2 (chan :- t/Str)]
       (a/thread (while true
                   (let [[v ch] (a/alts!! [c1 c2])]
                     (println "Read" v "from" ch))))
       (a/>!! c1 "hi")
       (a/>!! c2 "there"))
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async :as ta :refer [chan]]])
  (is-tc-e 
    #(a/alts!! [(a/chan) (a/chan)] :priority true)
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async]])
  (is-tc-e
    (do
      (ann lift-chan (t/All [x y] [[x -> y] -> [(ta/Chan x) -> (ta/Chan y)]]))
      (defn lift-chan [function]
        (fn [in :- (ta/Chan x)]
          (let [out (chan :- y)]
            (a/go
              (loop []
                (let [rcv (<! in)]
                  (when rcv
                    (>! out (function rcv))))))
            out)))

      (ann upper-case [t/Str -> t/Str])
      (defn upper-case [s] s)

      (ann upcase [(ta/Chan t/Str) -> (ta/Chan t/Str)])
      (def upcase (lift-chan upper-case))
      (upcase (chan :- t/Str)))
    :requires [[clojure.core.async :as a :refer [<! >!]]
               [typed.lib.clojure.core.async :as ta :refer [chan]]]))

(deftest rps-async-test
  (is (t/check-ns 'typed-test.lib.clojure.core.async.rps-async-test)))

(deftest go-rule-test
  (is-tc-e 
    #(let [c (chan :- t/Str)]
       (a/go (a/>! c "hello"))
       (prn (a/<!! (a/go (a/<! c))))
       (a/close! c))
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async :refer [chan]]])
  (is-tc-err
    #(let [c (chan :- t/Str)]
       (a/go (a/>! c 123))
       (prn (a/<!! (a/go (a/<! c))))
       (a/close! c))
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async :refer [chan]]])
  (is-tc-e
    (t/ann-form #(a/<!! (a/go "hello"))
                [:-> (t/Nilable t/Str)])
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async :refer [chan]]])
  (is-tc-err
    (t/ann-form #(a/<!! (a/go 123))
                [:-> (t/Nilable t/Str)])
    :requires [[clojure.core.async :as a]
               [typed.lib.clojure.core.async :refer [chan]]]))

;(let [c1 (chan)
;      c2 (chan :- t/Str)]
;  (go (while true
;        (let [[v ch] (a/alts! [c1 c2])]
;          (println "Read" v "from" ch))))
;  (go (a/>! c1 "hi"))
;  (go (a/>! c2 "there")))

(comment
  (require '[clojure.core.typed.current-impl :as impl]
           '[typed.clj.checker.analyze-clj :as ana]
           '[clojure.core.typed.util-vars :as vs])
  )

(ns typed-test.lib.clojure.core.async
  (:require [typed.clj.checker.test-utils :as tu]
            [clojure.test :refer :all]
            [clojure.core.typed :as t]))

(def ^:private common-requires
  '[:requires [[clojure.core.async :as a :refer [<! >!]]
               [typed.lib.clojure.core.async :as ta :refer [chan]]]])

(defmacro is-tc-e [& args] `(tu/is-tc-e ~@args ~@common-requires))
(defmacro is-tc-err [& args] `(tu/is-tc-err ~@args ~@common-requires))

; wrap all these tests in thunks to prevent side effects

(deftest async-test
  (is-tc-e #(let [c (chan :- t/Str)]
              (a/go (a/>! c "hello"))
              (prn (a/<!! (a/go (a/<! c))))
              (a/close! c)))
  (is-tc-e #(let [c1 (chan :- t/Str)
                  c2 (chan :- t/Str)]
              (a/thread (while true
                          (let [[v ch] (a/alts!! [c1 c2])]
                            (println "Read" v "from" ch))))
              (a/>!! c1 "hi")
              (a/>!! c2 "there")))
  (is-tc-e #(a/alts!! [(a/chan) (a/chan)] :priority true))
  (is-tc-e (do
             (ann lift-chan (t/All [x y] [[x :-> y] :-> [(ta/Chan x) :-> (ta/Chan y)]]))
             (cc/defn lift-chan [function]
               (fn [in]
                 (let [out (chan :- y)] ;;TODO infer this
                   (a/go
                     (loop []
                       (let [rcv (<! in)]
                         (when rcv
                           (>! out (function rcv))))))
                   out)))

             (ann upper-case [t/Str :-> t/Str])
             (cc/defn upper-case [s] s)

             (ann upcase [(ta/Chan t/Str) :-> (ta/Chan t/Str)])
             (def upcase (lift-chan upper-case))
             #(upcase (chan :- t/Str)))
           [:-> (ta/Chan t/Str)]))

(deftest pipe-test
  (is-tc-e #(a/pipe (chan :- t/Str)
                    (chan :- t/Str))
           [:-> (ta/Chan t/Str)])
  (is-tc-err #(a/pipe (chan :- t/Int)
                      (chan :- t/Str))
             [:-> (ta/Chan t/Str)])
  (is-tc-e #(a/pipe (t/ann-form (chan) (ta/Chan2 t/Int t/Str))
                    (t/ann-form (chan) (ta/Chan2 t/Str t/Bool)))
           [:-> (ta/Chan2 t/Str t/Bool)])
  (is-tc-err #(a/pipe (t/ann-form (chan) (ta/Chan2 t/Int t/Str))
                      (t/ann-form (chan)
                                  ;; swapped Bool/Str
                                  (ta/Chan2 t/Bool t/Str)))
             [:-> (ta/Chan2 t/Str t/Bool)])
  (is-tc-err #(a/pipe (t/ann-form (chan) (ta/Chan2 t/Int t/Str))
                      (t/ann-form (chan) (ta/Chan2 t/Str t/Bool)))
             ;; swapped Bool/Str
             [:-> (ta/Chan2 t/Bool t/Str)]))

(deftest rps-async-test
  (is (t/check-ns 'typed-test.lib.clojure.core.async.rps-async-test)))

(deftest go-rule-test
  (is-tc-e #(let [c (chan :- t/Str)]
              (a/go (a/>! c "hello"))
              (prn (a/<!! (a/go (a/<! c))))
              (a/close! c)))
  (is-tc-err #(let [c (chan :- t/Str)]
                (a/go (a/>! c 123))
                (prn (a/<!! (a/go (a/<! c))))
                (a/close! c)))
  (is-tc-e #(a/<!! (a/go "hello"))
           [:-> (t/Nilable t/Str)])
  (is-tc-err #(a/<!! (a/go 123))
             [:-> (t/Nilable t/Str)]))

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

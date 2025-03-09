(ns ^:typed.clojure typed-test.lib.clojure.core.async
  (:require [typed.clj.checker.test-utils :as tu]
            [clojure.test :refer :all]
            [clojure.core.typed :as t]))

(def ^:private common-requires
  '[[clojure.core.async :as a :refer [<! >!]]
    [typed.lib.clojure.core.async :as ta :refer [chan]]])

(apply require common-requires)

(defmacro is-tc-e [& args] `(tu/is-tc-e ~@args :requires ~common-requires))
(defmacro is-tc-err [& args] `(tu/is-tc-err ~@args :requires ~common-requires))

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
             (ann lift-chan (t/All [x y] [[x :-> (t/I Object y)] :-> [(ta/Chan x) :-> (ta/Chan y)]]))
             (cc/defn lift-chan [function]
               (fn [in]
                 (let [out (chan :- y)] ;;TODO infer this
                   (a/go-loop []
                     (some->> (<! in)
                              function
                              (>! out))
                     (recur))
                   out)))

             (ann upper-case [t/Str :-> t/Str])
             (cc/defn upper-case [s] s)

             (ann upcase [(ta/Chan t/Str) :-> (ta/Chan t/Str)])
             (def upcase (lift-chan upper-case))
             #(upcase (chan :- t/Str)))
           [:-> (ta/Chan t/Str)]))

(deftest chan-test
  (is-tc-e #(chan 1 (map str))
           [:-> (ta/Chan2 t/Str t/Any)])
  (is-tc-e #(chan 1 (map str))
           [:-> (ta/Chan2 t/Any t/Str)])
  (is-tc-e #(chan 1 (map str))
           [:-> (ta/Chan2 (t/U nil t/Str) t/Str)])
  (is-tc-err #(chan 1 (map str))
             [:-> (ta/Chan2 (t/U nil t/Str) t/Bool)]))

(deftest >!!-test
  (is-tc-e (fn [c :- (ta/Chan t/Str)]
             (a/>!! c "hi")))
  (is-tc-err (fn [c :- (ta/Chan t/Str)]
               (a/>!! c true)))
  ;; nil not allowed
  (is-tc-err (fn [c :- (ta/Chan t/Str)]
               (a/>!! c nil)))
  (is-tc-err (fn [c :- (ta/Chan (t/U nil t/Str))]
               (a/>!! c nil))))

(deftest >!-test
  (is-tc-e (fn [c :- (ta/Chan t/Str)]
             (a/go (a/>! c "hello"))))
  (is-tc-err (fn [c :- (ta/Chan t/Str)]
               (a/go (a/>! c true))))
  ;; nil not allowed
  (is-tc-err (fn [c :- (ta/Chan t/Str)]
               (a/go (a/>! c nil))))
  (is-tc-err (fn [c :- (ta/Chan (t/U nil t/Str))]
               (a/go (a/>! c nil))))
  (is-tc-err (fn [c :- (ta/Chan nil)]
               (a/go (a/>! c nil)))))

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

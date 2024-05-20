(ns cache.defn-chain
  (:require [typed.clojure :as t]))

;; changing any defn form should only recheck that defn, since
;; the type annotation for the others doesn't change

(t/defalias A1 ':A1)
(t/defalias A2 ':A2)
(t/defalias A3 ':A3)
(t/defalias A4 ':A4)
(t/defalias A5 ':A5)

(declare a1 a2 a3 a4 a5)

(t/ann a1 [A1 :-> A5])
(defn a1 [x] ^{::t/dbg "a1"} (a2 :A2))

(t/ann a2 [A2 :-> A5])
(defn a2 [x] ^{::t/dbg "a2"} (a3 :A3))

(t/ann a3 [A3 :-> A5])
(defn a3 [x] ^{::t/dbg "a3"} (a4 :A4))

(t/ann a4 [A4 :-> A5])
(defn a4 [x] ^{::t/dbg "a4"} (a5 :A5))

(t/ann a5 [A5 :-> A5])
(defn a5 [x] ^{::t/dbg "a5"} x)

(comment
  (t/cns)
  ;Start checking cache.defn-chain
  ;::t/dbg id=G__74894 a1
  ;::t/dbg id=G__74894 (a2 :A2)
  ;::t/dbg id=G__74894 expected: cache.defn-chain/A5
  ;::t/dbg id=G__74894 result: cache.defn-chain/A5
  ;::t/dbg id=G__74900 a2
  ;::t/dbg id=G__74900 (a3 :A3)
  ;::t/dbg id=G__74900 expected: cache.defn-chain/A5
  ;::t/dbg id=G__74900 result: cache.defn-chain/A5
  ;::t/dbg id=G__74906 a3
  ;::t/dbg id=G__74906 (a4 :A4)
  ;::t/dbg id=G__74906 expected: cache.defn-chain/A5
  ;::t/dbg id=G__74906 result: cache.defn-chain/A5
  ;::t/dbg id=G__74912 a4
  ;::t/dbg id=G__74912 (a5 :A5)
  ;::t/dbg id=G__74912 expected: cache.defn-chain/A5
  ;::t/dbg id=G__74912 result: cache.defn-chain/A5
  ;::t/dbg id=G__74918 a5
  ;::t/dbg id=G__74918 x
  ;::t/dbg id=G__74918 expected: cache.defn-chain/A5
  ;::t/dbg id=G__74918 result: [cache.defn-chain/A5 {:then tt, :else ff}]
  ;Checked cache.defn-chain in 91.340223 msecs
  :ok
  )

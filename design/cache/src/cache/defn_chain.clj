(ns cache.defn-chain
  (:require [typed.clojure :as t]))

;; changing any defn form should only recheck that defn, since
;; the type annotation for the others doesn't change

(t/defalias A1 ':A1)
(t/defalias A2 ':A2)
(t/defalias A3 ':A3 #_':A0) ;; changing this should recheck a3 and a2 (since it uses a3)
(t/defalias A4 ':A4)
(t/defalias A5 ':A5)

(declare a1 a2 a3 a4 a5)

(t/ann a1 [A1 :-> A5])
(defn a1 [x]
  ^::t/dbg (a2 :A2))

(t/ann a2 [A2 :-> A5])
(defn a2 [x] ^::t/dbg (a3 :A3))

(t/ann a3 [A3 :-> A5])
(defn a3 [x]
  (identity 1 #_nil) ;; changing this should only recheck a3
  ^::t/dbg (a4 :A4))

(t/ann a4 [A4 :-> A5])
(defn a4 [x] ^::t/dbg (do :A5))

(comment
  (t/cns)
  ;Start checking cache.defn-chain
  ;::t/dbg id=G__75255 (a2 :A2)
  ;::t/dbg id=G__75255 expected: cache.defn-chain/A5
  ;::t/dbg id=G__75255 result: cache.defn-chain/A5
  ;::t/dbg id=G__75261 (a3 :A3)
  ;::t/dbg id=G__75261 expected: cache.defn-chain/A5
  ;::t/dbg id=G__75261 result: cache.defn-chain/A5
  ;::t/dbg id=G__75267 (a4 :A4)
  ;::t/dbg id=G__75267 expected: cache.defn-chain/A5
  ;::t/dbg id=G__75267 result: cache.defn-chain/A5
  ;::t/dbg id=G__75273 (do :A5)
  ;::t/dbg id=G__75273 expected: cache.defn-chain/A5
  ;::t/dbg id=G__75273 result: [cache.defn-chain/A5 {:then tt, :else ff}]
  ;Checked cache.defn-chain in 78.504638 msecs
  :ok
  )

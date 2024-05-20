(ns cache.poly
  (:require [typed.clojure :as t]))

(t/defalias As (t/Seqable t/Int))
(t/defalias Bs (t/Seqable t/Bool #_t/Int)) ;; changing this alias should trigger rechecking all defn's in this namespace

(t/ann-many [As :-> Bs]
            map-kw1
            map-kw2
            map-kw3
            map-kw4
            map-kw5
            map-kw6
            map-kw7
            map-kw8
            map-kw9
            map-kw10
            map-kw11
            map-kw12
            map-kw13
            map-kw14
            map-kw15
            map-kw16
            map-kw17
            map-kw18
            map-kw19)
(defn map-kw1 [as] ^{::t/dbg "map-kw1"} (map boolean as))
(defn map-kw2 [as] ^{::t/dbg "map-kw2"} (map boolean as))
(defn map-kw3 [as] ^{::t/dbg "map-kw3"} (map boolean as))
(defn map-kw4 [as] ^{::t/dbg "map-kw4"} (map boolean as))
(defn map-kw5 [as] ^{::t/dbg "map-kw5"} (map boolean as))
(defn map-kw6 [as] ^{::t/dbg "map-kw6"} (map boolean as))
(defn map-kw7 [as] ^{::t/dbg "map-kw7"} (map boolean as))
(defn map-kw8 [as] ^{::t/dbg "map-kw8"} (map boolean as))
(defn map-kw9 [as] ^{::t/dbg "map-kw9"} (map boolean as))
(defn map-kw10 [as] ^{::t/dbg "map-kw10"} (map boolean as))
(defn map-kw11 [as] ^{::t/dbg "map-kw11"} (map boolean as))
(defn map-kw12 [as] ^{::t/dbg "map-kw12"} (map boolean as))
(defn map-kw13 [as] ^{::t/dbg "map-kw13"} (map boolean as))
(defn map-kw14 [as] ^{::t/dbg "map-kw14"} (map boolean as))
(defn map-kw15 [as] ^{::t/dbg "map-kw15"} (map boolean as))
(defn map-kw16 [as] ^{::t/dbg "map-kw16"} (map boolean as))
(defn map-kw17 [as] ^{::t/dbg "map-kw17"} (map boolean as))
(defn map-kw18 [as] ^{::t/dbg "map-kw18"} (map boolean as))
(defn map-kw19 [as] ^{::t/dbg "map-kw19"} (map boolean as))

(comment
  (t/cns)
  ;Start checking cache.poly
  ;::t/dbg id=G__61173 map-kw1
  ;::t/dbg id=G__61173 (map boolean as)
  ;::t/dbg id=G__61173 expected: cache.poly/Bs
  ;::t/dbg id=G__61173 result: cache.poly/Bs
  ;::t/dbg id=G__61538 map-kw2
  ;::t/dbg id=G__61538 (map boolean as)
  ;::t/dbg id=G__61538 expected: cache.poly/Bs
  ;::t/dbg id=G__61538 result: cache.poly/Bs
  ;::t/dbg id=G__61864 map-kw3
  ;::t/dbg id=G__61864 (map boolean as)
  ;::t/dbg id=G__61864 expected: cache.poly/Bs
  ;::t/dbg id=G__61864 result: cache.poly/Bs
  ;::t/dbg id=G__62182 map-kw4
  ;::t/dbg id=G__62182 (map boolean as)
  ;::t/dbg id=G__62182 expected: cache.poly/Bs
  ;::t/dbg id=G__62182 result: cache.poly/Bs
  ;::t/dbg id=G__62524 map-kw5
  ;::t/dbg id=G__62524 (map boolean as)
  ;::t/dbg id=G__62524 expected: cache.poly/Bs
  ;::t/dbg id=G__62524 result: cache.poly/Bs
  ;::t/dbg id=G__62856 map-kw6
  ;::t/dbg id=G__62856 (map boolean as)
  ;::t/dbg id=G__62856 expected: cache.poly/Bs
  ;::t/dbg id=G__62856 result: cache.poly/Bs
  ;::t/dbg id=G__63182 map-kw7
  ;::t/dbg id=G__63182 (map boolean as)
  ;::t/dbg id=G__63182 expected: cache.poly/Bs
  ;::t/dbg id=G__63182 result: cache.poly/Bs
  ;::t/dbg id=G__63518 map-kw8
  ;::t/dbg id=G__63518 (map boolean as)
  ;::t/dbg id=G__63518 expected: cache.poly/Bs
  ;::t/dbg id=G__63518 result: cache.poly/Bs
  ;::t/dbg id=G__63846 map-kw9
  ;::t/dbg id=G__63846 (map boolean as)
  ;::t/dbg id=G__63846 expected: cache.poly/Bs
  ;::t/dbg id=G__63846 result: cache.poly/Bs
  ;::t/dbg id=G__64172 map-kw10
  ;::t/dbg id=G__64172 (map boolean as)
  ;::t/dbg id=G__64172 expected: cache.poly/Bs
  ;::t/dbg id=G__64172 result: cache.poly/Bs
  ;::t/dbg id=G__64508 map-kw11
  ;::t/dbg id=G__64508 (map boolean as)
  ;::t/dbg id=G__64508 expected: cache.poly/Bs
  ;::t/dbg id=G__64508 result: cache.poly/Bs
  ;::t/dbg id=G__64842 map-kw12
  ;::t/dbg id=G__64842 (map boolean as)
  ;::t/dbg id=G__64842 expected: cache.poly/Bs
  ;::t/dbg id=G__64842 result: cache.poly/Bs
  ;::t/dbg id=G__65180 map-kw13
  ;::t/dbg id=G__65180 (map boolean as)
  ;::t/dbg id=G__65180 expected: cache.poly/Bs
  ;::t/dbg id=G__65180 result: cache.poly/Bs
  ;::t/dbg id=G__65522 map-kw14
  ;::t/dbg id=G__65522 (map boolean as)
  ;::t/dbg id=G__65522 expected: cache.poly/Bs
  ;::t/dbg id=G__65522 result: cache.poly/Bs
  ;::t/dbg id=G__65860 map-kw15
  ;::t/dbg id=G__65860 (map boolean as)
  ;::t/dbg id=G__65860 expected: cache.poly/Bs
  ;::t/dbg id=G__65860 result: cache.poly/Bs
  ;::t/dbg id=G__66192 map-kw16
  ;::t/dbg id=G__66192 (map boolean as)
  ;::t/dbg id=G__66192 expected: cache.poly/Bs
  ;::t/dbg id=G__66192 result: cache.poly/Bs
  ;::t/dbg id=G__66518 map-kw17
  ;::t/dbg id=G__66518 (map boolean as)
  ;::t/dbg id=G__66518 expected: cache.poly/Bs
  ;::t/dbg id=G__66518 result: cache.poly/Bs
  ;::t/dbg id=G__66852 map-kw18
  ;::t/dbg id=G__66852 (map boolean as)
  ;::t/dbg id=G__66852 expected: cache.poly/Bs
  ;::t/dbg id=G__66852 result: cache.poly/Bs
  ;::t/dbg id=G__67186 map-kw19
  ;::t/dbg id=G__67186 (map boolean as)
  ;::t/dbg id=G__67186 expected: cache.poly/Bs
  ;::t/dbg id=G__67186 result: cache.poly/Bs
  ;Checked cache.poly in 1643.827655 msecs
  :ok
  )

(ns ^:typed.clojure clojure.core.typed.test.records2
  (:require [clojure.core.typed :as t :refer [ann ann-form ann-record]]))

(ann-record Point [lat :- t/Num
                   lng :- t/Num])
(defrecord Point [lat lng])

(ann-record VerticalLine [p :- Point
                          l :- t/Num])
(defrecord VerticalLine [p l])

(ann-record HorizontalLine [p :- Point
                            l :- t/Num])
(defrecord HorizontalLine [p l])

(ann-record Rect [l :- VerticalLine
                  r :- VerticalLine
                  t :- HorizontalLine
                  b :- HorizontalLine])
(defrecord Rect [^VerticalLine l
                 ^VerticalLine r
                 ^HorizontalLine t
                 ^HorizontalLine b])

(ann hline [Double Double Double -> HorizontalLine])
(defn hline [lat lng l]
  (HorizontalLine. (Point. lat lng) l))

(ann vline [Double Double Double -> VerticalLine])
(defn vline [lat lng l]
  (VerticalLine. (Point. lat lng) l))

(ann create-rect [Double Double Double Double -> Rect])
(defn create-rect [lat_ll lng_ll lat_ur lng_ur]
  (let [ll (Point. lat_ll lng_ll)
        ul (Point. lat_ur lng_ll)
        ur (Point. lat_ur lng_ur)
        lr (Point. lat_ll lng_ur)
        lh (- lng_ur lng_ll)
        lv (- lat_ur lat_ll)]
    (Rect. (VerticalLine. ll lv)
           (VerticalLine. lr lv)
           (HorizontalLine. ul lh)
           (HorizontalLine. ll lh))))

(ann lines-intersect? [VerticalLine HorizontalLine -> Boolean])
(defn lines-intersect? [v h]
  (and
   (>= (:lat (:p h)) (:lat (:p v)))
   (>= (:lng (:p v)) (:lng (:p h)))
   (<= (:lat (:p h)) (+ (:lat (:p v)) (:l v)))
   (<= (:lng (:p v)) (+ (:lng (:p h)) (:l h)))))

(ann rect-overlap? [Rect Rect -> Boolean])
(defn rect-overlap? [r0 r1]
  (or
    (lines-intersect? (:l r0) (:t r1))
    (lines-intersect? (:l r0) (:b r1))
    (lines-intersect? (:r r0) (:t r1))
    (lines-intersect? (:r r0) (:b r1))
    (lines-intersect? (:l r1) (:t r0))
    (lines-intersect? (:l r1) (:b r0))
    (lines-intersect? (:r r1) (:t r0))
    (lines-intersect? (:r r1) (:b r0))))

; a record is never an ISeq
(ann-form
  (let [r (->Point 1 2)]
    (if (seq? r)
      (do (t/print-env "") 'a)
      (do (t/print-env "") 1)))
  t/Num)

(ann-form
  (let [r {}]
    (if (seq? r)
      'a
      1))
  t/Num)

; destructuring
(ann destruct [Point -> t/Num])
(defn destruct [{lng :lng :keys [lat] :as r}]
  lat)

(ann map-over [(t/Seqable Point) -> t/Num])
;FIXME Datatypes+records need proper handling in cs-gen
;(defn map-over [ps]
;  (apply + (map (t/inst :lat t/Num) ps)))

;(typed.clj.checker.subtype/sub-clj? [Point -> t/Num]
;                                     ['{:lat t/Num} -> t/Num])
;
;(typed.clj.checker.subtype/sub-clj? ['{:lat t/Num} -> t/Num]
;                                     [Point -> t/Num])
;
;(typed.clj.checker.subtype/sub-clj? '{:lat t/Num}
;                                     Point)
;
;(typed.clj.checker.subtype/sub-clj? Point
;                                     '{:lat t/Num})

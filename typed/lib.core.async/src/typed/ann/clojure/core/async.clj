;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure 
  ^{:doc 
    "This namespace contains annotations type checking core.async code.

    End users should use typed.lib.clojure.core.async, which all types here
    are qualified under."}
  typed.ann.clojure.core.async
  (:require [typed.clojure :refer [ann ann-datatype defalias ann-protocol] :as t]
            [typed.lib.clojure.core.async :as-alias ta]
            [clojure.core.async.impl.protocols :as-alias impl])
  (:import (java.util.concurrent.locks Lock)))

;TODO how do we encode that nil is illegal to provide to Ports/Channels?
;     Is it essential?

;;;;;;;;;;;;;;;;;;;;
;; Protocols

(ann-protocol clojure.core.async.impl.protocols/Channel
              close! [impl/Channel -> nil])

(ann-protocol [[r :variance :covariant]]
              clojure.core.async.impl.protocols/ReadPort
              take! [(impl/ReadPort r) Lock 
                     -> (t/U nil (t/Deref (t/U nil r)))])

(ann-protocol [[w :variance :contravariant]]
              clojure.core.async.impl.protocols/WritePort
              put! [(impl/WritePort w) w Lock
                    -> (t/U nil (t/Deref nil))])

(ann-protocol [[w :variance :contravariant]
               [r :variance :covariant]]
               clojure.core.async.impl.protocols/Buffer
               full? [(impl/Buffer w r) :-> t/Any]
               remove! [(impl/Buffer w r) :-> nil]
               add!* [(impl/Buffer w r) w :-> (impl/Buffer w r)])

(ann-protocol clojure.core.async.impl.protocols/UnblockingBuffer)

(ann-datatype [[w :variance :contravariant]
               [r :variance :covariant]]
              clojure.core.async.impl.channels.ManyToManyChannel 
              []
              :unchecked-ancestors [impl/Channel
                                    (impl/ReadPort r)
                                    (impl/WritePort w)])

;;;;;;;;;;;;;;;;;;;;
;; Aliases

(defalias 
  ^{:forms '[(Port2 t t)]}
  ta/Port2
  "A port that can write type w and read type r"
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]]
         (t/I (impl/WritePort w)
              (impl/ReadPort r))))

(defalias 
  ^{:forms '[(Port t)]}
  ta/Port
  "A port that can read and write type x"
  (t/TFn [[x :variance :invariant]]
         (ta/Port2 x x)))

(defalias 
  ^{:forms '[(Chan2 t t)]}
  ta/Chan2
  "A core.async channel that can take type w and put type r"
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]]
         (t/I (ta/Port2 w r)
              impl/Channel)))

(defalias 
  ^{:forms '[(Chan t)]}
  ta/Chan
  "A core.async channel"
  (t/TFn [[x :variance :invariant]]
         (ta/Chan2 x x)))

(defalias 
  ^{:forms '[(ReadOnlyChan t)]}
  ta/ReadOnlyChan
  "A core.async channel that statically disallows writes."
  (t/TFn [[r :variance :covariant]]
         (ta/Chan2 t/Nothing r)))

(defalias 
  ^{:forms '[(ReadOnlyPort t)]}
  ta/ReadOnlyPort
  "A read-only port that can read type x"
  (t/TFn [[t :variance :covariant]]
         (ta/Port2 t/Nothing t)))

(defalias 
  ^{:forms '[(WriteOnlyPort t)]}
  ta/WriteOnlyPort
  "A write-only port that can write type p"
  (t/TFn [[p :variance :contravariant]]
         (ta/Port2 p t/Nothing)))

(defalias
  ^{:forms '[TimeoutChan]}
  ta/TimeoutChan
  "A timeout channel"
  (ta/Chan t/Any))

(defalias 
  ^{:forms '[(Buffer2 t t)]}
  ta/Buffer2
  "A buffer of that can write type w and read type t."
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]]
         (t/I (impl/Buffer w r)
              clojure.lang.Counted)))

(defalias 
  ^{:forms '[(Buffer t)]}
  ta/Buffer
  "A buffer of type x."
  (t/TFn [[x :variance :invariant]]
         (ta/Buffer2 x x)))

(defalias 
  ^{:forms '[(UnblockingBuffer2 t t)]}
  ta/UnblockingBuffer2
  "An unblocking buffer that can write type w and read type t."
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]]
         (t/I (ta/Buffer2 w r)
              impl/UnblockingBuffer)))

(defalias 
  ^{:forms '[(UnblockingBuffer t)]}
  ta/UnblockingBuffer
  "An unblocking buffer of type x."
  (t/TFn [[x :variance :invariant]]
         (ta/UnblockingBuffer2 x x)))

;;;;;;;;;;;;;;;;;;;;
;; Var annotations

(ann ^:no-check clojure.core.async/buffer (t/All [t] [t/Int :-> (ta/Buffer t)]))
(ann ^:no-check clojure.core.async/dropping-buffer (t/All [t] [t/Int :-> (ta/Buffer t)]))
(ann ^:no-check clojure.core.async/sliding-buffer (t/All [t] [t/Int :-> (ta/Buffer t)]))

(ann ^:no-check clojure.core.async/thread-call (t/All [x] [[:-> x] :-> (ta/Chan x)]))

(ann ^:no-check clojure.core.async/pipe
     (t/All [a b c]
            [(ta/Chan2 a b) (ta/Chan2 b c) t/Any :? :-> (ta/Chan2 b c)]))

(ann ^:no-check clojure.core.async/timeout [t/Int :-> ta/TimeoutChan])

; TODO buffer must be supplied when xform is
(ann ^:no-check clojure.core.async/chan
     (t/All [p t]
            (t/IFn [:-> (ta/Chan2 p t)]
                   [; buf-or-n
                    (t/U (ta/Buffer2 p t) t/Int nil) :-> (ta/Chan2 p t)]
                   [; buf-or-n
                    (t/U (ta/Buffer2 p t) t/Int nil)
                    ; xform
                    (t/U nil (t/Transducer p t))
                    ; ex-handler
                    (t/U nil [Throwable :-> (t/U nil p)])
                    :?
                    :-> (ta/Chan2 p t)])))


(ann ^:no-check clojure.core.async/<!! (t/All [t] [(ta/Port2 t/Nothing t) :-> (t/U nil t)]))
; should this use Port's?
(ann ^:no-check clojure.core.async/<! (t/All [t] [(ta/Port2 t/Nothing t) :-> (t/U nil t)]))
(ann ^:no-check clojure.core.async/>!! (t/All [p] [(ta/Port2 p t/Any) (t/I Object p) :-> t/Any]))
(ann ^:no-check clojure.core.async/>! (t/All [p t] [(ta/Port2 p t) (t/I Object p) :-> t/Any]))
(t/ann-many
  (t/All [x d]
         (t/IFn [(t/Seqable (t/U (ta/Port x) '[(ta/Port x) x]))
                 & :mandatory {:default d}
                 :optional {:priority (t/U nil true)}
                 :-> (t/U '[d ':default] '[(t/U nil x) (ta/Port x)])]
                [(t/Seqable (t/U (ta/Port x) '[(ta/Port x) x]))
                 & :optional {:priority (t/U nil true)}
                 :-> '[(t/U nil x) (ta/Port x)]]))
  ^:no-check clojure.core.async/alts!!
  ^:no-check clojure.core.async/alts!)

(ann ^:no-check clojure.core.async/close! [impl/Channel :-> nil])

(ann ^:no-check clojure.core.async/put!
     (t/All [p]
            (t/IFn [(ta/Port2 p t/Any) p :-> t/Any]
                   [(ta/Port2 p t/Any) p [t/Any :-> t/Any] :-> t/Any]
                   [(ta/Port2 p t/Any) p [t/Any :-> t/Any] t/Any :-> t/Any])))

#_
(ann ^:no-check clojure.core.async/map<
     (t/All [t o]
            [[t -> o]
             (ta/Chan2 t/Nothing t)
             :->
             (ta/Chan o)]))

#_
(ann ^:no-check clojure.core.async/map>
     (t/All [p t]
            [[t -> p]
             (ta/Chan2 p t)
             :->
             (ta/Chan2 p t)]))

;(ann ^:no-check clojure.core.async/filter>
;     (t/All [t t']
;            (t/IFn
;              [[t :-> t/Any :filters {:then (is t' 0)}] (ta/Chan2 t/Nothing t) :-> (ta/Chan t')]
;              [[t :-> t/Any] (ta/Chan2 t/Nothing t) :-> (ta/Chan t)])))
;
;(ann ^:no-check clojure.core.async/remove>
;     (t/All [p t]
;            (t/IFn
;              [[t :-> t/Any :filters {:then (! p 0)}] (ta/Chan2 p t) :-> (ta/Chan2 p t)]
;              [[t :-> t/Any] (ta/Chan2 p t) :-> (ta/Chan2 p t)])))
;
;(ann ^:no-check clojure.core.async/filter<
;     (t/All [p t]
;            (t/IFn
;              [[t :-> t/Any :filters {:then (is p 0)}] (ta/Chan2 t/Nothing t) :-> (ta/Chan2 p t)]
;              [[t :-> t/Any] (ta/Chan2 t/Nothing t) :-> (ta/Chan2 t t)])))

#_
(ann ^:no-check clojure.core.async/onto-chan
     (t/All [x]
            [(ta/Chan x)
             (t/U nil (t/Seqable x))
             :->
             (ta/Chan t/Any)]))

#_
(ann ^:no-check clojure.core.async/to-chan
     (t/All [x]
            [(t/U nil (t/Seqable x))
             :-> (ta/Chan x)]))

;(ann ^:no-check clojure.core.async/map
;     (All [x]
;          [[x :-> y]
;           (t/U nil (t/Seqable (ta/Chan x)))))

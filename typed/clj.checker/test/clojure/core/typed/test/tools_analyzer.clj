(ns ^:typed.clojure clojure.core.typed.test.tools-analyzer
  (:require [typed.clj.checker.analyze-clj :as ana]
            #_[typed.cljc.checker.utils :as u]
            #_[clojure.core.typed.current-impl :as impl]))

#_(comment
(defprotocol D)

(->
  (ana/ast-for-form
    '(letfn [(a [c] b)
             (b [c] a)]))
  :bindings
  first
  :init
  :methods
  first
  :body
  :ret
  :name
  )

(u/emit-form-fn
(ana/ast-for-form
  '(fn a [b])
  )
(clj-opts))
  )

;; records
(deftype FooDT3 [])
(assert (cast FooDT3 (->FooDT3)))
;(assert (instance? FooDT (->FooDT)))
;(.normal ^FooDT (->FooDT 2 1))

(ns clojure.core.typed.test.CTYP-83-performance
  (:require
    [clojure.core.typed :as t]))

(defmacro pf [frm]
  frm
  #_`(t/print-filterset ~(str frm) ~frm))

(t/defalias VersionVector (t/NonEmptyVec Number))
(t/ann version-less [(t/U nil VersionVector) (t/U nil VersionVector) -> t/Any])
(defn version-less
  "Compare two version vectors."
  [v1 v2]
  (t/loop [v1 :- (t/NilableNonEmptySeq t/Num) (seq v1)
           v2 :- (t/NilableNonEmptySeq t/Num) (seq v2)]
    (let [fv1 (first v1)
          fv2 (first v2)]
     (cond
       (pf (and (pf (not v1)) 
                (pf (not v2)))) false
       (and v1 (not v2)) false
         (or (and (not v1) v2)
             (and fv1 fv2 (< fv1 fv2)))
        true
       (and fv1 fv2 (> fv1 fv2)) false
       :else (recur (next v1) (next v2))
       ))))

;(def f
;  (future
;       (do
;         (require '[clojure.tools.trace])
;         (clojure.tools.trace/trace-vars typed.cljc.checker.filter-ops/-and
;                       typed.cljc.checker.filter-ops/-or)
;         (spit "combine"
;               (with-out-str
;                 (t/check-ns 'clojure.core.typed.test.CTYP-83-performance))))))
;
;(env+ #PropEnv{:l {fv2__#0 (Option java.lang.Number), 
;                   fv1__#0 (Option java.lang.Number), 
;                   v2__#1 (NilableNonEmptySeq Num), 
;                   v1__#1 (NilableNonEmptySeq Num), 
;                   v2__#0 (U VersionVector nil), 
;                   v1__#0 (U VersionVector nil)}, 
;               :props #{(when (is (U nil false) v1__#1) (is (U EmptyCount nil) v1__#0)) 
;                        (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;                        (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is NonEmptyCount v1__#0))) 
;                        (when (is (U nil false) v2__#1) (is (U EmptyCount nil) v2__#0))}} 
;      [(& (when (is (U nil false) v1__#1) 
;            (is (U EmptyCount nil) v1__#0)) 
;          (is (U nil false) v1__#1) 
;          (when (! (U nil false) v2__#1) 
;            (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;          (when (! (U nil false) v1__#1) 
;            (& (! nil v1__#0) (is NonEmptyCount v1__#0))) 
;          (is (U nil false) v2__#1) 
;          (when (is (U nil false) v2__#1) 
;            (is (U EmptyCount nil) v2__#0)))] 
;      #<Atom@15366b7d: true>)
;PropEnv{:l {fv2__#0 (Option java.lang.Number), 
;            fv1__#0 (Option java.lang.Number), 
;            v2__#1 nil, 
;            v1__#1 nil, 
;            v2__#0 nil, 
;            v1__#0 nil}, 
;        :props #{(is (U nil false) v1__#1) 
;                 (when (! (U nil false) v2__#1) 
;                   (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;                 (is (U EmptyCount nil) v1__#0) 
;                 (is (U EmptyCount nil) v2__#0) 
;                 (when (! (U nil false) v1__#1) 
;                   (& (! nil v1__#0) (is NonEmptyCount v1__#0))) 
;                 (is (U nil false) v2__#1)}}
;
;TRACE t73988: (env+ 
;                #PropEnv{:l {v1__#1 (NilableNonEmptySeq Num), 
;                             v2__#0 (U VersionVector nil), 
;                             v1__#0 (U VersionVector nil)}, 
;                         :props #{(when (is (U nil false) v1__#1) 
;                                    (is (U EmptyCount nil) v1__#0)) 
;                                  (when (! (U nil false) v1__#1) 
;                                    (& (! nil v1__#0) 
;                                       (is NonEmptyCount v1__#0)))}} 
;                [tt] #<Atom@60f988ac: true>)
;TRACE t73988: => #PropEnv{:l {v1__#1 (NilableNonEmptySeq Num), 
;                              v2__#0 (U VersionVector nil), 
;                              v1__#0 (U VersionVector nil)}, 
;                          :props #{(when (is (U nil false) v1__#1) 
;                                     (is (U EmptyCount nil) v1__#0)) 
;                                   (when (! (U nil false) v1__#1) 
;                                     (& (! nil v1__#0) (is NonEmptyCount v1__#0)))}}
;
;(env+ #PropEnv{:l {v2__#1 (NilableNonEmptySeq Num), 
;                   v1__#1 (NilableNonEmptySeq Num), 
;                   v2__#0 (U VersionVector nil), 
;                   v1__#0 (U VersionVector nil)}, 
;               :props #{(when (is (U nil false) v1__#1) 
;                          (is (U EmptyCount nil) v1__#0)) 
;                        (when (! (U nil false) v2__#1) 
;                          (& (! nil v2__#0) 
;                             (is NonEmptyCount v2__#0))) 
;                        (when (! (U nil false) v1__#1) 
;                          (& (! nil v1__#0) 
;                             (is NonEmptyCount v1__#0))) 
;                        (when (is (U nil false) v2__#1) 
;                          (is (U EmptyCount nil) v2__#0))}} 
;      [tt] #<Atom@6e917b55: true>)
;TRACE t74338: => #typed.cljc.checker.lex_env.PropEnv{:l {v2__#1 (NilableNonEmptySeq Num), v1__#1 (NilableNonEmptySeq Num), v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), v1__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil)}, :props #{(when (is (U nil false) v1__#1) (is (U EmptyCount nil) v1__#0)) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is NonEmptyCount v2__#0))) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is NonEmptyCount v1__#0))) (when (is (U nil false) v2__#1) (is (U EmptyCount nil) v2__#0))}}
;
;
;
;TRACE t74596: (env+ #PropEnv{:l {and__3941__auto____#0 boolean, 
;                                 fv2__#0 (Option java.lang.Number), 
;                                 fv1__#0 (Option java.lang.Number), 
;                                 v2__#1 (NilableNonEmptySeq Num), 
;                                 v1__#1 (NilableNonEmptySeq Num), 
;                                 v2__#0 (U VersionVector nil), 
;                                 v1__#0 (U VersionVector nil)}, 
;                             :props #{(when (is (U nil false) v1__#1) 
;                                        (is (U EmptyCount nil) v1__#0)) 
;                                      (when (! (U nil false) v2__#1) 
;                                        (& (! nil v2__#0) 
;                                           (is NonEmptyCount v2__#0))) 
;                                      (when (is (U nil false) and__3941__auto____#0) 
;                                        (! (U nil false) v1__#1)) 
;                                      (when (! (U nil false) and__3941__auto____#0) 
;                                        (is (U nil false) v1__#1)) 
;                                      (when (! (U nil false) v1__#1) 
;                                        (& (! nil v1__#0) 
;                                           (is NonEmptyCount v1__#0))) 
;                                      (when (is (U nil false) v2__#1) 
;                                        (is (U EmptyCount nil) v2__#0))}} 
;                    [(is (U nil false) and__3941__auto____#0)] 
;                    #<Atom@252f13e9: true>)
;TRACE t74596: => #PropEnv{:l {and__3941__auto____#0 false, fv2__#0 (Option java.lang.Number), fv1__#0 (Option java.lang.Number), v2__#1 (NilableNonEmptySeq Num), v1__#1 (I (clojure.lang.ISeq Num) (CountRange 1)), v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), v1__#0 (I (clojure.lang.IPersistentVector java.lang.Number) (CountRange 1))}, 
;                          :props #{(! nil v1__#0) 
;                                   (! (U nil false) v1__#1) 
;                                   (is NonEmptyCount v1__#0) 
;                                   (when (is (U nil false) v1__#1) (is (U EmptyCount nil) v1__#0)) 
;                                   (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;                                   (is (U nil false) and__3941__auto____#0) 
;                                   (when (! (U nil false) and__3941__auto____#0) (is (U nil false) v1__#1)) 
;                                   (when (is (U nil false) v2__#1) (is (U EmptyCount nil) v2__#0))}}

;TRACE t74634: (env+ #PropEnv{:l {fv2__#0 (clojure.core.typed/Option java.lang.Number), 
;                                 fv1__#0 (clojure.core.typed/Option java.lang.Number), 
;                                 v2__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), 
;                                 v1__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), 
;                                 v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), 
;                                 v1__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil)}, 
;                             :props #{(when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0)) 
;                                      (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                                      (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) 
;                                      (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))}} 
;                    [(& (| (when (! (U nil false) v2__#1) 
;                             (& (! nil v2__#0) 
;                                (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                           (when (! (U nil false) v1__#1) 
;                             (& (! nil v1__#0) 
;                                (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v2__#1) 
;                           (is clojure.core.typed/NonEmptyCount v1__#0)) 
;                        (| (! (U nil false) v2__#1) 
;                           (when (is (U nil false) v1__#1) 
;                             (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (! nil v1__#0) 
;                           (is (U nil false) v1__#1)) 
;                        (| (! nil v1__#0) 
;                           (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! (U nil false) v2__#1) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (is (U nil false) v1__#1)) 
;                        (| (! nil v1__#0) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (! nil v1__#0) 
;                           (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (when (is (U nil false) v1__#1) 
;                          (is (U clojure.core.typed/EmptyCount nil) v1__#0)) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v2__#1) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! nil v1__#0) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (is (U nil false) v1__#1) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (when (! (U nil false) v2__#1) 
;                          (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! nil v1__#0) 
;                           (! (U nil false) v2__#1)) 
;                        (| (is (U nil false) v1__#1) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (when (is (U nil false) v2__#1) 
;                          (is (U clojure.core.typed/EmptyCount nil) v2__#0)) 
;                        (| (! (U nil false) v2__#1) 
;                           (! (U nil false) v1__#1)))] 
;                    #<Atom@1f9f75fa: true>)
;TRACE t74634: => #typed.cljc.checker.lex_env.PropEnv{:l {fv2__#0 (clojure.core.typed/Option java.lang.Number), fv1__#0 (clojure.core.typed/Option java.lang.Number), v2__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), v1__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), v1__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil)}, :props #{(| (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v2__#1) (is clojure.core.typed/NonEmptyCount v1__#0)) (| (! (U nil false) v2__#1) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (! nil v1__#0) (is (U nil false) v1__#1)) (| (! nil v1__#0) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! (U nil false) v2__#1) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (is (U nil false) v1__#1)) (| (! nil v1__#0) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! (U nil false) v1__#1) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (! nil v1__#0) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v1__#1) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0)) (| (! (U nil false) v1__#1) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v2__#1) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! nil v1__#0) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (is (U nil false) v1__#1) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v1__#1) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! nil v1__#0) (! (U nil false) v2__#1)) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) (| (is (U nil false) v1__#1) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0)) (| (! (U nil false) v2__#1) (! (U nil false) v1__#1))}}

(ns ^:typed.clojure typed-test.spec.clojure.core
  (:require [typed.spec.clojure.core :as api]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [typed.clj.spec :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

;; reduced-spec

(deftest reduced-spec-test
  (doseq [v (gen/sample
              (s/gen (api/reduced-spec integer?)))]
    (assert (reduced? v) v)
    (is (integer? @v) v))
  (tu/is-valid (api/reduced-spec integer?) (reduced 1))
  (tu/is-invalid (api/reduced-spec symbol?) (reduced 1))
  (tu/is-valid (s/or :reduced (api/reduced-spec integer?)
                     :integer integer?)
               (reduced 1))
  (tu/is-invalid (s/or :reduced (api/reduced-spec integer?)
                       :integer integer?)
                 (gensym))
  (tu/is-invalid (api/reduced-spec integer?)
                 (gensym))
  (is (apply
        (every-pred
          reduced?
          (comp integer? deref))
        (gen/sample
          (s/gen (api/reduced-spec integer?)))))
  )

;; atom-spec

(s/def ::deref
  (t/all :binder (t/binder :r (t/bind-tv))
         :body
         (s/fspec :args (s/cat :atom (api/atom-spec :read (t/tv :r)))
                  :ret (t/tv :r))))

(s/def ::reset!
  (t/all :binder (t/binder :w (t/bind-tv))
         :body
         (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w))
                               :w (t/tv :w))
                  :ret (t/tv :w))))

(s/def ::swap!-2
  (t/all :binder (t/binder :w (t/bind-tv)
                           :r (t/bind-tv))
         :body
         (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                    :read (t/tv :r))
                               :f (s/fspec :args (s/cat :r (t/tv :r))
                                           :ret (t/tv :w)))
                  :ret (t/tv :w))))

(s/def ::swap!
  (t/all :binder (t/binder :w (t/bind-tv)
                           :r (t/bind-tv)
                           :extra-args (t/bind-tv
                                         :kind (s/* (t/binder
                                                      :arg (t/bind-tv)))))
         :body
         (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                    :read (t/tv :r))
                               :f (s/fspec :args (s/cat :r (t/tv :r)
                                                        :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                           :ret (t/tv :w))
                               :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                  :ret (t/tv :w))))

(s/def ::swap-vals!-2
  (t/all :binder (t/binder :w (t/bind-tv)
                           :r (t/bind-tv))
         :body
         (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                    :read (t/tv :r))
                               :f (s/fspec :args (s/cat :r (t/tv :r))
                                           :ret (t/tv :w)))
                  :ret (s/tuple (t/tv :r) (t/tv :w)))))

(s/def ::swap-vals!
  (t/all :binder (t/binder :w (t/bind-tv)
                           :r (t/bind-tv)
                           :extra-args (t/bind-tv
                                         :kind (s/* (t/binder
                                                      :arg (t/bind-tv)))))
         :body
         (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                    :read (t/tv :r))
                               :f (s/fspec :args (s/cat :r (t/tv :r)
                                                        :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                           :ret (t/tv :w))
                               :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                  :ret (s/tuple (t/tv :r) (t/tv :w)))))

(s/def ::atom
  (t/all :binder (t/binder :w (t/bind-tv)
                           :r (t/bind-tv
                                :kind (t/spec-between
                                        :lower (t/tv :w))))
         :body
         (s/fspec :args (s/cat :w (t/tv :w))
                  :ret (api/atom-spec :write (t/tv :w)
                                      :read (t/tv :r)))))

(deftest atom-spec-test
  ;; deref
  (tu/is-valid (s/fspec :args (s/cat :atom (api/atom-spec :read integer?))
                        :ret integer?)
               deref)
  (tu/is-invalid (s/fspec :args (s/cat :atom (api/atom-spec :read integer?))
                          :ret boolean?)
                 deref)
  (tu/is-valid ::deref deref)
  (tu/is-invalid ::deref (comp str deref))
  ;; reset!
  (tu/is-valid ::reset! reset!)
  (tu/is-invalid ::reset! deref)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w))
                                              :w (t/tv :w))
                                 ; bad return
                                 :ret integer?))
                 reset!)
  ;; swap! (arity 2)
  (tu/is-valid ::swap!-2 swap!)
  (tu/is-valid ::swap!-2
               ; spec does not require the write to actually happen
               (fn [a f]
                 (f @a)))
  (tu/is-invalid ::swap!-2
                 ; cannot just return deref
                 (fn [a f]
                   @a))
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r))
                                                          ;returns wrong val
                                                          :ret any?))
                                 :ret (t/tv :w)))
                 swap!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :w)) ;takes :w
                                                          :ret (t/tv :w)))
                                 :ret (t/tv :w)))
                 swap!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :r) ;swap :r and :w
                                                                   :read (t/tv :w))
                                              :f (s/fspec :args (s/cat :r (t/tv :r))
                                                          :ret (t/tv :w)))
                                 :ret (t/tv :w)))
                 swap!)
  ;; swap! (arity 3+)
  (tu/is-valid ::swap! swap!)
  (tu/is-invalid ::swap!
                 ;drops args
                 (fn [a f & args] (swap! a f)))
  (tu/is-invalid ::swap!
                 swap-vals!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv)
                                          :extra-args (t/bind-tv
                                                        :kind (s/* (t/binder
                                                                     :arg (t/bind-tv)))))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r)
                                                                       :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                                          :ret (t/tv :w))
                                              ;provided args too broad
                                              :extra-args (t/fold-binders any? :extra-args))
                                 :ret (t/tv :w)))
                 swap!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv)
                                          :extra-args (t/bind-tv
                                                        :kind (s/* (t/binder
                                                                     :arg (t/bind-tv)))))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r)
                                                                       :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                                          :ret (t/tv :w))
                                              ; no extra args passed
                                              #_#_:extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                 :ret (t/tv :w)))
                 swap!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv)
                                          :extra-args (t/bind-tv
                                                        :kind (s/* (t/binder
                                                                     :arg (t/bind-tv)))))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r)
                                                                       ; no extra args accepted
                                                                       #_#_:extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                                          :ret (t/tv :w))
                                              :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                 :ret (t/tv :w)))
                 swap!)
  ;; swap-vals! (arity 2)
  (tu/is-valid ::swap-vals!-2
               swap-vals!)
  (tu/is-invalid ::swap-vals!-2
                 swap!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r))
                                                          :ret (t/tv :w)))
                                 ;swapped return order
                                 :ret (s/tuple (t/tv :w) (t/tv :r))))
                 swap-vals!)
  ;; swap-vals! (arity 3+)
  (tu/is-valid ::swap-vals!
               swap-vals!)
  (tu/is-invalid ::swap-vals!
                 ;drops args
                 (fn [a f & args] (swap-vals! a f)))
  (tu/is-invalid ::swap-vals!
                 swap!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv)
                                          :extra-args (t/bind-tv
                                                        :kind (s/* (t/binder
                                                                     :arg (t/bind-tv)))))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r)
                                                                       :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                                          :ret (t/tv :w))
                                              ;provided args too broad
                                              :extra-args (t/fold-binders any? :extra-args))
                                 :ret (s/tuple (t/tv :r) (t/tv :w))))
                 swap-vals!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv)
                                          :extra-args (t/bind-tv
                                                        :kind (s/* (t/binder
                                                                     :arg (t/bind-tv)))))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r)
                                                                       :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                                          :ret (t/tv :w))
                                              ; no extra args passed
                                              #_#_:extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                 :ret (s/tuple (t/tv :r) (t/tv :w))))
                 swap-vals!)
  (tu/is-invalid (t/all :binder (t/binder :w (t/bind-tv)
                                          :r (t/bind-tv)
                                          :extra-args (t/bind-tv
                                                        :kind (s/* (t/binder
                                                                     :arg (t/bind-tv)))))
                        :body
                        (s/fspec :args (s/cat :atom (api/atom-spec :write (t/tv :w)
                                                                   :read (t/tv :r))
                                              :f (s/fspec :args (s/cat :r (t/tv :r)
                                                                       ; no extra args accepted
                                                                       #_#_:extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                                          :ret (t/tv :w))
                                              :extra-args (t/fold-binders (t/tv :arg) :extra-args))
                                 :ret (s/tuple (t/tv :r) (t/tv :w))))
                 swap-vals!)
  ;; clojure.core/atom
  ;(tu/is-valid ::atom atom) ;;FIXME fails in CI
  (tu/is-invalid ::atom (comp atom str))
  (tu/is-invalid ::atom (comp #(doto % (reset! "a")) atom))
  )

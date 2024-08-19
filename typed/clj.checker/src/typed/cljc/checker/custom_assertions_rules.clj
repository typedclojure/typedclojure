(ns typed.cljc.checker.custom-assertions-rules
  (:refer-clojure :exclude [defn defn- fn assert])
  (:require [clojure.core :as core]
            [typed.cljc.checker.custom-assertions :as custom]
            [clojure.core.typed.internal :as internal]
            [typed.cljc.checker.check :as check]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]
            [typed.clj.checker.check.unanalyzed :as un-clj]))

(defuspecial defuspecial__custom-assert
  "defuspecial implementation for typed.cljc.checker.custom-assertions/assert"
  [{:keys [form] :as expr} expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (assoc :form (with-meta (list* (with-meta 'clojure.core/assert (meta (first form)))
                                     (rest form))
                              (meta form)))
      (check-expr expected opts)))

(defuspecial defuspecial__custom-fn
  "defuspecial implementation for typed.cljc.checker.custom-fnions/fn"
  [{:keys [form] :as expr} expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (assoc :form (with-meta (list* (with-meta 'clojure.core/fn (meta (first form)))
                                     (rest form))
                              (meta form)))
      (check-expr expected opts)))


(defuspecial defuspecial__custom-defn
  "defuspecial implementation for typed.cljc.checker.custom-assertions/defn"
  [{:keys [form] :as expr} expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (assoc :form (with-meta (list* (with-meta 'clojure.core/defn (meta (first form)))
                                     (rest form))
                              (meta form)))
      (check-expr expected opts)))

(defuspecial defuspecial__custom-defn-
  "defuspecial implementation for typed.cljc.checker.custom-assertions/defn-"
  [{:keys [form] :as expr} expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (assoc :form (with-meta (list* (with-meta 'clojure.core/defn- (meta (first form)))
                                     (rest form))
                              (meta form)))
      (check-expr expected opts)))

(un-clj/install-defuspecial
  'typed.cljc.checker.custom-assertions/assert
  `defuspecial__custom-assert)

(un-clj/install-defuspecial
  'typed.cljc.checker.custom-assertions/fn
  `defuspecial__custom-fn)

(un-clj/install-defuspecial
  'typed.cljc.checker.custom-assertions/defn
  `defuspecial__custom-defn)

(un-clj/install-defuspecial
  'typed.cljc.checker.custom-assertions/defn-
  `defuspecial__custom-defn-)

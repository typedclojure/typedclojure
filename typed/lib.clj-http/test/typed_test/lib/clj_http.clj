(ns ^:typed.clojure typed-test.lib.clj-http
  (:require [typed.clj.checker.test-utils :as tu]
            [clojure.test :refer :all]
            [clojure.core.typed :as t]))

(def ^:private common-requires
  '[[clj-http.client :as client]
    [typed.lib.clj-http :as tclient]
    [clojure.core.typed :as t]])

(apply require common-requires)

(defmacro is-tc-e [& args] `(tu/is-tc-e ~@args :requires ~common-requires))
(defmacro is-tc-err [& args] `(tu/is-tc-err ~@args :requires ~common-requires))

(deftest get-test
  (is-tc-e #(client/get "https://example.com")
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/get "https://example.com" {})
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/get "https://example.com" {:as :json})
           [:-> tclient/ResponseMap]))

(deftest post-test
  (is-tc-e #(client/post "https://example.com")
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/post "https://example.com" {:body "test"})
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/post "https://example.com" {:body "test" :headers {}})
           [:-> tclient/ResponseMap]))

(deftest put-test
  (is-tc-e #(client/put "https://example.com")
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/put "https://example.com" {:body "test"})
           [:-> tclient/ResponseMap]))

(deftest patch-test
  (is-tc-e #(client/patch "https://example.com")
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/patch "https://example.com" {:body "test"})
           [:-> tclient/ResponseMap]))

(deftest delete-test
  (is-tc-e #(client/delete "https://example.com")
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/delete "https://example.com" {:as :json})
           [:-> tclient/ResponseMap]))

(deftest head-test
  (is-tc-e #(client/head "https://example.com")
           [:-> tclient/ResponseMap]))

(deftest options-test
  (is-tc-e #(client/options "https://example.com")
           [:-> tclient/ResponseMap]))

(deftest request-test
  (is-tc-e #(client/request {:url "https://example.com" :method :get})
           [:-> tclient/ResponseMap])
  (is-tc-e #(client/request {:url "https://example.com" :method :post :body "test"})
           [:-> tclient/ResponseMap]))

(deftest response-keys-test
  (is-tc-e #(let [resp (client/get "https://example.com")]
              (:status resp))
           [:-> t/Any])
  (is-tc-e #(let [resp (client/get "https://example.com")]
              (:headers resp))
           [:-> t/Any])
  (is-tc-e #(let [resp (client/get "https://example.com")]
              (:body resp))
           [:-> t/Any]))

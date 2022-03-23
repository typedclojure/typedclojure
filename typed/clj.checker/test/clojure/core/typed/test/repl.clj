(ns clojure.core.typed.test.repl
  (:require [clojure.test :refer [deftest is]]
            [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.server :as server]
            [clojure.tools.nrepl.ack :as ack]
            [clojure.set :as set]
            [typed.clj.checker.repl :as repl]))

(def ^{:dynamic true} *server* nil)

(defn repl-server-fixture
  [f]
  (with-open [^java.io.Closeable server (server/start-server
                                          :handler (server/default-handler
                                                     #'repl/wrap-clj-repl))]
    (binding [*server* server]
      (let [s (f)]
        (set! *print-length* nil)
        (set! *print-level* nil)
        s))))

;(use-fixtures :each repl-server-fixture)

(deftest eval-switch-ns
  (is (= "blah"
         (:ns 
           (with-open [^java.io.Closeable server (server/start-server
                                                   :handler (server/default-handler
                                                              #'repl/wrap-clj-repl))]
             (with-open [^java.io.Closeable transport (nrepl/connect :port (:port server))]
               (let [cl (nrepl/client transport Long/MAX_VALUE)
                     ses (nrepl/client-session cl)]
                 (nrepl/combine-responses
                   (nrepl/message ses
                                  {:op :eval :code "(ns blah)"}))
                 (nrepl/combine-responses
                   (nrepl/message ses
                                  {:op :eval :code "*ns*"})))))))))

(deftest load-file-test
  (is (= ["42"]
         (:value
           (with-open [^java.io.Closeable server (server/start-server
                                                   :handler (server/default-handler
                                                              #'repl/wrap-clj-repl))]
             (with-open [^java.io.Closeable transport (nrepl/connect :port (:port server))]
               (let [cl (nrepl/client transport Long/MAX_VALUE)
                     ses (nrepl/client-session cl)]
                 (nrepl/combine-responses
                   (nrepl/message ses
                                  {:op :load-file 
                                   :file "(ns ^:core.typed foo.bar
                                         (:require [typed.clojure :as t]))
                                         (t/ann a t/Sym)
                                         (def a 'a)
                                         42"
                                   :file-path "foo/bar.clj"
                                   :file-name "bar.clj"}))))))))
  ;;FIXME broke after re-throwing exception in check-form-common
  #_
  (testing "runtime error"
    (is (= "class java.lang.ArithmeticException"
           (:ex
             (with-open [^java.io.Closeable server (server/start-server
                                                     :handler (server/default-handler
                                                                #'repl/wrap-clj-repl))]
               (with-open [^java.io.Closeable transport (nrepl/connect :port (:port server))]
                 (let [cl (nrepl/client transport Long/MAX_VALUE)
                       ses (nrepl/client-session cl)]
                   (nrepl/combine-responses
                     (nrepl/message ses
                                    {:op :load-file 
                                     :file "(ns ^:core.typed foo.bar
                                           (:require [typed.clojure :as t]))
                                           (t/ann a t/Sym)
                                           (def a 'a)
                                           (/ 1 0)"
                                     :file-path "foo/bar.clj"
                                     :file-name "bar.clj"})))))))))
  ;; FIXME
  #_
  (testing "fireplace test"
    (is (= "class clojure.lang.ExceptionInfo"
           (:ex
             (with-open [^java.io.Closeable server (server/start-server
                                                     :handler (server/default-handler
                                                                #'repl/wrap-clj-repl))]
               (with-open [^java.io.Closeable transport (nrepl/connect :port (:port server))]
                 (let [cl (nrepl/client transport Long/MAX_VALUE)
                       ses (nrepl/client-session cl)]
                   (nrepl/combine-responses
                     (nrepl/message ses
                                    {:op :load-file 
                                     :file "(ns ^:core.typed baz.boo)"
                                     :file-path "baz/boo.clj"
                                     :file-name "boo.clj"}))
                   (nrepl/combine-responses
                     (nrepl/message ses
                                    {:op :load-file 
                                     :file "(in-ns 'baz.boo)
                                           (inc 'a)"
                                     :file-path "baz/boo.clj"
                                     :file-name "boo.clj"})))))))))
  )

;(with-open [^java.io.Closeable server (server/start-server)]
;  (with-open [transport (nrepl/connect :port (:port server))]
;    (nrepl/message (nrepl/client transport Long/MAX_VALUE)
;                   {:op :eval :code "1"})))

(defmacro repl-test
  [& body]
  `(repl-server-fixture
     #(with-open [~(with-meta 'transport {:tag 'java.io.Closeable}) (nrepl/connect :port (:port *server*))]
        ~@body)))

;(repl-test
;  (nrepl/combine-responses
;    (nrepl/message (nrepl/client transport Long/MAX_VALUE)
;                   {:op :eval :code "(ns ^:core.typed foo)"})))
;
;(repl-test
;  (nrepl/combine-responses
;    (nrepl/message (nrepl/client transport Long/MAX_VALUE)
;                   {:op :eval :code "(new java.io.File \"a\")"})))
;
;(deftest tst-pkt
;  (repl-test
;    (nrepl/combine-responses
;      (nrepl/message (nrepl/client transport Long/MAX_VALUE)
;                     {:op :eval :code "(ns ^:core.typed foo)"}))))

(defn eval-msg [transport c]
  (nrepl/message (nrepl/client transport Long/MAX_VALUE)
                 {:op :eval :code c}))

(defn eval-val [transport c]
  (-> (eval-msg transport c)
      nrepl/combine-responses
      (select-keys [:value])))

;(deftest core-typed-test
;  (repl-test
;    (eval-msg transport "(ns bar) ")
;    ;(is (= {:value ["nil"]}
;    ;       (eval-val transport "(ns ^:core.typed foo) ")))
;    ;(is (= {:value ["nil"]}
;    ;       (eval-val transport "*ns*")))
;    ;(is (= {:value ["2"]}
;    ;       (eval-val transport "(inc 1)")))
;    )
;  )

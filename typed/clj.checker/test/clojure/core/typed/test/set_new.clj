(ns ^:typed.clojure clojure.core.typed.test.set-new
  (:require [typed.clojure :as t]
            [clojure.set :as set] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest subset-test
  (is-tc-e (subset? #{1} #{2}) t/Bool 
           :requires [[clojure.set :refer [subset?]]])
  (is-tc-err (subset? #{1} #{2}) (t/Set t/Any) 
             :requires [[clojure.set :refer [subset?]]]))

(deftest superset-test
  (is-tc-e (superset? #{1} #{2}) t/Bool 
           :requires [[clojure.set :refer [superset?]]])
  (is-tc-err (superset? #{1} #{2}) (t/Set t/Any) 
             :requires [[clojure.set :refer [superset?]]]))

(deftest join-test
  (is-tc-e (join #{{:a 1} {:a 2}} #{{:b 1} {:b 2}}) (t/Set (t/Map t/Any t/Any)) 
           :requires [[clojure.set :refer [join]]])
  (is-tc-err (join #{{:a 1} {:a 2}} #{{:b 1} {:b 2}}) (t/Vec t/Any)
             :requires [[clojure.set :refer [join]]]))

(deftest index-test
  (is-tc-e (index #{{:a 1 :b 2} {:a 3 :b 4} {:a 1 :b 5} {:a 2 :b 6}} [:a]) 
           (t/Map (t/Map t/Any t/Any) (t/Set (t/Map t/Any t/Any))) 
           :requires [[clojure.set :refer [index]]])
  (tc-e (if-let [f (first (index #{{:a 1 :b 2} {:a 3 :b 4} {:a 1 :b 5} {:a 2 :b 6}} [:a]))]
          (val f)
          (assert nil))
        (t/Set (t/Map t/Kw t/Int))
        :requires [[clojure.set :refer [index]]])
  (is-tc-err (index #{{:a 1 :b 2} {:a 3 :b 4} {:a 1 :b 5} {:a 2 :b 6}} [:a]) (t/Vec t/Any) 
             :requires [[clojure.set :refer [index]]]))

(deftest map-invert-test
  (is-tc-e (map-invert {:a 1, :b 2}) (t/Map t/Int t/Kw)
           :requires [[clojure.set :refer [map-invert]]])
  (is-tc-err (map-invert {:a 1, :b 2}) (t/Map t/Kw t/Int)
             :requires [[clojure.set :refer [map-invert]]]))

(deftest project-test
  (is-tc-e (project #{{:a "b" :c 1} {:a "d" :c 2}} [:a]) (t/Set (t/Map t/Kw (t/U t/Int t/Str)))
           :requires [[clojure.set :refer [project]]])
  (is-tc-err (project #{{:a "b" :c 1} {:a "d" :c 2}} [:a]) (t/Set (t/Map t/Kw t/Str))
             :requires [[clojure.set :refer [project]]]))

(deftest rename-test
  (is-tc-e (rename #{{:a 1, :b 1}  {:a 2, :b 2}} {:a :new-a}) (t/Set (t/Map t/Kw t/Int))
           :requires [[clojure.set :refer [rename]]])
  (is-tc-e (rename #{{:a 1, :b 1}  {:a 2, :b 2}} {:a 1}) (t/Set (t/Map (t/U t/Int t/Kw) t/Int))
           :requires [[clojure.set :refer [rename]]])
  (is-tc-err (rename #{{:a 1, :b 1}  {:a 2, :b 2}} {:a 1}) (t/Set (t/Map t/Kw t/Int))
           :requires [[clojure.set :refer [rename]]]))


(deftest rename-keys-test
  (is-tc-e (rename-keys {:a 1, :b 2} {:a :new-a, :b :new-b}) (t/Map t/Kw t/Int)
           :requires [[clojure.set :refer [rename-keys]]])
  (is-tc-e (rename-keys {:a 1, :b 2} {:a 1, :b :new-b}) (t/Map (t/U t/Int t/Kw) t/Int)
           :requires [[clojure.set :refer [rename-keys]]])
  (is-tc-err (rename-keys {:a 1, :b 2} {:a 1, :b :new-b}) (t/Map t/Kw t/Int)
             :requires [[clojure.set :refer [rename-keys]]]))

(deftest select-test
  (is-tc-e (select odd? #{1 2 3}) (t/Set t/Int)
           :requires [[clojure.set :refer [select]]])
  (is-tc-e (select integer? #{1 2 3 nil}) (t/Set t/Int)
           :requires [[clojure.set :refer [select]]])
  ;would be nice
;  (is-tc-e (select some? #{1 2 3 nil}) (t/Set t/Int)
;           :requires [[clojure.set :refer [select]]])

  ;3rd arity
  (is-tc-e (select symbol? #{1}) (t/Set t/Int)
          :requires [[clojure.set :refer [select]]])
  (is-tc-err (select symbol? #{1}) (t/Set t/Bool)
             :requires [[clojure.set :refer [select]]])
)

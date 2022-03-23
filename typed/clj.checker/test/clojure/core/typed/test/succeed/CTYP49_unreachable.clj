(ns clojure.core.typed.test.succeed.CTYP49-unreachable
  (:require [typed.clojure :as t]))

(t/ann ^:no-check request 
       ['{:url t/Str, :method ':get} 
        -> (t/Atom1 '{:status t/Num, :body t/Str})])
(declare request)

(t/ann get-or-throw [t/Str -> '{:status t/Num :body t/Str}])
(defn get-or-throw [url]
  (let [doc (request {:url url :method :get})
        doc @doc
        _ (if (not= (:status doc) 200) (throw (Exception. (str "Got bad status: " doc))))]
    doc))

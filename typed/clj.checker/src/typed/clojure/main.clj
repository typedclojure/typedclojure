;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clojure.main
  (:require [clojure.edn :as edn]
            [typed.clojure :as t]))

(defn exec [{:keys [dirs platform] :or {platform :clj}}]
  (case platform
    :clj (t/check-dir-clj dirs)
    :cljs (t/check-dir-cljs dirs)))

(defn -main [& args]
  (try (exec (apply hash-map (map edn/read-string args)))
       (System/exit 0)
       (catch Throwable e
         (System/exit 1))))

(comment
  (exec {:dirs "src"})
  (-main ":dirs" "[\"typed\"]")
  )

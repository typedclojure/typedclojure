;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.java.analyzer.main
  (:gen-class)
  (:require typed.java.analyzer
            [clojure.java.io :as io]))

;; $ mkdir classes
;; $ clojure -M -e "(compile 'typed.java.analyzer.main)"
;; $ java -cp $(clojure -Spath):classes typed.java.analyzer.main
(defn -main []
  (println
    (#'typed.java.analyzer/class-is->class-info
      (io/input-stream
        (io/resource "java/lang/Object.class")))))

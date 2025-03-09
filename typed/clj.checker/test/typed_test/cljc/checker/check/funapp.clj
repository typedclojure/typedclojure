;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed-test.cljc.checker.check.funapp
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :refer :all]))

#_ ;;TODO
(deftest invoke-HMap-test
  (is-tc-e ({} :a))
  (is-tc-e ({} :a) nil)
  (is-tc-e ({} :a 1) t/Int)
  (is-tc-e ({} (ann-form :a t/Any) 1) t/Int)
  (is-tc-err ({:a 1} (ann-form :a t/Any) 1) t/Int)
  (is-tc-e ({:a 1} :a) t/Int)
  (is-tc-e ({:a 1} :b) nil)
  (is-tc-err ({:a 1} (ann-form :a t/Any)) t/Int)
  (is-tc-err ({:a 1} (ann-form :b t/Any)) t/Int))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clojure.jvm
  "JVM-specific annotations and operations.
  See typed.clojure for cross-platform ops."
  (:require clojure.core.typed
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.internal :refer [take-when]]))

(defmacro ann-interface [& args]
  `(clojure.core.typed/ann-interface ~@args))

(defmacro non-nil-return [& args]
  `(clojure.core.typed/non-nil-return ~@args))

(defmacro nilable-param [& args]
  `(clojure.core.typed/nilable-param ~@args))

(defmacro override-constructor [& args]
  `(clojure.core.typed/override-constructor ~@args))

(defmacro override-method [& args]
  `(clojure.core.typed/override-method ~@args))

(defmacro override-class [& args]
  (let [[binder args] (take-when vector? args)
        [nme args] (take-when symbol? args)
        _ (assert (symbol? nme) (str "Missing name in override-class" [nme args]))
        nme (or (when-some [^Class c (resolve nme)]
                  (when (class? c)
                    (-> c .getName symbol)))
                (throw (ex-info (str "Could not resolve class: " nme))))
        [opts args] (take-when map? args)
        opts (if opts
               (do (assert (empty? args) (str "Trailing args to override-class: " (pr-str args)))
                   opts)
               (apply hash-map args))
        this-ns (ns-name *ns*)]
    `(do ;; TODO runtime env
         #_
         (impl/add-rclass-env '~nme {:op :RClass})
         ;; type env
         (impl/with-clojure-impl
           ;;TODO implement reparsing on ns reload
           (impl/add-rclass '~nme (delay
                                    ((requiring-resolve 'typed.clj.checker.parse-unparse/with-parse-ns*)
                                     '~this-ns
                                     #((requiring-resolve 'typed.cljc.checker.base-env-helper/make-RClass)
                                       '~nme
                                       '~binder
                                       '~opts))))))))

(defmacro override-classes [& args]
  (assert (even? (count args)))
  `(do ~@(map (fn [[nme [frees & {:as opts}]]]
                `(override-class ~@(some-> (not-empty frees) vector) ~nme ~(or (not-empty opts) {})))
              (partition-all 2 args))))

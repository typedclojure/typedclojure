;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clojure.jvm
  "JVM-specific annotations and operations.
  See typed.clojure for cross-platform ops."
  #?(:clj (:refer-clojure :exclude [requiring-resolve]))
  (:require clojure.core.typed
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.internal :refer [take-when]]
            [typed.cljc.runtime.env-utils :refer [delay-type]]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])
            [clojure.core.typed.macros :as macros]))

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
        [opt args] (take-when map? args)
        opt (if opt
              (do (assert (empty? args) (str "Trailing args to override-class: " (pr-str args)))
                  opt)
              (apply hash-map args))
        this-ns (ns-name *ns*)]
    `(clojure.core.typed/tc-ignore
       (let [nme# (or (when-some [^#?(:cljr Type :default Class) c# (ns-resolve '~this-ns '~nme)]
                        (when (class? c#)
                          (-> c# #?(:cljr .FullName :default .getName) symbol)))
                      (throw (ex-info (str "Could not resolve class: " '~nme) {:class-name '~nme})))
             opts# (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                          :typed.clj.checker.parse-unparse/parse-type-in-ns '~this-ns)]
          ;; TODO runtime env
         #_
         (impl/add-rclass-env (impl/clj-checker) nme# {:op :RClass})
         ;; type env
         ;inline when-bindable-defining-ns
         (macros/when-bindable-defining-ns '~this-ns
           (impl/with-clojure-impl
             (impl/add-rclass (impl/clj-checker)
                              nme# (delay-type
                                     ((requiring-resolve 'typed.cljc.checker.base-env-helper/make-RClass)
                                      nme#
                                      '~binder
                                      '~opt
                                      opts#)))))))))

(defmacro override-classes [& args]
  (assert (even? (count args)))
  `(do ~@(map (fn [[nme [frees & {:as opt}]]]
                `(override-class ~@(some-> (not-empty frees) vector) ~nme ~(or (not-empty opt) {})))
              (partition-all 2 args))))

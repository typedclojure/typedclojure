#!/usr/bin/env bb

(ns regen-kondo-config
  (:require [babashka.curl :as curl]
            [clojure.pprint :as pp]))

(def base-config 
  {:hooks {:macroexpand (sorted-map)}})

;; don't forget to add a test in typed-example.clj-kondo-hooks

(def aliased-macros
  (-> ;; global annotation macros
      (zipmap '#{ann
                 ann-many
                 ann-protocol 
                 ann-datatype
                 ann-record
                 defalias
                 declare-datatypes
                 declare-protocols
                 declare-alias-kind
                 declare-names}
              (repeat 'typed.clj.runtime.clj-kondo-hooks/ignored))
      ;; macros in clojure.core.typed.macros
      (into (map (fn [macro]
                   {macro (symbol "clojure.core.typed.macros" (name macro))}))
            '#{;;TODO
               ;inst
               ;;TODO
               ;inst-ctor
               def
               fn
               loop
               ann-form
               tc-ignore
               defprotocol
               defn
               atom
               ref})))

(defn gen-config []
  (update-in base-config [:hooks :macroexpand]
             (fnil into (sorted-map))
             (map (fn [[macro rewrite]]
                    (zipmap (map #(symbol % (name macro))
                                 ["clojure.core.typed"
                                  "typed.clojure"])
                            (repeat rewrite))))
             aliased-macros))

(defn -main [& args]
  (spit "typed/clj.runtime/resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/config.edn"
        (with-out-str
          (pp/pprint (gen-config)))))

(when (= *file* (System/getProperty "babashka.file")) (-main))

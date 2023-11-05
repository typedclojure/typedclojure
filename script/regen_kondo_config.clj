#!/usr/bin/env bb

(ns regen-kondo-config
  (:require [babashka.curl :as curl]
            [clojure.pprint :as pp]))

(def base-config 
  '{:hooks {:macroexpand {}}
    ;:linters {:typed.clojure/unannotated {:level :error}}
    }
  )

(def aliased-macros
  '#{ann-form})

(defn gen-config []
  (update-in base-config [:hooks :macroexpand]
             (fnil into {})
             (map (fn [macro]
                    (zipmap (map #(symbol % (name macro))
                                 ["clojure.core.typed"
                                  "typed.clojure"])
                            (repeat
                              (symbol "clojure.core.typed.macros" (name macro))))))
             aliased-macros))

(defn -main [& args]
  (spit "typed/clj.runtime/resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/config.edn"
        (with-out-str
          (pp/pprint (gen-config)))))

(when (= *file* (System/getProperty "babashka.file")) (-main))

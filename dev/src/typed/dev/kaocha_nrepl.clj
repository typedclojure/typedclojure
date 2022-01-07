(ns typed.dev.kaocha-nrepl
  (:require [nrepl.cmdline :as nrepl]
            [kaocha.runner :as kaocha]))

(defn -main [& args]
  (future (nrepl/-main
            #_"
            Note:
              introducing other middleware makes vim-fireplace choose
              fipp for pprint, which doesn't play well with the delicately
              defined classes in type-rep."
            "--middleware" "[cider.nrepl/wrap-complete,cider.nrepl/wrap-info]"))
  (apply kaocha/-main args))

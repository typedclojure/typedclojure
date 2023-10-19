(ns typed.dev.repl
  (:require [clojure.main :as m]
            [nrepl.cmdline :as cmdline]
            [nrepl.server :as server]
            [cider.piggieback :as pback]
            cider.nrepl
            [nrepl.middleware :refer [set-descriptor!]]
            [cider.nrepl.middleware :as mw]
            [nrepl.middleware.session :as mw-session]
            typed.dev
            reply.main))

(def ^:dynamic init-ns-sentiel nil)

;; https://github.com/technomancy/leiningen/blob/efbe70923a7bb40c5fcef4e2be2bea893525667f/src/leiningen/repl.clj#L149 
(defn wrap-init-ns [init-ns]
  (with-local-vars [;; set-descriptor! currently only accepts a var
                    wrap-init-ns
                    (fn [handler]
                      (fn [{:keys [session] :as msg}]
                        (when-not (get @session #'init-ns-sentiel)
                          (require init-ns)
                          (swap! session assoc
                                 #'*ns* (the-ns init-ns)
                                 #'init-ns-sentiel true))
                        (handler msg)))]
    (doto wrap-init-ns
      (set-descriptor!
        {:requires #{#'mw-session/session}
         :expects #{"eval"}})
      (alter-var-root (constantly @wrap-init-ns)))))

(defn- resolve-or-fail [sym]
  (or (resolve sym)
      (throw (IllegalArgumentException. (format "Cannot resolve %s" sym)))))

(def nrepl-handler
  "CIDER's nREPL handler."
  (apply server/default-handler
         #_
         (wrap-init-ns 'typed.dev)
         (map resolve-or-fail mw/cider-middleware)))

(defn -main [& args]
  (when-not (System/getProperty "typed.clj.checker.parse-unparse.fipp-override")
    (System/setProperty "typed.clj.checker.parse-unparse.fipp-override" "true"))
  ;; sigh, how do you set init-ns with nrepl??
  #_
  (deref (-> ((requiring-resolve 'nrepl.cmdline/-main)
              "--middleware"
              "[cider.nrepl/cider-middleware,cider.piggieback/wrap-cljs-repl]")
             future)
         2000
         nil)
  #_
  (m/repl
    :init (fn []
            (apply require m/repl-requires)
            (doto 'typed.dev
              require
              in-ns)))
  (let [server (server/start-server :port 0
                                    :handler nrepl-handler)]
    (cmdline/save-port-file server {})
    #_
    (cmdline/dispatch-commands {:connect true
                                :port (:port server)})
    (reply.main/launch
      {:attach (str (:port server))
       :custom-eval `(do (require 'typed.dev)
                         (in-ns 'typed.dev))})
    (System/exit 0))
  )

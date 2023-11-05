(ns typed.clj.runtime.clj-kondo-hooks)

(defmacro ignored [& _])
(defmacro inst [e & _] e)
(defmacro inst-ctor [e & _] e)

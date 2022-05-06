{:ext [typed.ext.malli]
 :var-type-providers [#?(:clj typed.clj.provider.malli/var-type
                         #_#_:cljs typed.cljs.provider.malli/var-type)]
 :meta-ann-type-providers {:typed.clojure.malli #?(:clj typed.clj.provider.malli/malli-meta-ann->Type
                                                   #_#_:cljs typed.cljs.provider.malli/malli-meta-ann->Type)}}

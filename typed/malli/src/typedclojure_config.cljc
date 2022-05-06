{:ext [typed.ext.malli]
 :var-type-providers [#?(:clj typed.clj.provider.malli/var-type
                         :cljs typed.cljs.provider.malli/var-type)]
 :type-providers #?(:clj {:typed.clojure.malli typed.clj.provider.malli/malli-meta-ann->Type}
                    :cljs {})}

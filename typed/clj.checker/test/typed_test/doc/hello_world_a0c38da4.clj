;; doc-test: id=a0c38da4 type=fail version=2
(ns ^:typed.clojure typed-test.doc.hello-world-a0c38da4
  (:require [typed.clojure :as t]))

;; start-markdown:
(t/ann intentional-error [:-> t/Num])
(defn intentional-error []
  (+ "Hello, world!"))
;; end-markdown:

;; start-result:
(comment
{:type-errors
 [{:data
   {:args-results
    [{:proposition-set {:else ff, :then tt},
      :type (t/Val "Hello, world!")}],
    :expected-result {:type t/Num},
    :fn-type
    (t/IFn
     [Long :* :-> Long]
     [Double Double :* :-> Double]
     [t/AnyInteger :* :-> t/AnyInteger]
     [t/Num :* :-> t/Num])},
   :env
   {:column 3,
    :file
    "file:typed/clj.checker/test/typed_test/doc/hello_world_a0c38da4.clj",
    :line 8},
   :form (+ "Hello, world!"),
   :message
   "Function + could not be applied to arguments:\n\n\nDomains:\n\tLong :*\n\tDouble Double :*\n\tt/AnyInteger :*\n\tt/Num :*\n\nArguments:\n\t(t/Val \"Hello, world!\")\n\nRanges:\n\tLong\n\tDouble\n\tt/AnyInteger\n\tt/Num\n\nwith expected type:\n\tt/Num\n\n",
   :type-error :typed.clojure/app-type-error}]}
)
;; end-result:

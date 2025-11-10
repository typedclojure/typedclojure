# Hello, world!

<!-- doc-test: id=a0c38da4 type=fail version=2 -->
<!-- doc-test: id=a0c38da4 type=fail version=2 -->
```clojure
(t/ann intentional-error [:-> t/Num])
(defn intentional-error []
  (+ "Hello, world!"))
```
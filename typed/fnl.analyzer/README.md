# typed.fnl.analyzer

Analyzer for Fennel code. Provides integration with typed.fnl.reader and typed.cljc.analyzer.

## Usage

```clojure
(require '[typed.fnl.analyzer :as fnl-ana]
         '[typed.fnl.reader :as fnl-reader])

;; Analyze a Fennel form
(let [form (fnl-reader/read-string "(fn [x] x)")
      env (fnl-ana/empty-env)
      opts (fnl-ana/default-opts)]
  (fnl-ana/analyze form env opts))

;; Analyze a Fennel file
(fnl-ana/analyze-fennel-file "path/to/file.fnl" 
                             (fnl-ana/empty-env)
                             (fnl-ana/default-opts))
```

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors

Distributed under the Eclipse Public License 1.0

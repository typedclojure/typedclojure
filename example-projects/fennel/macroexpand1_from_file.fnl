#!/usr/bin/env fennel
;; Macroexpand-1 wrapper that works with any Fennel file
;; Expands file-local macros using the compiler scope
;;
;; Usage: macroexpand1_from_file.fnl <fennel-file> '<form-to-expand>'
;; Example: macroexpand1_from_file.fnl src/typed-fennel-demo.fnl '(ann-forwarded demo-func t/Str)'
;; Output: (t.ann-form demo-func [t/Str "->" t/Str])

(local fennel-file (or (. arg 1) ""))
(local form-to-expand (or (. arg 2) ""))

(when (or (= fennel-file "") (= form-to-expand ""))
  (io.stderr:write "ERROR: Usage: macroexpand1_from_file.fnl <fennel-file> '<form>'\n")
  (os.exit 1))

(local fennel (require :fennel))
(local compiler (. package.loaded "fennel.compiler"))

;; Read and compile the file to get the scope
(local (file-ok file-content) 
  (pcall #(with-open [f (io.open fennel-file :r)]
            (f:read "*all"))))

(when (not file-ok)
  (io.stderr:write (.. "ERROR: Could not read file: " fennel-file "\n"))
  (os.exit 1))

;; Create an empty scope for top-level forms
(local scope (fennel.scope))

;; Compile the file to populate the scope with macros
(local (compile-ok _lua-code) 
  (pcall fennel.compileString file-content {:scope scope :filename fennel-file}))

(when (not compile-ok)
  (io.stderr:write (.. "ERROR: Failed to compile file: " fennel-file "\n"))
  (os.exit 1))

;; Parse the form to expand
(local parser (fennel.parser form-to-expand))
(local (parse-ok ast) (parser))

(when (not parse-ok)
  (io.stderr:write (.. "ERROR: Failed to parse form: " form-to-expand "\n"))
  (os.exit 1))

;; Macroexpand once using compiler.macroexpand with once=true
(local expanded (compiler.macroexpand ast scope true))

;; Print the expanded form using fennel.view for proper serialization
(print (fennel.view expanded))

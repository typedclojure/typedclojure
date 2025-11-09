#!/usr/bin/env fennel
;; Compile a Fennel file and return information about the compiler scope
;;
;; Usage: compile_file_with_scope.fnl <fennel-file-path>
;; Output: EDN map with :forms (vector of forms) and :macros (set of macro names)

(local fennel-file (or (. arg 1) ""))

(when (= fennel-file "")
  (io.stderr:write "ERROR: No file provided\n")
  (os.exit 1))

(local fennel (require :fennel))

;; Read the file content
(local (file-ok file-content) 
  (pcall #(with-open [f (io.open fennel-file :r)]
            (f:read "*all"))))

(when (not file-ok)
  (io.stderr:write (.. "ERROR: Could not read file: " fennel-file "\n"))
  (os.exit 1))

;; Create a scope and compile the file
(local scope (fennel.scope))
(local (compile-ok lua-code-or-err) 
  (pcall fennel.compileString file-content 
         {:scope scope :filename fennel-file}))

(when (not compile-ok)
  (io.stderr:write (.. "ERROR: Compilation failed: " lua-code-or-err "\n"))
  (os.exit 1))

;; Parse the file to get all forms
(local parser (fennel.parser file-content fennel-file))
(var forms [])
(var (parse-ok form) (parser))
(while parse-ok
  (table.insert forms (fennel.view form))
  (set (parse-ok form) (parser)))

;; Extract macro names from the scope
(var macro-names [])
(each [name _func (pairs scope.macros)]
  (when (not= name ":") ;; Skip special entries
    (table.insert macro-names name)))

;; Output as EDN
(local forms-str (table.concat forms " "))
(local macros-str (table.concat macro-names " "))
(print (.. "{:forms [" forms-str "] :macros #{" macros-str "}}"))

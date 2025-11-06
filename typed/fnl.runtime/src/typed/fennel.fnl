;; Fennel macro module for Typed Clojure
;; This module provides type annotation macros for Fennel code
;; 
;; Design: Typed Fennel uses purely static analysis. Macros expand to expressions
;; that preserve the original code while providing type information to the type
;; checker. No runtime side effects or exfiltration mechanisms are used.

(fn alias [name type-expr]
  "Introduce a type alias in the current scope.
  
  Usage: (alias MyType (U nil Str))
  
  The alias is purely for static analysis - this macro expands to nil."
  nil)

(fn ann [sym type-expr]
  "Declare the type of a variable in the current scope.
  
  Usage: (ann my-var Str)
  
  This associates a type with a variable for static analysis purposes.
  The macro expands to nil and has no runtime effect."
  nil)

(fn ann-form [form type-expr]
  "Ascribe a type to the form.
  
  Usage: (ann-form (+ 1 2) Int)
  
  This tells the type checker to validate that form has the given type.
  The macro expands to the form itself, preserving runtime behavior."
  form)

(fn ann-module [& args]
  "Declare a type for the current or named module and type alias exports.
  
  Usage: 
    (ann-module :name \"foo.bar\"
                :type (HMap {:foo Str :bar Int})
                :aliases [FooBar BazQux])
  
    (ann-module :type SomeType
                :aliases :all)  ; export all top-level type aliases
  
  Options:
    :name - Optional module name (defaults to current module)
    :type - Type for the module's return value
    :aliases - Vector of type alias names to export, or :all for all top-level aliases
  
  This macro expands to nil and provides metadata for static analysis only."
  nil)

;; Export macro functions
{:alias alias
 :ann ann
 :ann-form ann-form
 :ann-module ann-module}

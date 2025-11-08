;; Demo file showing usage of typed.fennel macros

(import-macros t :typed.fennel)

;; Declare a type alias for readability
(t.alias StrFn [:Fn [t.Str :-> t.Str]])

;; Annotate the demo-func with a type signature: String -> String
(t.ann demo-func StrFn)

;; Define the demo function
(fn demo-func [x]
  x)

;; Use ann-form to verify a form has the expected type
(print (t.ann-form (demo-func "Hello from Typed Fennel!") t.Str))

;; Declare module type and exports (for illustration)
(t.ann-module :type {:demo-func StrFn}
              :aliases [StrFn])
{: demo-func}

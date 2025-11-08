;; Demo file showing usage of typed.fennel macros

(import-macros t :typed.fennel)

;; Declare a type alias for readability
(t.alias StrFn [t/Str :-> t/Str])

;; Annotate the demo-func with a type signature: String -> String
(t.ann demo-func StrFn)

;; Define the demo function
(fn demo-func [x]
  x)

;; Use ann-form to verify a form has the expected type
(print (t.ann-form (demo-func "Hello from Typed Fennel!") t/Str))

;; Types are reified as syntax so they can be composed by other macros
(macro ann-forwarded [form type] `(t.ann-form ,form [,type :-> ,type]))

(ann-forwarded demo-func t/Str)

;; Declare module type and exports (for illustration)
(t.ann-module :type {:demo-func StrFn}
              :aliases [StrFn])
{: demo-func}

# Public interface for type errors as data

`clojure.core.typed/check-{ns,form}-info` will return errors as data.

## :typed.clojure/type-mismatch-error

```
- :data :expected-type   the expected type, as data, relative to the current ns
- :data :actual-type     the actual type, as data, relative to the current ns
```

Example:

```clojure
(check-form-info '(t/ann-form 1 t/Str))
=>
{:type-errors
 [{:type-error :typed.clojure/type-mismatch-error,
   :env
   {:line 329,
    :column 38,
    :file
    "clojure/core/typed/test/symbolic_closures.clj"},
   :form 1,
   :data {:expected-type t/Str, :actual-type (t/Val 1)},
   :message
   "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \t(t/Val 1)"}],
 :ret {:t {}, :fl {:then {}, :else {}}, :o {}, :opts {}},
 :out-form 1}
```

## :typed.clojure/app-type-error

```
- :data :fn-type           the type of the function, as data, relative to the current ns
- :data :args-results      the type checking results of the arguments, as data, relative to the current ns.
- :data :expected-result   (optional) the expected type checking result of calling the function with the arguments, as data, relative to the current ns
```

Example:

```clojure
(check-form-info '#(+ 1 "oops"))
=>
{:type-errors
 [{:type-error :typed.clojure/app-type-error,
   :env
   {:line 61,
    :column 20,
    :file
    "clojure/core/typed/test/error_msg.clj"},
   :form (+ 1 "oops"),
   :data
   {:fn-type
    (t/IFn
     [Long :* :-> Long]
     [(t/U Double Long) :* :-> Double]
     [t/AnyInteger :* :-> t/AnyInteger]
     [t/Num :* :-> t/Num]),
    :args-results
    [{:type (t/Val 1), :filter-set {:then tt, :else ff}}
     {:type (t/Val "oops"), :filter-set {:then tt, :else ff}}],
    :expected-result {:type t/Infer}},
   :message
   "Function + could not be applied to arguments:\n\n\nDomains:\n\tLong :*\n\t(t/U Double Long) :*\n\tt/AnyInteger :*\n\tt/Num :*\n\nArguments:\n\t(t/Val 1) (t/Val \"oops\")\n\nRanges:\n\tLong\n\tDouble\n\tt/AnyInteger\n\tt/Num\n\n"}],
 :ret
 {:t
  {:types
   #{{:types
      [{:dom [],
        :rng {:t {}, :fl {:then {}, :else {}}, :o {}},
        :rest nil,
        :drest nil,
        :kws nil,
        :prest nil,
        :pdot nil,
        :regex nil,
        :kind :fixed}]}
     {:the-class clojure.lang.Fn,
      :poly? nil,
      :variances nil,
      :replacements {},
      :unchecked-ancestors #{}}}},
  :fl {:then {}, :else {}},
  :o {},
  :opts {}},
 :out-form (fn* ([] (clojure.lang.Numbers/add 1 "oops")))}


;;with non-trivial :expected-result
(check-form-info '#(t/ann-form (+ 1 "oops") nil))
=>
{:type-errors
 [{:type-error :typed.clojure/app-type-error,
   :env
   {:line 61,
    :column 32,
    :file
    "clojure/core/typed/test/error_msg.clj"},
   :form (+ 1 "oops"),
   :data
   {:fn-type
    (t/IFn
     [Long :* :-> Long]
     [(t/U Double Long) :* :-> Double]
     [t/AnyInteger :* :-> t/AnyInteger]
     [t/Num :* :-> t/Num]),
    :args-results
    [{:type (t/Val 1), :filter-set {:then tt, :else ff}}
     {:type (t/Val "oops"), :filter-set {:then tt, :else ff}}],
    :expected-result {:type nil}},
   :message
   "Function + could not be applied to arguments:\n\n\nDomains:\n\tLong :*\n\t(t/U Double Long) :*\n\tt/AnyInteger :*\n\tt/Num :*\n\nArguments:\n\t(t/Val 1) (t/Val \"oops\")\n\nRanges:\n\tLong\n\tDouble\n\tt/AnyInteger\n\tt/Num\n\nwith expected type:\n\tnil\n\n"}],
 :ret
 {:t
  {:types
   #{{:types
      [{:dom [],
        :rng {:t {:val nil}, :fl {:then {}, :else {}}, :o {}},
        :rest nil,
        :drest nil,
        :kws nil,
        :prest nil,
        :pdot nil,
        :regex nil,
        :kind :fixed}]}
     {:the-class clojure.lang.Fn,
      :poly? nil,
      :variances nil,
      :replacements {},
      :unchecked-ancestors #{}}}},
  :fl {:then {}, :else {}},
  :o {},
  :opts {}},
 :out-form (fn* ([] (clojure.lang.Numbers/add 1 "oops")))}
```

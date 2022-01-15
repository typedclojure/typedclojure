(ns typed.cljc.checker.test-utils
  (:require [clojure.core.typed.errors :as err]))

(defn tc-e 
  "Type check an an expression in namespace that :refer's
  all of clojure.core.typed (aliased to t) and aliases clojure.core
  to core.

  Takes one form and then options, and returns the inferred type if the form checks
  with the expected input/output types according to the provided options.
  
  The first form in the options can be a static type syntax scoped
  in the new namespace. This is disambiguated with a call to keyword?
  (literal keywords aren't valid type syntax).
  
  eg. (tc-e (+ 1 1) Num)
      ;=> Num

  Keyword Options:

    :expected-ret An expected ret, evaluated in the current namespace (not the new
                  one that refers c.c.t). Cannot be provided in combination with the implicit
                  first option as a type, as above.
    :ret          Check the return TCResult of this expression against this ret. Evaluated
                  in the current namespace."
  [tc-common* frm & opts]
  (let [[opts t has-t?] (if (and opts (not (keyword? (first opts))))
                          [(rest opts) (first opts) true]
                          [opts])
        _ (assert (even? (count opts))
                  "Uneven arguments to tc-e")
        {:as opts} opts
        _ (assert (not (and has-t? (contains? opts :expected)))
                  "Can't provide both implicit expected type and :expected kw to tc-e")
        has-t? (or has-t? (contains? opts :expected))
        t (or t (:expected opts))
        has-ret? (contains? opts :ret)
        _ (assert (not (and has-t? (contains? opts :expected-ret)))
                  "Can't provide both expected type and expected ret")
        actual-ret (gensym 'ret)]
    `(let [{~actual-ret :ret ex# :ex delayed-errors# :delayed-errors} ~(tc-common* frm (assoc opts
                                                                                              :expected-syntax {:provided? has-t?
                                                                                                                :syn t}))
           _# (some-> ex# throw)
           _# (some-> (seq delayed-errors#) err/print-errors!)
           _# ~(when has-ret?
                 `(assert (= ~actual-ret ~(:ret opts))))
           _# (assert ~actual-ret)] ;; should be a true value
       ;; return inferred type
       ~actual-ret)))

(defn tc-err
  "Like tc-e, except returns true if the form fails to type check."
  [tc-common* frm & opts]
  (let [[opts t has-t?] (if (and opts (not (keyword? (first opts))))
                          [(rest opts) (first opts) true]
                          [opts])
        _ (assert (even? (count opts))
                  "Uneven arguments to tc-err")
        {:as opts} opts
        _ (assert (not (and has-t? (contains? opts :expected)))
                  "Can't provide both implicit expected type and :expected kw to tc-err")
        has-t? (or has-t? (contains? opts :expected))
        t (or t (:expected opts))
        has-ret? (contains? opts :ret)
        _ (assert (not (and has-t? (contains? opts :expected-ret)))
                  "Can't provide both expected type and expected ret")
        actual-ret (gensym 'ret)]
    `(let [{~actual-ret :ret ex# :ex delayed-errors# :delayed-errors :as check-result#}
           ~(tc-common* frm 
                        (assoc 
                          opts
                          :expected-syntax {:provided? has-t?
                                            :syn t}))]
       ~(when has-ret?
          ;;FIXME do we want the opposite here?
          `(assert (= ~actual-ret ~(:ret opts))))
       (-> check-result#
           (select-keys #{:ex :delayed-errors})
           (cond->
             (some-> ex# ex-data err/top-level-error? not)
             (dissoc :ex))
           not-empty))))

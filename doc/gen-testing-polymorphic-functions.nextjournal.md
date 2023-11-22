# Generative testing and instrumentation for polymorphic functions with typed.clj.spec

Usually spec does not support polymorphic types, so library functions like `map` and `filter` are usually ascribed imprecise monomorphic types. For example, if `map` is `((a->b)a*->b*)` in a static type system, then it becomes `((any?->any?)any?*->any?*)` in spec.

While the second type is great for *instrumentation*, it's not helpful for generative testing.

[typed.clj.spec](https://github.com/typedclojure/typedclojure/blob/main/typed/clj.spec/README.md) attempts to bring together both worlds by enhancing spec with polymorphic types that can be used for generative testing, and tools to instantiate them to also instrument from the same types.

## Preliminaries

We'll need typed.clj.spec and the typedclojure fork of spec2.

```edn no-exec id=ffcf0396-b3f9-40e6-a0c2-654401879781
{:deps {org.clojure/clojure {:mvn/version "1.10.1"}
        org.clojure/data.csv {:mvn/version "1.0.0"}
        org.clojure/test.check {:mvn/version "1.1.0"}
        org.clojure/alpha.spec {:git/url "https://github.com/typedclojure/spec-alpha2.git" 
                                :git/sha "9da58ec60f5a4a3bfc61fa19f54bf1d160b49dfc"}
        org.typedclojure/typed.clj.spec
        {:git/url "https://github.com/typedclojure/typedclojure"
         :deps/root "typed/clj.spec"
         :git/sha "d9ee00ce05a32691a67e99f14a21cc70481f6dab"}}}
```

Let's use the following requires.

```clojure id=bbe7b1ed-8b90-48f7-8f1e-45166b207327
(require '[clojure.alpha.spec :as s]
         '[clojure.alpha.spec.test :as stest]
         '[clojure.alpha.spec.gen :as gen]
         '[typed.clj.spec :as t]
         '[clojure.pprint :refer [pprint]]
         '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])
```

## The Problem

Let's say we're trying to spec the following function.

```clojure id=643525ec-5329-4e44-b982-594428bcf872
(defn my-map
  "It's just clojure.core/map with 2 args, to keep this article easy to follow."
  [f c]
  (map f c))
```

As the community-driven library of core specs [speculative](https://github.com/borkdude/speculative/blob/4e773794a4065a84bdadd997516e52c76ab51b1f/src/speculative/core.cljc#L297-L302) shows us, there isn't much we can say about `my-map` in spec1 or spec2. We're using spec2 -- here's what I would use:

```clojure id=287ae356-4fc1-4a26-a146-8cc91d0d6244
(s/def
  ::map1-mono
  (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x any?)
                                     :ret any?)
                        :coll (s/nilable (s/every any?)))
           :ret (s/every any?)))
```

As the name suggests, this is a monomorphic types (no type variables). For comparison, here's the 2-arg static type in Typed Clojure:

```clojure id=147c78c0-7a08-438c-809c-763fedb0bf46
#_
(ann clojure.core/map
     (All [a b]
        [[a -> b] (U nil (Seqable a)) -> (t/ASeq b)]))
```

The spec version is missing key relationships between its arguments and return value. Does this really matter for the kinds of problems spec is trying to achieve? It depends what you want to use the type for. If it's for generative testing of `map`, then I claim yes, it makes a big difference.

```clojure id=ab1ccb72-cd40-4977-8aec-d9e060fccf33
(s/def
  ::map1-mono+count
  (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x any?)
                                     :ret any?)
                        :coll (s/nilable (s/every any?)))
           :fn #(= (bounded-count 30 (:ret %))
                   (bounded-count 30 (-> % :args :coll)))
           :ret (s/every any?)))
```

The monomorphic type of map is broad enough to be a supertype of all instantiations of map---this means we should expect my-map to be an instance of it (which it is):

```clojure id=60099d5e-7423-474f-890a-5245d680ea04
(s/valid? ::map1-mono my-map) ;; my-map is a correct impl of map
```

Unfortunately, there are many functions that are not even close to map which also are subtypes of this type. Let's define a few incorrect "map" definitions, and see which ones validate as `::map1-mono`.

```clojure id=332c361a-ea6b-4e96-9c9f-601014732399
(s/def
  ::map1-poly
  (t/all :binder (t/binder
                   :x (t/bind-tv)
                   :y (t/bind-tv))
         :body (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (t/tv :x))
                                                  :ret (t/tv :y))
                                     :coll (s/nilable (s/every (t/tv :x))))
                        :ret (s/every (t/tv :y)))))
```

```clojure id=fbf66681-af19-4735-a81b-f04311fcb826
(s/def
  ::map1-poly+count
  (t/all :binder (t/binder
                   :x (t/bind-tv)
                   :y (t/bind-tv))
         :body (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (t/tv :x))
                                                  :ret (t/tv :y))
                                     :coll (s/nilable (s/every (t/tv :x))))
                        :fn #(= (bounded-count 30 (:ret %))
                                (bounded-count 30 (-> % :args :coll)))
                        :ret (s/every (t/tv :y)))))
```

```clojure id=30527e76-55ae-497e-850a-703859daa5ff
;; make pretty graphs https://nextjournal.com/nextjournal/clojure-environment#plotting
(defn false-positive-benchmark
  [spec]
  {:pre [(qualified-keyword? spec)]}
  (let [syms (into #{} (comp (mapcat (comp vals #(do (require %)
                                                     (ns-publics %))))
                             (filter var?)
                             (remove (comp :macro meta))
                             (map symbol)
                             (distinct))
                   ['clojure.core
                    'clojure.set
                    'clojure.data
                    'clojure.string
                    'clojure.xml
                    'clojure.java.io
                    'clojure.zip
                    'clojure.repl])
        cases (concat syms
                      #_(map (fn [sym]
                               `(comp ~sym map))
                             syms))
        map-unit-tests (fn [map]
                         (try (and (= [] (map identity []))
                                   (= [1] (map identity [1]))
                                   (= [2] (map inc [1]))
                                   (= (range 1 11) (map inc (range 10)))
                                   ;; detect keep/filter/remove
                                   (= [nil true nil] (map (some-fn symbol? (constantly nil)) [1 'sym :kw]))
                                   ;; detect mapcat
                                   (= [[1] ['sym] [:kw]] (map vector [1 'sym :kw])))
                              (catch Throwable e false)))]
    (doall
      (pmap (fn [c]
              {:pre [(qualified-symbol? c)]}
              (binding [*out* (java.io.PrintStream.
                                (java.io.OutputStream/nullOutputStream))
                        *err* (java.io.PrintStream.
                                (java.io.OutputStream/nullOutputStream))]
                (let [f (resolve c)
                      _ (assert (var? f))
                      start (System/nanoTime)
                      v? (s/valid? spec f)
                      elapsed-ns (- (System/nanoTime) start) 
                      m? (map-unit-tests f)
                      result (if v?
                               (if m?
                                 :true-positive
                                 :false-positive)
                               (if m?
                                 :false-negative
                                 :true-negative))]
                  {:case c
                   :spec spec
                   :elapsed-ns elapsed-ns
                   :result result
                   :is-map? m?
                   :valid? v?})))
            cases))))


(defn false-positive-benchmarks [specs]
  (into #{} (mapcat false-positive-benchmark) specs))

(defn plot-bench
  ([specs] (plot-bench specs (false-positive-benchmarks specs)))
  ([specs ress]
   (let [ress (group-by #(select-keys % [:spec :result]) ress)]
     (prn (keys ress))
     ^{:nextjournal/viewer :plotly}
     {:data (map (fn [result]
                   {:x specs
                    :y (map (fn [spec]
                              (-> {:spec spec
                                   :result result}
                                  ress
                                  count))
                            specs)
                    #_#_:text (map (fn [spec]
                                 (name spec)
                                 #_(->> {:spec spec
                                       :result result}
                                      ress 
                                      (map :case)
                                      (str/join "\n")))
                               specs)
                    :type "bar"
                    :name (name result)})
                 [:false-negative
                  :false-positive])
      :layout {;:barmode "stack"
               :title "Incorrectly validated values for specs"
               :autosize false :width 600 :height 1000 
               ;:xaxis1 {:title "spec name"}
               :xaxis {:tickangle -45}
               :yaxis1 {:title "false results"}}})))
```

```clojure id=2837b882-9b87-4761-bdbd-f6e08473b37f
(def all-map-specs [::map1-mono ::map1-mono+count ::map1-poly ::map1-poly+count])
(defonce bench-results (false-positive-benchmarks all-map-specs))
(plot-bench all-map-specs bench-results)
```

[result][nextjournal#output#2837b882-9b87-4761-bdbd-f6e08473b37f#result]

Notice that as long as

```clojure id=988fbdc9-3388-453b-acb3-6bc89e37d1c3
(->> bench-results
     (group-by :spec)
     (map (fn [[k v]] [k (into (sorted-map)
                               (comp
                                  (remove (comp #{:true-positive :true-negative}
                                                first))
                                  (map (fn [[k v]]
                                         [k (mapv :case v)])))
                               (group-by :result v))]))
     (into (sorted-map)))
```

To demonstrate how broad the monomorphic type is, 

```clojure id=327e0720-b0d1-4c88-89f1-a183180d90fa
(def csv-data 
   (->> bench-results
      (remove (comp #{:true-positive :true-negative} :result))
      (sort-by (juxt :spec :result))))
(def csv-keys [:spec :result :case])
(clojure.pprint/print-table
 csv-keys
 csv-data)

(with-open [writer (io/writer "/results/out-file.csv")]
  (csv/write-csv writer
                 (cons csv-keys (map (apply juxt csv-keys) csv-data))))
```

[out-file.csv][nextjournal#output#327e0720-b0d1-4c88-89f1-a183180d90fa#out-file.csv]

```clojure id=4a2e321c-af65-48c8-92d8-470bddb914b0
(s/valid? ::map1-poly my-map)
```

```clojure id=dd2d912e-2b32-4250-9896-f47fc789ec3e
(s/def my-map (t/inst ::map1-poly {:x any? :y any?}))
(pprint (s/describe (s/get-spec `my-map)))
```

```clojure id=7ee49629-229f-404f-af2b-34a7aa852cdd
(stest/instrument `my-map)
(try (my-map 1 2)
     (catch Throwable e
       (pprint e))
     (finally
       (stest/unstrument `my-map)))
```


[nextjournal#output#2837b882-9b87-4761-bdbd-f6e08473b37f#result]:
<https://nextjournal.com/data/QmazP26VDH2Z1JWq876z4XQ4GqYBCce582SnEwTBVAb8L1?content-type=application/vnd.plotly.v1%2Bjson&node-id=2837b882-9b87-4761-bdbd-f6e08473b37f&node-kind=output>

[nextjournal#output#327e0720-b0d1-4c88-89f1-a183180d90fa#out-file.csv]:
<https://nextjournal.com/data/QmWhqKknW8FifJ7kHZUKXWBuLpcKKWu1hP4y3DK4Xr89nr?content-type=text/csv&node-id=327e0720-b0d1-4c88-89f1-a183180d90fa&filename=out-file.csv&node-kind=output>

<details id="com.nextjournal.article">
<summary>This notebook was exported from <a href="https://nextjournal.com/a/NMRwUi4RsUdyg41UTcUZG?change-id=Cqmn57tmYucdG3wivgt6gu">https://nextjournal.com/a/NMRwUi4RsUdyg41UTcUZG?change-id=Cqmn57tmYucdG3wivgt6gu</a></summary>

```edn nextjournal-metadata
{:article
 {:settings nil,
  :nodes
  {"147c78c0-7a08-438c-809c-763fedb0bf46"
   {:compute-ref #uuid "f12bf66d-b048-43ae-8dfb-d9bcddcf4f90",
    :exec-duration 3,
    :id "147c78c0-7a08-438c-809c-763fedb0bf46",
    :kind "code",
    :name "ann clojure.core/map",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "2837b882-9b87-4761-bdbd-f6e08473b37f"
   {:compute-ref #uuid "70a0a7df-a868-4940-903c-ecf34aaddd63",
    :exec-duration 312,
    :id "2837b882-9b87-4761-bdbd-f6e08473b37f",
    :kind "code",
    :name "Plots",
    :output-log-lines {:stdout 2},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "287ae356-4fc1-4a26-a146-8cc91d0d6244"
   {:compute-ref #uuid "cb25bea5-ea55-404d-a4d0-557e46abc3c6",
    :exec-duration 15,
    :id "287ae356-4fc1-4a26-a146-8cc91d0d6244",
    :kind "code",
    :name "::map1-mono",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "30527e76-55ae-497e-850a-703859daa5ff"
   {:compute-ref #uuid "2bdee41b-5729-4bdc-9f79-049c0a27a37b",
    :exec-duration 38,
    :id "30527e76-55ae-497e-850a-703859daa5ff",
    :kind "code",
    :name "Benchmarks",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "327e0720-b0d1-4c88-89f1-a183180d90fa"
   {:compute-ref #uuid "115000ae-d909-434f-8b0a-5d34e6128ae5",
    :exec-duration 298,
    :id "327e0720-b0d1-4c88-89f1-a183180d90fa",
    :kind "code",
    :output-log-lines {:stdout 32},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "332c361a-ea6b-4e96-9c9f-601014732399"
   {:compute-ref #uuid "130ca3a1-47e2-4bef-ae26-9c62134948e1",
    :exec-duration 9,
    :id "332c361a-ea6b-4e96-9c9f-601014732399",
    :kind "code",
    :name "::map1-poly",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "4a2e321c-af65-48c8-92d8-470bddb914b0"
   {:compute-ref #uuid "ed542727-6519-4f82-bdb1-266f4e94b94b",
    :exec-duration 557,
    :id "4a2e321c-af65-48c8-92d8-470bddb914b0",
    :kind "code",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "60099d5e-7423-474f-890a-5245d680ea04"
   {:compute-ref #uuid "061c35c4-bd41-4881-9478-b105954375c3",
    :exec-duration 1437,
    :id "60099d5e-7423-474f-890a-5245d680ea04",
    :kind "code",
    :name "valid? ::map1-mono",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "643525ec-5329-4e44-b982-594428bcf872"
   {:compute-ref #uuid "849c1e23-4f11-4deb-8b12-a8a2e9bb9021",
    :exec-duration 42,
    :id "643525ec-5329-4e44-b982-594428bcf872",
    :kind "code",
    :name "my-map",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "7ee49629-229f-404f-af2b-34a7aa852cdd"
   {:compute-ref #uuid "a11ba1d6-61dc-4615-8498-4a665f7309bf",
    :exec-duration 392,
    :id "7ee49629-229f-404f-af2b-34a7aa852cdd",
    :kind "code",
    :output-log-lines {:stdout 35},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "988fbdc9-3388-453b-acb3-6bc89e37d1c3"
   {:compute-ref #uuid "e875102d-eaa8-47ee-aa74-df9862d4ac50",
    :exec-duration 222,
    :id "988fbdc9-3388-453b-acb3-6bc89e37d1c3",
    :kind "code",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "a5a845ef-5cc0-4dda-a123-e5365baa1500"
   {:environment
    [:environment
     {:article/nextjournal.id
      #uuid "5b45eb52-bad4-413d-9d7f-b2b573a25322",
      :change/nextjournal.id
      #uuid "5f045c36-90bd-428b-a26c-b59fa0a2e1db",
      :node/id "0ae15688-6f6a-40e2-a4fa-52d81371f733"}],
    :id "a5a845ef-5cc0-4dda-a123-e5365baa1500",
    :kind "runtime",
    :language "clojure",
    :resources {:machine-type "n1-highcpu-4"},
    :type :prepl,
    :runtime/mounts
    [{:src [:node "ffcf0396-b3f9-40e6-a0c2-654401879781"],
      :dest "/deps.edn"}]},
   "ab1ccb72-cd40-4977-8aec-d9e060fccf33"
   {:compute-ref #uuid "82464d27-f385-4960-8336-b113eef8cdfc",
    :exec-duration 10,
    :id "ab1ccb72-cd40-4977-8aec-d9e060fccf33",
    :kind "code",
    :name "::map1-mono+count",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "bbe7b1ed-8b90-48f7-8f1e-45166b207327"
   {:compute-ref #uuid "78f486ee-419f-4e02-bf79-78168081fce1",
    :exec-duration 2352,
    :id "bbe7b1ed-8b90-48f7-8f1e-45166b207327",
    :kind "code",
    :name "requires",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "dd2d912e-2b32-4250-9896-f47fc789ec3e"
   {:compute-ref #uuid "098ce1cb-9ad4-4d0e-a8e5-ad055760e0c4",
    :exec-duration 279,
    :id "dd2d912e-2b32-4250-9896-f47fc789ec3e",
    :kind "code",
    :output-log-lines {:stdout 10},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "fbf66681-af19-4735-a81b-f04311fcb826"
   {:compute-ref #uuid "9c5e038b-5f07-49cd-8df9-2bbcca2e8ea2",
    :exec-duration 6,
    :id "fbf66681-af19-4735-a81b-f04311fcb826",
    :kind "code",
    :output-log-lines {},
    :runtime [:runtime "a5a845ef-5cc0-4dda-a123-e5365baa1500"]},
   "ffcf0396-b3f9-40e6-a0c2-654401879781"
   {:id "ffcf0396-b3f9-40e6-a0c2-654401879781",
    :kind "code-listing",
    :name "deps.edn"}},
  :nextjournal/id #uuid "02fb35a8-27c7-4593-95f0-84521e49b793",
  :article/change
  {:nextjournal/id #uuid "5fe3d232-4a3d-406e-9e3e-e7f7a1fcbf86"}}}

```
</details>

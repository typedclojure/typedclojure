(ns typed.clj.refactor
  "Alpha"
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [typed.clj.analyzer :as ana]
            [typed.cljc.analyzer :as ana-cljc]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as ana-env]
            [typed.clj.reader :as rdr]
            [typed.clj.reader.reader-types :refer [indexing-push-back-reader]])
  (:import [java.io StringReader BufferedReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(defn update-rdr-children [ast f]
  (if-some [forms (:forms ast)]
    (assoc ast :forms (mapv f forms))
    ast))

(declare ^:private refactor-form*)

(defmulti ^:private -refactor-form* (fn [expr rdr-ast opt] (::ana-cljc/op expr)))

(defmethod -refactor-form* ::ana-cljc/local
  [{:keys [tag form] sym :name :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:local (cond->
                       {:local (:local expr)
                        :name sym
                        :form form}
                       (and (not (:tag form)) tag)
                       (assoc :inferred-tag tag))])))
  expr)

(defmethod -refactor-form* ::ana-cljc/var
  [{:keys [form var] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:var {:sym (symbol var)}])))
  expr)

(defmethod -refactor-form* ::ana-cljc/binding
  [{:keys [form local env] sym :name :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:binding {:local local
                        :name sym
                        :sym sym}])))
  expr)

(defmethod -refactor-form* :default
  [expr rdr-ast opt]
  (-> expr
      (ast/update-children #(refactor-form* % rdr-ast opt))))

(defmulti -refactor-special (fn [op-sym expr rdr-ast opt] op-sym))

(defn ^:private run!
  "Runs the supplied procedure (via reduce), for purposes of side
  effects, on successive items in the collection. Returns nil"
  {:added "1.7"}
  [proc coll]
  (reduce #(do (proc %2) nil) nil coll)
  nil)

(defn record-destructuring [b file-map-atom]
  (cond
    (vector? b) (do
                  (when (seq (meta b))
                    (swap! file-map-atom assoc
                           (select-keys (meta b)
                                        [:line :column
                                         :end-line :end-column
                                         :file])
                           [:vector-destructure {:binding b}]))
                  (run! #(record-destructuring % file-map-atom) b))
    (map? b) (do
               (when (seq (meta b))
                 (swap! file-map-atom assoc
                        (select-keys (meta b)
                                     [:line :column
                                      :end-line :end-column
                                      :file])
                        [:map-destructure {:binding b}]))
               (->> b
                    (run! (fn [[bi v :as orig]]
                            (cond
                              (#{:as} bi) [orig]
                              (and (#{:keys} bi)
                                   (vector? v))
                              (run! (fn [bk]
                                      (when (seq (meta bk))
                                        (swap! file-map-atom assoc
                                               (select-keys (meta bk)
                                                            [:line :column
                                                             :end-line :end-column
                                                             :file])
                                               [:map-destructure-keys-enty {:binding bk}])))
                                    v)
                              ;;TODO namespaced keywords, :syms :strs
                              (keyword? bi) nil
                              :else nil)))))))

(comment
  (let [a (atom {})]
    (record-destructuring
      '{:keys [a b]}
      a)
    @a)
  )

(defmethod -refactor-special 'clojure.core/let
  [_ {:keys [form] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [bindings (second form)]
    (when (and (vector? bindings)
               (even? (count bindings)))
      (let [_ (doseq [[b v] (partition 2 bindings)]
                (record-destructuring b file-map-atom))]
        (-> expr
            ana-cljc/analyze-outer
            (refactor-form* rdr-ast opt))))))

(defmethod -refactor-special :default [_ _ _ _] nil)

(defmulti ^:private -post-refactor-form* (fn [expr rdr-ast opt] (::ana-cljc/op expr)))

(defmethod -post-refactor-form* ::ana/instance-call
  [expr _ _]
  #_
  (prn `-post-refactor-form* ::ana/instance-call
       (:tag expr)
       (:class expr)
       (:method expr)
       (count (:args expr))
       (:children expr)
       (-> expr :instance :tag))
  expr)

(defmethod -post-refactor-form* :default [expr  _ _]
  expr)

(defn- refactor-form* [expr rdr-ast {:keys [file-map-atom] :as opt}]
  (let [refactor-form* (fn [expr]
                         (case (::ana-cljc/op expr)
                           ::ana-cljc/unanalyzed (let [{:keys [form env]} expr
                                                       op-sym (ana/resolve-op-sym form env)]
                                                   (when op-sym
                                                     (let [mta (meta (first form))]
                                                       (when ((every-pred :line :column) mta)
                                                         (swap! file-map-atom assoc
                                                                (select-keys mta [:line :column
                                                                                  :end-line :end-column
                                                                                  :file])
                                                                [:var {:sym op-sym}]))))
                                                   (or (some-> op-sym (-refactor-special expr rdr-ast opt))
                                                       (recur (ana-cljc/analyze-outer expr))))
                           (-> expr
                               ana-cljc/run-pre-passes
                               (-refactor-form* rdr-ast opt)
                               ana-cljc/run-post-passes
                               ana-cljc/eval-top-level
                               (-post-refactor-form* rdr-ast opt))))
        expr (refactor-form* expr)]
    expr))

(defn- file-map [form rdr-ast opt]
  (let [{:keys [file-map-atom] :as opt}
        (cond-> opt
          (not (:file-map-atom opt)) (assoc :file-map-atom (atom {})))
        env (ana/empty-env)]
    (with-bindings (ana/default-thread-bindings env)
      (ana-env/ensure (ana/global-env)
                      (-> form
                          (ana-cljc/unanalyzed-top-level env)
                          (refactor-form* rdr-ast opt))))
    @file-map-atom))

;; pre pass
(defn fq-rdr-ast [rdr-ast opt]
  (let [file-map (if (:top-level rdr-ast)
                   (file-map (:val rdr-ast) rdr-ast opt)
                   (::file-map rdr-ast))
        _ (prn file-map)
        _ (assert (map? file-map) [((juxt :op :string) rdr-ast)
                                   file-map])
        assoc-file-map #(assoc % ::file-map file-map)
        rdr-ast (case (:op rdr-ast)
                  ::rdr/symbol
                  (let [{:keys [val]} rdr-ast]
                    (if-let [mapped (file-map (select-keys (meta val) [:line :column
                                                                       :end-line :end-column
                                                                       :file]))]
                      (case (first mapped)
                        :var (-> rdr-ast
                                 (assoc :string (str (:sym (second mapped))))
                                 assoc-file-map)
                        (:local :binding)
                        (let [info (second mapped)
                              wrap-discard (fn [sym-ast]
                                             {:op ::rdr/forms
                                              :forms [{:op ::rdr/discard
                                                       ::file-map file-map
                                                       :forms [{:op ::rdr/keyword
                                                                ::file-map file-map
                                                                :string (str (symbol (:local info)))
                                                                :val (keyword (:local info))}]}
                                                      {:op ::rdr/whitespace
                                                       ::file-map file-map
                                                       :string " "}
                                                      sym-ast]})
                              sym-ast (cond-> (assoc rdr-ast
                                                     :string (-> info :name str)
                                                     ::file-map file-map)
                                        (:add-local-origin opt)
                                        wrap-discard)]
                          (if-some [tag (:inferred-tag info)]
                            {:op ::rdr/meta
                             ::file-map file-map
                             :val (vary-meta (:val sym-ast) assoc :tag tag)
                             :forms [{:op ::rdr/symbol
                                      ::file-map file-map
                                      :string (str tag)
                                      :val tag}
                                     {:op ::rdr/whitespace
                                      ::file-map file-map
                                      :string " "}
                                     sym-ast]}
                            sym-ast)))
                      (-> rdr-ast
                          assoc-file-map
                          (update-rdr-children #(assoc % ::file-map file-map)))))
                  (-> rdr-ast
                      assoc-file-map
                      (update-rdr-children #(assoc % ::file-map file-map))))]
    rdr-ast))

;; pre pass
(defn indent-by [{:keys [top-level] :as rdr-ast} indent]
  (case (:op rdr-ast)
    ::rdr/whitespace (update rdr-ast :string
                             str/replace "\n" (str "\n" (apply str (repeat indent \space))))
    (cond-> rdr-ast
      top-level
      #(do {:op ::rdr/forms
            :forms [{:op ::rdr/whitespace
                     :string (apply str (repeat indent \space))}
                    %]}))))

;; pre-pass
;; - must be followed by delete-orphan-whitespace
(defn delete-discard [rdr-ast]
  (case (:op rdr-ast)
    ::rdr/discard {:op ::rdr/forms
                   ::delete-preceding-whitespace true
                   :forms []}
    rdr-ast))

(defn compile-passes [pre post]
  (fn r [rdr-ast]
    (-> rdr-ast
        pre
        (update-rdr-children r)
        post)))

;; post pass
;; - depends on delete-discard
(defn delete-orphan-whitespace [{:keys [forms] :as rdr-ast}]
  (let [forms (reduce (fn [forms i]
                        (if (zero? i)
                          ;; propagate up to parent
                          forms
                          (cond-> forms
                            (::delete-preceding-whitespace (nth forms i))
                            (-> (update i dissoc ::delete-preceding-whitespace)
                                (update (dec i)
                                        (fn [maybe-ws]
                                          (case (:op maybe-ws)
                                            ::rdr/whitespace
                                            (update maybe-ws :string
                                                    ;; TODO review docs to see if this does the
                                                    ;; right thing
                                                    (fn [s]
                                                      (prn 's s (str/trimr s))
                                                      (str/trimr s)))
                                            ;; no preceding whitespace
                                            maybe-ws)))))))
                      forms
                      (range (count forms)))]
    (cond-> (assoc rdr-ast :forms forms)
      (and (pos? (count forms))
           (-> forms (nth 0) ::delete-preceding-whitespace))
      (-> (assoc ::delete-preceding-whitespace true)
          (update-in [:forms 0] dissoc ::delete-preceding-whitespace)))))

(defn reformat-rdr-ast-pre [rdr-ast opt]
  (case (:op rdr-ast)
    ::rdr/whitespace {:op ::rdr/forms
                      :forms []}
    rdr-ast))

(defn reformat-rdr-ast-post [rdr-ast opt]
  rdr-ast)

(defn refactor-form-string
  ([s] (refactor-form-string s {}))
  ([s opt]
   {:pre [(string? s)]}
   (with-open [rdr (-> s
                       StringReader.
                       LineNumberingPushbackReader.
                       (indexing-push-back-reader 1 "example.clj"))]
     (let [read-next #(rdr/read+ast {:read-cond :allow
                                     :features #{:clj}}
                                    ;; eof-error
                                    false
                                    rdr)
           opt (cond-> opt
                 (not (:file-map-atom opt)) (assoc :file-map-atom (atom {})))
           ;; left-to-right order
           pre-passes (filterv identity
                               [#(fq-rdr-ast % opt)
                                (when (:delete-discard opt)
                                  delete-discard)
                                (when (:indent-by opt)
                                  #(indent-by % 2))
                                (when (:reformat opt)
                                  #(reformat-rdr-ast-pre % opt))])
           ;; left-to-right order
           post-passes (filterv identity
                                [(when (:delete-discard opt)
                                   delete-orphan-whitespace)
                                 (when (:reformat opt)
                                   #(reformat-rdr-ast-post % opt))])
           passes (compile-passes (apply comp (rseq pre-passes))
                                  (apply comp (rseq post-passes)))
           sb (StringBuilder.)]
       (loop [{:keys [eof] form :val :as rdr-ast} (read-next)]
         (let [rdr-ast (passes rdr-ast)
               _ (rdr/ast->StringBuilder rdr-ast sb)]
           (if eof
             (str sb)
             (recur (read-next)))))))))

(comment
  (refactor-form-string "(map identity [1 2 3])" {})
  ;=> "(sequence (map identity) [1 2 3])"
  (refactor-form-string "(merge {:a 1 :b 2} {:c 3 :d 4})" {})
  ;=> "(into {:a 1 :b 2} {:c 3 :d 4})"
  (refactor-form-string "(merge {+ 1 :b 2} {:c 3 :d 4})" {})
  (refactor-form-string "(fn [])" {})
  (refactor-form-string "(fn [a] (a))" {})
  (refactor-form-string "(fn [a] (+ a 3))" {})

  (refactor-form-string "+" {})
  ;=> "clojure.core/+"
  (refactor-form-string "(defn foo [a] (+ a 3))" {})
  ;=> "(clojure.core/defn foo [a__#0] (clojure.core/+ a__#0 3))"
  (refactor-form-string "(defmacro foo [a] `(+ ~a 3))" {})
  ;=> "(clojure.core/defmacro foo [a__#0] `(+ ~a__#0 3))"
  (refactor-form-string "(defmacro foo [a] `(+ ~@a 3))" {})
  (refactor-form-string "(str (fn []))" {})
  (refactor-form-string "`fn" {})
  (let [a (atom {})]
    (refactor-form-string "(fn [{:keys [a]}] a)"
                          {:file-map-atom a})
    @a)
  (refactor-form-string "#(.string 1)" {})
  (refactor-form-string "#(.string %)" {})
  (refactor-form-string "#(.string (identity []))" {})
  (refactor-form-string "#(.string #?(:clj 1) (identity []))" {})
  (refactor-form-string "(let [^Long a {}] a)" {})
  (refactor-form-string "(let [^Long a {} b a] b)" {})
  (refactor-form-string "(let [^Long a {} b a c b] b)" {})
  ;; idea: trim redundant tag
  (refactor-form-string "(let [^Long a {} ^Long b a] b)"
                        {})
  (refactor-form-string "(let [^Long a {} ^Long b a] b)"
                        {:add-local-origin true})
  (refactor-form-string "(do #_1)" {})
  (refactor-form-string "(do #_1)"
                        {:delete-discard true})
  (refactor-form-string " #_1 1"
                        {:delete-discard true})
  (println
    (refactor-form-string "(do \n\n#_1)"
                          {:delete-discard true}))
  (println
    (refactor-form-string "(map identity\n)"
                          {:indent-by true}))
  (println
    (refactor-form-string "(map #(\n+ 1 2))"
                          {:reformat true}))
  (println
    (refactor-form-string "1\n2\n3"))
  )

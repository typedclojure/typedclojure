(in-ns 'clojure.core.typed)

(defmacro ^:deprecated doseq
  "DEPRECATED: Use clojure.core/doseq.
  
  Like clojure.core/doseq with optional annotations.

  :let option uses clojure.core.typed/let
  
  eg.
  (doseq [a :- (U nil AnyInteger) [1 nil 2 3]
          :when a]
     (inc a))"
  [seq-exprs & body]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let
       [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (core/fn [seq-exprs]
          (core/loop [flat-result []
                      seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result

              ;for options (:let, :while etc)
              (keyword? (first seq-exprs)) (core/let
                                                [_ (assert (#{2} (count (take 2 seq-exprs)))
                                                           (str "for option missing " (first seq-exprs)))
                                                 [k v & rst] seq-exprs]
                                             (recur (conj flat-result k v)
                                                    rst))
              :else (if (#{:-} (second seq-exprs))
                      (core/let
                           [_ (assert (#{4} (count (take 4 seq-exprs)))
                                      (str "for parameter missing after ':-'"))
                            [b colon t init & rst] seq-exprs]
                        (recur (conj flat-result [b colon t] init)
                               rst))
                      (core/let
                           [_ (assert (#{2} (count (take 2 seq-exprs)))
                                      (str "for binding needs initial values"))
                            [b init & rst] seq-exprs]
                        (recur (conj flat-result [b :- `Any] init)
                               rst))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)
        step (core/fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (core/let
                      [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (core/let
                          [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         ;typed let
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     ;; k is [k :- k-ann]
                     (core/let
                          [_ (assert (and (vector? k)
                                          (#{3} (count k))
                                          (#{:-} (second k))) 
                                     "Binder must be of the form [lhs :- type]")
                           k-ann (nth k 2)
                           k (nth k 0)
                           ; k is the lhs binding
                           seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk 
                             `(recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(loop [~seq- :- (U nil (Seq ~k-ann)) (seq ~v), 
                                ~chunk- :- (U nil (clojure.lang.IChunk ~k-ann)) nil
                                ~count- :- Int 0,
                                ~i- :- Int 0]
                           (if (and (< ~i- ~count-)
                                    ;; FIXME review this
                                    ;; core.typed thinks chunk- could be nil here
                                    ~chunk-)
                             (core/let
                                  [;~k (.nth ~chunk- ~i-)
                                   ~k (nth ~chunk- ~i-)]
                               ~subform-chunk
                               ~@(when needrec [recform-chunk]))
                             (when-let [~seq- (seq ~seq-)]
                               (if (chunked-seq? ~seq-)
                                 (core/let [c# (chunk-first ~seq-)]
                                   (recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))
                                 (core/let [~k (first ~seq-)]
                                   ~subform
                                   ~@(when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

(defmacro ^:deprecated for
  "DEPRECATED: Use clojure.core/for.
  
  Like clojure.core/for with optional type annotations.

  All types default to Any.

  The :let option uses clojure.core.typed/let.
  
  eg. (for [a :- (U nil Int) [1 nil 2 3]
            :when a]
        :- Number
        (inc a))
  
  Metadata using the :clojure.core.typed/ann keyword
  can also be used for annotation.

  eg. (for ^{::ann Number}
        [^{::ann (U nil Int)} a [1 nil 2 3]
         :when a]
        (inc a))
  "
  [seq-exprs & maybe-ann-body-expr]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (core/let
       [orig-seq-exprs seq-exprs
        has-explicit-return-type? (#{:-} (first maybe-ann-body-expr))
        [ret-ann body-expr] (if has-explicit-return-type?
                              (core/let 
                                   [_ (assert (#{3} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [colon t body] maybe-ann-body-expr]
                                [t body])
                              (core/let 
                                   [_ (assert (#{1} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [body] maybe-ann-body-expr]
                                [`Any body]))
        ret-ann (if-let [[_ meta-ann] (find (meta seq-exprs) ::ann)]
                  (do (assert (not has-explicit-return-type?)
                              "Cannot mix explicit and metadata return type in for.")
                      meta-ann)
                  ret-ann)
        ;_ (prn "ret-ann" ret-ann)
        normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (core/fn [seq-exprs]
          (core/loop [flat-result []
                      seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result

              ;for options (:let, :while etc)
              (keyword? (first seq-exprs)) (core/let 
                                                [_ (assert (#{2} (count (take 2 seq-exprs)))
                                                           (str "for option missing " (first seq-exprs)))
                                                 [k v & rst] seq-exprs]
                                             (recur (conj flat-result k v)
                                                    rst))
              :else (core/let 
                         [[meta-ann has-meta-ann?]
                          (when-let [[_ meta-ann] (find (meta (first seq-exprs)) ::ann)]
                            [meta-ann true])]
                      (if (#{:-} (second seq-exprs))
                        (core/let
                             [_ (assert (#{4} (count (take 4 seq-exprs)))
                                        (str "for parameter missing after ':-'"))
                              [b colon t init & rst] seq-exprs]
                          (assert (not meta-ann)
                                  "Cannot mix metadata annotation and explicit annotation in for.")
                          (recur (conj flat-result [b colon t] init)
                                 rst))
                        (core/let 
                             [_ (assert (#{2} (count (take 2 seq-exprs)))
                                        (str "for binding needs initial values"))
                              [b init & rst] seq-exprs
                              ann (if has-meta-ann? meta-ann `Any)]
                          (recur (conj flat-result [b :- ann] init)
                                 rst)))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)

        to-groups (core/fn [seq-exprs]
                    (reduce (core/fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (core/fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (core/fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (core/let 
                         [_ (assert (and (vector? bind)
                                         (#{3} (count bind))
                                         (#{:-} (second bind))) 
                                    "Binder must be of the form [lhs :- type]")
                          bind-ann (nth bind 2)
                          bind (nth bind 0)
                          giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (core/fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     ;typed let
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(core/let
                                            [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons (ann-form ~body-expr ~ret-ann) ;; ann-form for better error messages
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter 
                           [~gxs :- (Option (Seqable ~bind-ann))]
                           :- (Seq ~ret-ann)
                           (lazy-seq
                             (map (fn [t# :- ~ret-ann] :- ~ret-ann
                                    (core/let
                                         [^{::auto-ann ~(meta orig-seq-exprs)
                                            ::track-kind ::for-return}
                                          t# t#]
                                      ;(prn "tracked t#" t#)
                                      t#))
                                  (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                    (when-let [xs# (seq ~gxs)]
                                      (core/let
                                           [^{::auto-ann ~(meta bind)
                                              ::track-kind ::for-param}
                                            x# (first xs#)
                                            ;_# (prn "for param x#" x#)
                                            ~bind x#]
                                        ~(do-mod mod-pairs)))))))
                        #_"inner-most loop"
                        (core/let
                             [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (core/fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          ; typed let
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb 
                                                               ; put an ann-form here so at least one error message
                                                               ; points to code the user can recognise.
                                                               (ann-form ~body-expr
                                                                         ~ret-ann))
                                                 (recur (unchecked-inc ~gi)))))]
                          `(fn ~giter [~gxs :- (Option (Seqable ~bind-ann))]
                             :- (Seq ~ret-ann)
                             (lazy-seq
                               (map (fn [t# :- ~ret-ann] :- ~ret-ann
                                      (core/let
                                           [^{::auto-ann ~(meta orig-seq-exprs)
                                              ::track-kind ::for-return}
                                            t# t#]
                                        t#))
                                    (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                      (when-let [~gxs (seq ~gxs)]
                                        (if (chunked-seq? ~gxs)
                                          (core/let 
                                               [c# (chunk-first ~gxs)
                                                size# (int (count c#))
                                                ~gb (ann-form (chunk-buffer size#)
                                                              (~'clojure.lang.ChunkBuffer ~ret-ann))]
                                            (if (loop [~gi :- Int, (int 0)]
                                                  (if (< ~gi size#)
                                                    (core/let 
                                                         [;~bind (.nth c# ~gi)]
                                                          ^{::auto-ann ~(meta bind)
                                                            ::track-kind ::for-param}
                                                          x# (nth c# ~gi)
                                                          ~bind x#]
                                                      ~(do-cmod mod-pairs))
                                                    true))
                                              (chunk-cons
                                                (chunk ~gb)
                                                (~giter (chunk-rest ~gxs)))
                                              (chunk-cons (chunk ~gb) nil)))
                                          (core/let
                                               [^{::auto-ann ~(meta bind)
                                                  ::track-kind ::for-param}
                                                x# (first ~gxs)
                                                ;_# (prn "for param x#" x#)
                                                ~bind x#]
                                            ~(do-mod mod-pairs))))))))))))]
    `(core/let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

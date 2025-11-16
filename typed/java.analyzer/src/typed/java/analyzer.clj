;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; based on https://github.com/clj-kondo/clj-kondo/blob/dd45054e3eb8d9aa76a7da0e42826144dbd957c6/src/clj_kondo/impl/analysis/java.clj
(ns typed.java.analyzer
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str])
  (:import
   [com.github.javaparser JavaParser Range]
   [com.github.javaparser.ast
    CompilationUnit
    Modifier
    Modifier$Keyword
    Node]
   [com.github.javaparser.ast.body
    ClassOrInterfaceDeclaration
    EnumConstantDeclaration
    ConstructorDeclaration
    FieldDeclaration
    MethodDeclaration
    Parameter
    VariableDeclarator]
   [com.github.javaparser.ast.comments Comment]
   [com.github.javaparser.ast.expr SimpleName]
   [com.github.javaparser.metamodel PropertyMetaModel]
   [java.io File InputStream]
   [java.util.jar JarFile JarFile$JarFileEntry]
   [org.objectweb.asm
    ClassReader
    ClassVisitor
    Opcodes
    Type]))

(set! *warn-on-reflection* true)

;; https://github.com/clj-kondo/clj-kondo/blob/dd45054e3eb8d9aa76a7da0e42826144dbd957c6/src/clj_kondo/impl/utils.clj#L456C1-L461C57
(defn- ->uri [jar entry file]
  (cond file (when (fs/exists? file)
               (str (.toURI (fs/file file))))
        (and jar entry)
        (str "jar:" (.toURI (io/file jar)) "!/" entry)))

(defn ^:private input-stream->bytes ^bytes [^InputStream input-stream]
  (with-open [xin input-stream
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

#_(defn ^:private opcode->flags []
    {Opcodes/ACC_PUBLIC #{:public}
     Opcodes/ALOAD #{:public :field :static}
     Opcodes/SIPUSH #{:public :field :final}
     Opcodes/LCONST_0 #{:public :method :static}})

(def ^:private opcode->flag
  [Opcodes/ACC_ABSTRACT :abstract
   Opcodes/ACC_ANNOTATION :annotation
   Opcodes/ACC_BRIDGE :bridge
   Opcodes/ACC_DEPRECATED :deprecated
   Opcodes/ACC_ENUM :enum
   Opcodes/ACC_FINAL :final
   Opcodes/ACC_INTERFACE :interface
   Opcodes/ACC_MANDATED :mandated
   Opcodes/ACC_MODULE :module
   Opcodes/ACC_NATIVE :native
   Opcodes/ACC_OPEN :open
   Opcodes/ACC_PRIVATE :private
   Opcodes/ACC_PROTECTED :protected
   Opcodes/ACC_PUBLIC :public
   Opcodes/ACC_STATIC :static
   Opcodes/ACC_STATIC_PHASE :static_phase
   Opcodes/ACC_STRICT :strict
   Opcodes/ACC_SUPER :super
   Opcodes/ACC_SYNCHRONIZED :synchronized
   Opcodes/ACC_SYNTHETIC :synthetic
   Opcodes/ACC_TRANSIENT :transient
   Opcodes/ACC_TRANSITIVE :transitive
   Opcodes/ACC_VARARGS :varargs
   Opcodes/ACC_VOLATILE :volatile])

(defn- opcode->flags
  [x]
  (reduce (fn [s i]
            (let [i (* 2 i)
                  opcode (nth opcode->flag i)
                  kw (nth opcode->flag (inc i))]
              (cond-> s
                (= (bit-and x opcode) opcode)
                (conj kw))))
          #{} (range (/ (count opcode->flag) 2))))

(defn- modifier-keyword->flag []
  (reduce #(assoc %1 %2 (keyword (str/lower-case (.asString ^Modifier$Keyword %2))))
          {} (Modifier$Keyword/values)))

(defn- class-is->class-info
  "Parse class-bytes using ASM."
  [^InputStream class-is]
  (let [class-reader (ClassReader. (input-stream->bytes class-is))
        class-name (str/replace (.getClassName class-reader) "/" ".")
        result* (atom {class-name {:members [] :flags nil}})]
    (.accept
     class-reader
     (proxy [ClassVisitor] [Opcodes/ASM9]
       (visit [version access name signature superName interfaces]
         (swap! result* assoc-in [class-name :flags] (opcode->flags access))
         nil)
       (visitField [access ^String name ^String desc signature value]
         (let [flags (opcode->flags access)]
           (when (:public flags)
             (swap! result* update-in [class-name :members] conj
                    {:name name
                     :flags (conj flags :field)
                     :type (.getClassName (Type/getType desc))})))
         nil)
       (visitMethod [access ^String name ^String desc signature exceptions]
         (let [flags (opcode->flags access)]
           (when (:public flags)
             (swap! result* update-in [class-name :members] conj
                    {:name name
                     :parameter-types (mapv #(.getClassName ^Type %) (Type/getArgumentTypes desc))
                     :flags (conj flags :method)
                     :return-type (.getClassName (Type/getReturnType desc))})))
         nil))
     ClassReader/SKIP_DEBUG)
    @result*))

(defn- node->flag-member-type [node]
  (condp = (type node)
    FieldDeclaration :field
    MethodDeclaration :method
    ConstructorDeclaration :method
    EnumConstantDeclaration :field))

(defn- node->location [^Node node]
  (when-let [^Range range (.orElse (.getRange node) nil)]
    {:row (.-line (.-begin range))
     :col (.-column (.-begin range))
     :end-row (.-line (.-end range))
     :end-col (.-column (.-end range))}))

(defn- node->member
  [^Node node modifier-keyword->flag]
  (let [member (for [^PropertyMetaModel model (.getAllPropertyMetaModels (.getMetaModel node))
                     :let [value-or-list (.getValue model node)]]
                 (condp identical? (.getType model)
                   Modifier {:flags (set (map #(modifier-keyword->flag (.getKeyword ^Modifier %)) value-or-list))}
                   Comment (some->> ^Comment value-or-list .asString (hash-map :doc))
                   VariableDeclarator {:name (.asString (.getName ^VariableDeclarator (first value-or-list)))
                                       :type (.asString (.getType ^VariableDeclarator (first value-or-list)))}
                   Parameter {:parameters (mapv #(.toString ^Parameter %) value-or-list)}
                   SimpleName {:name (.asString ^SimpleName value-or-list)}
                   com.github.javaparser.ast.type.Type {:return-type (.asString ^com.github.javaparser.ast.type.Type value-or-list)}
                   nil))
        member (reduce merge {} member)]
    (when-not (contains? (:flags member) :private)
      (-> member
          (merge (some-> node node->location))
          (update :flags set/union #{(node->flag-member-type node)})))))

(defn- source-is->java-member-definitions [^InputStream source-input-stream filename]
  (let [modifier-keyword->flag (modifier-keyword->flag)]
    (try
      (when-let [compilation ^CompilationUnit (.orElse (.getResult (.parse (JavaParser.) source-input-stream)) nil)]
        (reduce
         (fn [classes ^com.github.javaparser.ast.body.TypeDeclaration class-or-interface]
           (if-let [class-name (.orElse (.getFullyQualifiedName class-or-interface) nil)]
             (let [is-interface? (and (instance? ClassOrInterfaceDeclaration class-or-interface)
                                      (.isInterface ^ClassOrInterfaceDeclaration class-or-interface))
                   members (->> (concat
                                 (.findAll class-or-interface FieldDeclaration)
                                 (.findAll class-or-interface ConstructorDeclaration)
                                 (.findAll class-or-interface MethodDeclaration)
                                 (.findAll class-or-interface EnumConstantDeclaration))
                                (keep #(node->member % modifier-keyword->flag))
                                (mapv (fn [member]
                                        (if (and is-interface? (contains? (:flags member) :method))
                                          (update member :flags conj :public)
                                          member))))
                   flags (set (map #(modifier-keyword->flag
                                     (.getKeyword ^Modifier %))
                                   (.getModifiers class-or-interface)))
                   flags (if is-interface?
                           (conj flags :interface)
                           flags)]
               (assoc classes class-name {:members members
                                          :flags flags}))
             classes))
         {}
         (.findAll compilation com.github.javaparser.ast.body.TypeDeclaration)))
      (catch Throwable e
        (binding [*out* *err*]
          (println "Error parsing java file" filename "with error" e))))))

#_
(defn reg-class-def! [ctx {:keys [^JarFile jar ^JarFile$JarFileEntry entry filename ^File file]}]
  (let [uri (if jar
              (->uri (str (.getCanonicalPath file)) (.getName entry) nil)
              (->uri nil nil filename))
        class-is (or (and jar entry (.getInputStream jar entry))
                     (io/input-stream filename))
        class-by-info (with-open [is ^InputStream class-is]
                        (if (str/ends-with? filename ".class")
                          (class-is->class-info is)
                          (source-is->java-member-definitions is filename)))]
    (doseq [[class-name class-info] class-by-info]
      (let [flags (:flags class-info)
            class-def {:class class-name
                       :uri uri
                       :filename filename
                       :flags flags}]
        (swap! (:analysis ctx)
               update :java-class-definitions conj
               class-def))
      (when (:analyze-java-member-defs? ctx)
        (doseq [member (:members class-info)]
          (swap! (:analysis ctx)
                 update :java-member-definitions conj
                 (merge {:class class-name
                         :uri uri}
                        member)))))))

#_:clj-kondo/ignore
(comment
  (def x (source-is->java-member-definitions (io/input-stream "/Users/borkdude/.cache/clojure-lsp/jdk/java.base/java/lang/System.java") "/Users/borkdude/.cache/clojure-lsp/jdk/java.base/java/lang/System.java"))
  x
  (keys x)
  (def sys (get x "java.lang.System"))
  (defn ana->cached [name ana]
    (let [members (:members ana)
          grouped (group-by :name members)]
      (utils/update-vals grouped (fn [dudes]
                                   (-> dudes first
                                       (select-keys [:flags]))))))
  (ana->cached "java.lang.System" sys)
  (def clazz (io/resource "java/lang/Object.class"
                          #_"java/time/temporal/ChronoField.class"))
  (class-is->class-info (io/input-stream clazz)))

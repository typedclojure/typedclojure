(ns clojure.core.typed.test.noninitializing-class-resolution
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.repl :as repl]
            [clojure.test :refer [deftest is]]
            [typed.clj.analyzer.utils :as analyzer])
  (:import [clojure.asm ClassWriter Opcodes]
           [clojure.lang Compiler DynamicClassLoader Namespace RT]))

(set! *warn-on-reflection* true)

(def INITIALIZED-MSG "initialized")
(def FAILED-INITIALIZER-MSG "type inspection initialized owner")

(defn- fixture-class [^DynamicClassLoader loader ^String name property fail?]
  (let [writer (ClassWriter. 0)
        internal (.replace name "." "/")]
    (.visit writer Opcodes/V1_8 Opcodes/ACC_PUBLIC internal nil "java/lang/Object" nil)
    (.visitEnd (.visitField writer Opcodes/ACC_PUBLIC "coefficient" "D" nil nil))
    (let [method (.visitMethod writer Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)]
      (.visitCode method)
      (.visitLdcInsn method property)
      (.visitLdcInsn method INITIALIZED-MSG)
      (.visitMethodInsn method Opcodes/INVOKESTATIC "java/lang/System" "setProperty"
                        "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;" false)
      (.visitInsn method Opcodes/POP)
      (if fail?
        (do (.visitTypeInsn method Opcodes/NEW "java/lang/IllegalStateException")
            (.visitInsn method Opcodes/DUP)
            (.visitLdcInsn method FAILED-INITIALIZER-MSG)
            (.visitMethodInsn method Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>"
                              "(Ljava/lang/String;)V" false)
            (.visitInsn method Opcodes/ATHROW))
        (.visitInsn method Opcodes/RETURN))
      (.visitMaxs method 3 0)
      (.visitEnd method))
    (.visitEnd writer)
    (.defineClass loader name (.toByteArray writer) nil)))

(defn- with-owner [check]
  (doseq [fail? [false true]]
    (let [loader (DynamicClassLoader. (RT/baseLoader))
          name (str "typed.fixture.TypeInspection" (RT/nextID))
          property (str "typed.test.type-inspection.property" (RT/nextID))
          owner (fixture-class loader name property fail?)
          ^Namespace source (create-ns (gensym "typed.type-inspection.ns"))]
      (try
        (.importClass source 'OwnerAlias owner)
        (with-bindings {Compiler/LOADER loader}
          (binding [*ns* source]
            (check owner name property)
            (is (nil? (System/getProperty property)))
            ;; An actual active use must still initialize (or fail) normally.
            (is (= (when fail? FAILED-INITIALIZER-MSG)
                   (try (Class/forName name true loader)
                        nil
                        (catch Throwable error (ex-message (repl/root-cause error))))))
            (is (= INITIALIZED-MSG (System/getProperty property)))))
        (finally
          (System/clearProperty property)
          (remove-ns (ns-name source)))))))

(deftest type-class-lookups-do-not-initialize-user-classes
  (with-owner
    (fn [owner name property]
      (is (identical? owner (analyzer/maybe-class-from-string name)))
      (is (identical? owner (analyzer/maybe-class (symbol name))))
      (is (identical? owner (analyzer/maybe-class-from-string "OwnerAlias")))
      (is (identical? owner (coerce/symbol->Class (symbol name))))
      (is (nil? (System/getProperty property))))))

(defn- instance-field-in-ast? [ast]
  ;; Follow the analyzer's declared AST children, not arbitrary TCResult/type
  ;; records; those intentionally reject lookup of unknown keys such as :op.
  (boolean
    (and (map? ast)
         (or (= :instance-field (:op ast))
             (some (fn [key]
                     (let [child (get ast key)]
                       (if (vector? child)
                         (some instance-field-in-ast? child)
                         (instance-field-in-ast? child))))
                   (:children ast))))))

(deftest checker-field-inspection-is-not-an-active-use
  ;; Initialize checker configuration before creating the observed owner.
  (let [warmup (t/check-form-info 1 {:checked-ast true :check-config {:check-form-eval :never}})]
    (is (nil? (:ex warmup)))
    (is (empty? (:type-errors warmup)))
    (is (some? (:checked-ast warmup))))
  (with-owner
    (fn [_ name property]
      (let [tag (symbol name)
            value (with-meta 'value {:tag tag})
            form (list `t/fn [value ':- tag]
                       (list '. value '-coefficient))
            result (t/check-form-info form {:checked-ast true
                                            :check-config {:check-form-eval :never}})]
        (is (empty? (:type-errors result)))
        (is (nil? (:ex result)))
        (is (some? (:checked-ast result)))
        (is (instance-field-in-ast? (:checked-ast result)))
        (is (nil? (System/getProperty property)))))))

(deftest primitive-and-missing-class-policies-are-unchanged
  (doseq [[tag expected] [['byte Byte/TYPE] ['short Short/TYPE] ['int Integer/TYPE]
                          ['long Long/TYPE] ['float Float/TYPE] ['double Double/TYPE]
                          ['boolean Boolean/TYPE] ['char Character/TYPE]]]
    (is (identical? expected (coerce/symbol->Class tag))))
  (is (nil? (analyzer/maybe-class-from-string "typed.fixture.ClassThatDoesNotExist")))
  (is (= ClassNotFoundException
         (try (coerce/symbol->Class 'typed.fixture.ClassThatDoesNotExist)
              nil
              (catch Throwable error (class error))))))

(deftest type-reference-resolution-preserves-var-alias-and-class-policies
  (with-owner
    (fn [^Class owner name property]
      (let [other (create-ns (gensym "typed.type-alias.ns"))]
        (try (let [variable (intern other 'NamedType 'type-value)]
               (alias 'other-type (ns-name other))
               (is (identical? owner (coerce/resolve-type-reference-symbol *ns* (symbol name))))
               (is (identical? owner (coerce/resolve-type-reference-symbol *ns* 'OwnerAlias)))
               (let [descriptor (str "[L" name ";")
                     expected (Class/forName descriptor false (.getClassLoader owner))]
                 (is (identical? expected (coerce/resolve-type-reference-symbol *ns* (symbol descriptor)))))
               (is (identical? variable (coerce/resolve-type-reference-symbol *ns* 'other-type/NamedType)))
               (is (nil? (coerce/resolve-type-reference-symbol *ns* 'typed.fixture.ClassThatDoesNotExist)))
               (is (nil? (System/getProperty property))))
             (finally (remove-ns (ns-name other)))))))
  (let [missing-source (gensym "typed.missing-source.ns")]
    (is (thrown? Exception (coerce/resolve-type-reference-symbol missing-source 'java.lang.String)))))

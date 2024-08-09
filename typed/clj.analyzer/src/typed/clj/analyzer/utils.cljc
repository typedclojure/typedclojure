;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.jvm.utils
(ns typed.clj.analyzer.utils
  #?(:clj (:refer-clojure :exclude [delay]))
  (:require [typed.cljc.analyzer.utils :as u]
            [typed.cljc.analyzer :as ana2]
            [clojure.reflect :as reflect]
            [clojure.string :as s]
            [clojure.core.memoize :refer [lru]]
            #?(:cljr [clojure.clr.io]
               :default [clojure.java.io :as io])
            #?(:clj [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]))
  (:import (clojure.lang RT Symbol Var)
           #?(:clj org.objectweb.asm.Type)))

(set! *warn-on-reflection* true)

(defn resolve-sym
  "Resolves the value mapped by the given sym in the global env
  If sym is shadowed by a local in env, returns nil."
  [sym {:keys [ns locals] :as env}]
  (when (symbol? sym)
    (ns-resolve ns locals sym)))

#?(
:cljr 

(defn ^:private type-reflect
  [typeref & options]
  (apply reflect/type-reflect typeref
         options))

:default

(defn ^:private type-reflect
  [typeref & options]
  (apply reflect/type-reflect typeref
         :reflector #?(:cljr (reflect/->ClrReflector nil)
                       :default (reflect/->JavaReflector (RT/baseLoader)))
         options))
)


;difference: use resolve-sym
(defn macro? [sym env]
  (when-let [v (resolve-sym sym env)]
    (and (not (-> env :locals (get sym)))
         (u/macro? v)
         v)))

;difference: use resolve-sym
(defn inline? [sym args env]
  (when-let [v (resolve-sym sym env)]
    (let [inline-arities-f (:inline-arities (meta v))]
      (and (not (-> env :locals (get sym)))
           (or (not inline-arities-f)
               (inline-arities-f (count args)))
           (:inline (meta v))))))
		   
#?(
:cljr 

(defn specials [c]
  (case c
    "byte"    Byte         ;;; Byte/TYPE
    "boolean" Boolean      ;;; Boolean/TYPE
    "char"    Char         ;;; Character/TYPE
    "int"     Int32        ;;; Integer/TYPE
    "long"    Int64        ;;; Long/TYPE
    "float"   Single       ;;; Float/TYPE
    "double"  Double       ;;; Double/TYPE
    "short"   Int16        ;;; Short/TYPE
    "void"    System.Void  ;;; Void/TYPE
    "object"  Object       ;;; DM: Added
	"decimal" Decimal      ;;; DM: Added
	"sbyte"   SByte        ;;; DM: Added
	"ushort"  UInt16       ;;; DM: Added
	"uint"    UInt32       ;;; DM: Added
	"ulong"   UInt64       ;;; DM: Added
    nil))

:default

(defn specials [c]
  (case c
    "byte" Byte/TYPE
    "boolean" Boolean/TYPE
    "char" Character/TYPE
    "int" Integer/TYPE
    "long" Long/TYPE
    "float" Float/TYPE
    "double" Double/TYPE
    "short" Short/TYPE
    "void" Void/TYPE
    "object" Object
    nil))
)

#?(
:cljr

(defn special-arrays [c]
  (case c
    "bytes"    |System.Byte[]|             ;;; (Class/forName "[B")
    "booleans" |System.Boolean[]|          ;;; (Class/forName "[Z")
    "chars"    |System.Char[]|             ;;; (Class/forName "[C")
    "ints"     |System.Int32[]|            ;;; (Class/forName "[I")
    "longs"    |System.Int64[]|            ;;; (Class/forName "[J")
    "floats"   |System.Single[]|           ;;; (Class/forName "[F")
    "doubles"  |System.Double[]|           ;;; (Class/forName "[D")
    "shorts"   |System.Int16[]|            ;;; (Class/forName "[S")
    "objects"  |System.Object[]|           ;;; (Class/forName "[Ljava.lang.Object;")
	"sbytes"   |System.SByte[]|            ;;;  DM: Added 
	"ushorts"  |System.Int16[]|            ;;;  DM: Added
	"uints"    |System.Int32[]|            ;;;  DM: Added
	"ulongs"   |System.Int64[]|            ;;;  DM: Added
	"decimals" |System.Decimal[]|          ;;;  DM: Added
    nil))

:default

(defn special-arrays [c]
  (case c
    "bytes" (Class/forName "[B")
    "booleans" (Class/forName "[Z")
    "chars" (Class/forName "[C")
    "ints" (Class/forName "[I")
    "longs" (Class/forName "[J")
    "floats" (Class/forName "[F")
    "doubles" (Class/forName "[D")
    "shorts" (Class/forName "[S")
    "objects" (Class/forName "[Ljava.lang.Object;")
    nil))
)


(defmulti ^#?(:cljr Type :default Class) maybe-class
  "Takes a Symbol, String or Class and tries to resolve to a matching Class"
  class)

(defn array-class [element-type]
  (RT/classForName
    #?(:cljr (str (-> element-type
                      maybe-class 
                      .FullName
                      (.Replace \/ \.))
                  "[]")
       :default (str "[" (-> element-type
                             maybe-class
                             Type/getType
                             .getDescriptor
                             (.replace \/ \.))))))


;difference: always use resolve-sym
(defn maybe-class-from-string [^String s]
  (or (when-let [maybe-class (and (neg? (#?(:cljr .IndexOf  :default .indexOf) s "."))
                                  (not= \[ (first s))
                                  (resolve-sym (symbol s) {:ns (ns-name *ns*)}))]
        (when (class? maybe-class) maybe-class))
      (try (RT/classForName s)
           (catch #?(:cljr Exception :default ClassNotFoundException) _))))

(defmethod maybe-class :default [_] nil)
(defmethod maybe-class #?(:cljr Type :default Class) [c] c)
(defmethod maybe-class String [s]
  (maybe-class (symbol s)))

(defmethod maybe-class Symbol [sym]
  (when-not (namespace sym)
    (let [sname (name sym)
          snamec (count sname)]
      (if-let [base-type (and (#?(:cljr .EndsWith :default .endsWith) sname "<>")
                              (maybe-class (subs sname 0 (- snamec 2))))]
        (array-class base-type)
        (if-let [ret (or (specials sname)
                         (special-arrays sname))]
          ret
          (maybe-class-from-string sname))))))

(defn maybe-class-literal [x]
  (cond
   (class? x) x
   (symbol? x) (when-not (namespace x)
                 (maybe-class-from-string (name x)))
   (string? x) (maybe-class-from-string x)))

(def primitive?
  "Returns non-nil if the argument represents a primitive Class other than Void"
  #?(:cljr
     #{Double Char Byte Boolean SByte Decimal
       Int16 Single Int64 Int32 UInt16 UInt64 UInt32}
     :default
     #{Double/TYPE Character/TYPE Byte/TYPE Boolean/TYPE
       Short/TYPE Float/TYPE Long/TYPE Integer/TYPE}))

(def ^:private convertible-primitives
  "If the argument is a primitive Class, returns a set of Classes
   to which the primitive Class can be casted"
  #?(:cljr
     {Int32   #{Int32 Int64 Int16 Byte SByte}  
      Single  #{Single Double}                 
      Double  #{Double Single}                 
      Int64   #{Int64 Int32 Int16 Byte}        
      Char    #{Char}                          
      Int16   #{Int16}                         
      Byte    #{Byte}                          
      Boolean #{Boolean}                       
      UInt32  #{Int32 Int64 Int16 Byte SByte}  
      UInt64  #{Int64 Int32 Int16 Byte}        
      UInt16  #{Int16}                         
      SByte   #{SByte}                         
      Decimal #{Decimal}                       
      System.Void    #{System.Void}}
     :default
     {Integer/TYPE   #{Integer Long/TYPE Long Short/TYPE Byte/TYPE Object Number}
      Float/TYPE     #{Float Double/TYPE Object Number}
      Double/TYPE    #{Double Float/TYPE Object Number}
      Long/TYPE      #{Long Integer/TYPE Short/TYPE Byte/TYPE Object Number}
      Character/TYPE #{Character Object}
      Short/TYPE     #{Short Object Number}
      Byte/TYPE      #{Byte Object Number}
      Boolean/TYPE   #{Boolean Object}
      Void/TYPE      #{Void}}))

#?(
:cljr
(defn ^Type box
  "If the argument is a primitive Class, returns its boxed equivalent,
   otherwise returns the argument"
  [c]
   c)

:default

(defn ^Class box
  "If the argument is a primitive Class, returns its boxed equivalent,
   otherwise returns the argument"
  [c]
  ({Integer/TYPE   Integer
    Float/TYPE     Float
    Double/TYPE    Double
    Long/TYPE      Long
    Character/TYPE Character
    Short/TYPE     Short
    Byte/TYPE      Byte
    Boolean/TYPE   Boolean
    Void/TYPE      Void}
   c c))
)


#?(
:cljr

(defn ^Type unbox                                                          ;;; ^Class
  "If the argument is a Class with a primitive equivalent, returns that,
   otherwise returns the argument"
  [c]
   c)

:default

(defn ^Class unbox
  "If the argument is a Class with a primitive equivalent, returns that,
   otherwise returns the argument"
  [c]
  ({Integer   Integer/TYPE,
    Long      Long/TYPE,
    Float     Float/TYPE,
    Short     Short/TYPE,
    Boolean   Boolean/TYPE,
    Byte      Byte/TYPE,
    Character Character/TYPE,
    Double    Double/TYPE,
    Void      Void/TYPE}
   c c))
)

(defn numeric?
  "Returns true if the given class is numeric"
  [c]
  (when c
    #?(:cljr (clojure.lang.Util/IsNumeric ^Type c)
       :default (.isAssignableFrom Number (box c)))))

(defmacro assignable-from? [t1 t2]
  `(#?(:cljr .IsAssignableFrom :default .isAssignableFrom) ~t1 ~t2))
  
(defn subsumes?
  "Returns true if c2 is subsumed by c1"
  [c1 c2]
  (let [c1 (maybe-class c1)
        c2 (maybe-class c2)]
    (and (not= c1 c2)
         (or (and (not (primitive? c1))
                  (primitive? c2))
             (assignable-from? c2 c1)))))

(defn convertible?
  "Returns true if it's possible to convert from c1 to c2"
  [c1 c2]
  (let [c1 (maybe-class c1)
        c2 (maybe-class c2)]
    (if (nil? c1)
      (not (primitive? c2))
      (or
       (= c1 c2)
       (assignable-from? c2 c1)
       (and (primitive? c2)
            ((convertible-primitives c2) c1))
       (and (primitive? c1)
            (assignable-from? (box c1) c2))))))

(def wider-than
  "If the argument is a numeric primitive Class, returns a set of primitive Classes
   that are narrower than the given one"
  #?(:cljr
     {Int64   #{Int32 UInt32 Int16 UInt16 Byte SByte}            
      Int32   #{Int16 UInt16 Byte SByte}                         
      Single  #{Int32 UInt32 Int16 UInt16 Byte SByte}            
      Double  #{Int32 UInt32 Int16 UInt16 Byte SByte Single}     
      Int16   #{Byte SByte}                                      
      UInt64  #{Int32 UInt32 Int16 UInt16 Byte SByte}            
      UInt32  #{Int16 UInt16 Byte SByte}                         
      UInt16  #{Byte SByte}                                      
      Decimal #{}                                                
      Byte    #{}}
     :default
     {Long/TYPE    #{Integer/TYPE Short/TYPE Byte/TYPE}
      Integer/TYPE #{Short/TYPE Byte/TYPE}
      Float/TYPE   #{Integer/TYPE Short/TYPE Byte/TYPE Long/TYPE}
      Double/TYPE  #{Integer/TYPE Short/TYPE Byte/TYPE Long/TYPE Float/TYPE}
      Short/TYPE   #{Byte/TYPE}
      Byte/TYPE    #{}}))

(defn wider-primitive
  "Given two numeric primitive Classes, returns the wider one"
  [from to]
  (if ((wider-than from) to)
    from
    to))

(defn wider-tag*
  "Given two Classes returns the wider one"
  [from to]
  (if (not= from to)
    (if (primitive? from)
      (if (primitive? to)
        (wider-primitive from to)
        (or (and (numeric? from)
                 (numeric? to)
                 to)
            ((convertible-primitives from) to)))
      (if (primitive? to)
        (or (and (numeric? from)
                 (numeric? to)
                 from)
            ((convertible-primitives to) from))
        (if (convertible? from to)
          to
          (when (convertible? to from)
            from))))
    from))

(defn wider-tag
  "Given a collection of Classes returns the wider one"
  [tags]
  (let [tags* (filter identity tags)
        wider (loop [wider (first tags*) tags* (rest tags*)]
                (if (seq tags*)
                  (if-let [t (wider-tag* wider (first tags*))]
                    (recur t (rest tags*)))
                  wider))]
    (when (or (= tags* tags)
              (not (primitive? wider)))
      wider)))

(defn name-matches?
  [member]
  (let [member-name (str member)
        i (#?(:cljr .LastIndexOf :default .lastIndexOf) member-name ".")
        member-name* (when (pos? i)
                       (str (s/replace (subs member-name 0 i) "-" "_") (subs member-name i)))
        member-name** (s/replace member-name "-" "_")
        member-name*** (delay (munge member-name))]
    (fn [name]
      (let [name (str name)]
        (or (= member-name name)
            (= member-name* name)
            (= member-name** name)
            (= @member-name*** name))))))

(def object-members
  (:members (type-reflect Object)))

(def members*
  (lru (fn members*
         ([class]
          (into object-members
                (remove (fn [{:keys [flags]}]
                          (not-any? #{:public :protected} flags))
                        (-> class
                            maybe-class
                            box
                            #?(:cljr .FullName :default .getName)
                            symbol
                            (type-reflect :ancestors true)
                            :members)))))))

(defn members
  ([class] (members* class))
  ([class member]
   (let [nm? (name-matches? member)]
     (filter #(nm? (:name %))
             (members* class)))))

(defn- members2
  ([class] (members* class))
  ([class member]
   (let [nm? (name-matches? member)]
     (eduction (filter #(nm? (:name %))) (members* class)))))

(defn- static-members [class f]
  (eduction (filter (comp :static :flags)) (members2 class f)))

(defn- instance-members [class f]
  (eduction (remove (comp :static :flags)) (members2 class f)))

(defn static-methods [class method argc]
  (eduction (filter #(and (instance? clojure.reflect.Method %)
                          (= argc (count (:parameter-types %)))))
            (static-members class method)))

(defn instance-methods [class method argc]
  (eduction (filter #(and (instance? clojure.reflect.Method %)
                          (= argc (count (:parameter-types %)))))
            (instance-members class method)))

(defn- field [member]
  (when (instance? clojure.reflect.Field member)
    member))

(defn static-field [class f]
  (some field (static-members class f)))

(defn instance-field [class f]
  (some field (instance-members class f)))

(defn static-method [class method]
  (first (static-methods class method 0)))

(defn instance-method [class method]
  (first (instance-methods class method 0)))

(defn prim-or-obj
  "If the given Class is a primitive, returns that Class, otherwise returns Object"
  [tag]
  (if (and tag (primitive? tag))
    tag
    #?(:cljr System.Object :default java.lang.Object)))

#?(
:cljr

;;; We have to work a lot harder on this one.
;;; The idea is that if (in Java) tags is Long Object Double Object, then you extract LODO and look up "clojure.lang.IFn$LODO" to see if it is a class.
;;; This would be one of the primitive interface types.
;;; Our problem is that we have Int64 instead of Long, so we get "I" instead of "L".  Double and Object are okay.
;;; We'll create a map mapping Int64, Double, Object to the correct character, and default every other type to something bogus.
;;; Then do the class lookup. However, our classes are named clojure.lang.primifs.LODO, e.g.

(defn prim-interface [tags]
  (when (some primitive? tags)
    (let [sig (apply str (mapv #(get {Object "O" Int64 "L" Double "D"} % "x") tags))]
	  (maybe-class (str "clojure.lang.primifs." sig)))))

:default 

(defn prim-interface [tags]
  (when (some primitive? tags)
    (let [sig (apply str (mapv #(.toUpperCase (subs (.getSimpleName ^Class %) 0 1)) tags))]
      (maybe-class (str "clojure.lang.IFn$" sig)))))

)


(defn tag-match? [arg-tags meth]
  (every? identity (map convertible? arg-tags (:parameter-types meth))))

(defn try-best-match
  "Given a vector of arg tags and a collection of methods, tries to return the
   subset of methods that match best the given tags"
  [tags methods]
  (let [o-tags (mapv #(or (maybe-class %) Object) tags)]
    (if-let [methods (or (not-empty
                           (filterv #(= o-tags (mapv maybe-class (:parameter-types %)))
                                    methods))
                         (not-empty
                           (filterv #(tag-match? tags %) methods)))]
      (reduce (fn [[prev & _ :as p] next]
                (let [prev-params (mapv maybe-class (:parameter-types prev))
                      next-params (mapv maybe-class (:parameter-types next))
                      prev-ret    (maybe-class (:return-type prev))
                      next-ret    (maybe-class (:return-type next))
                      prev-decl   (maybe-class (:declaring-class prev))
                      next-decl   (maybe-class (:declaring-class next))]
                  (cond
                   (not prev)
                   [next]
                   (= prev-params next-params)
                   (cond
                    (= prev-ret next-ret)
                    (cond
                     (assignable-from? prev-decl next-decl)
                     [next]
                     (assignable-from? next-decl prev-decl)
                     p
                     :else
                     (conj p next))
                    (assignable-from? prev-ret next-ret)
                    [next]
                    (assignable-from? next-ret prev-ret)
                    p
                    :else
                    (conj p next))
                   (and (some true? (map subsumes? next-params prev-params))
                        (not-any? true? (map subsumes? prev-params next-params)))
                   [next]
                   :else
                   (conj p next)))) [] methods)
      methods)))

(defn ns->relpath [s]
  (-> s str (s/replace \. \/) (s/replace \- \_) (str ".clj")))

#?(
:cljr

;; no equivalent

(defn ns-url [ns]
  (ns->relpath ns))

:default
(defn ns-url [ns]
  (let [f (ns->relpath ns)]
    (or (io/resource f)
        (io/resource (str f "c")))))
		
)

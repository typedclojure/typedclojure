;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.checker.base-env
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env]
            [clojure.core.typed.base-env-helper-cljs :as h]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set]
            [typed.cljc.checker.base-env-common :refer [delay-and-cache-env] :as common]
            [typed.cljc.checker.datatype-env :as datatype-env]
            [typed.cljc.checker.declared-kind-env :as declared-kind-env]
            [typed.cljc.checker.name-env :as name-env]
            [typed.cljc.checker.protocol-env :as protocol-env]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljs.checker.jsnominal-env :as jsnominal]
            [typed.cljs.checker.util :as ucljs]))

(ucljs/with-cljs-typed-env
(ucljs/with-core-cljs-typed
(binding [ana/*cljs-ns* 'cljs.core.typed]

#_
(ann-jsclass js/Document
  :extends
  :implements 
  :properties
  {getElementById [cljs.core.typed/JSString -> (U nil js/HTMLElement)]}
  :static-properties
  )

(delay-and-cache-env ^:private init-jsnominals 
  (h/jsnominal-mappings)
  #_
  (h/jsnominal-mappings

; seems like a good place to put this
;; FIXME this is actually js/String, delete
string [[]
        :fields
        {}
        :methods
        {toLowerCase [-> cljs.core.typed/JSstring]}]
    
js/Document [[]
          :fields
          {}
          :methods
          {getElementById [cljs.core.typed/JSstring -> (U nil js/HTMLElement)]}]

js/HTMLElement [[]
             :fields
             {innerHTML cljs.core.typed/JSstring
              tagName (U nil cljs.core.typed/JSstring)}]
    
    
js/Event [[]
       :methods
       {preventDefault [-> nil]}]


    ;http://dom.spec.whatwg.org/#interface-eventtarget
js/EventTarget [[]]
    
goog.events.Listenable [[]]
goog.events.EventTarget [[]]
    ))

(defn reset-jsnominal-env! []
  (impl/with-cljs-impl
    (jsnominal/reset-jsnominal!
     (init-jsnominals))))

;;; vars specific to cljs
(delay-and-cache-env ^:private init-var-env
  (reset-jsnominal-env!)
  (h/var-mappings)
  #_
  (merge
   (h/var-mappings

cljs.core/+ (IFn [cljs.core.typed/CLJSInteger * -> cljs.core.typed/CLJSInteger]
                 [cljs.core.typed/JSnumber * -> cljs.core.typed/JSnumber])
cljs.core/- (IFn [cljs.core.typed/CLJSInteger * -> cljs.core.typed/CLJSInteger]
                 [cljs.core.typed/JSnumber * -> cljs.core.typed/JSnumber])
cljs.core/* (IFn [cljs.core.typed/CLJSInteger * -> cljs.core.typed/CLJSInteger]
                 [cljs.core.typed/JSnumber * -> cljs.core.typed/JSnumber])
cljs.core/nth (All [x y]
                (IFn [(t/Seqable x) cljs.core.typed/CLJSInteger -> x]
                     [(t/Seqable x) cljs.core.typed/CLJSInteger y -> (U y x)]))

cljs.core/*flush-on-newline* cljs.core.typed/JSBoolean
cljs.core/*print-newline* cljs.core.typed/JSBoolean
cljs.core/*print-readably* cljs.core.typed/JSBoolean
cljs.core/*print-meta* cljs.core.typed/JSBoolean
cljs.core/*print-dup* cljs.core.typed/JSBoolean
cljs.core/*print-length* (U nil cljs.core.typed/CLJSInteger)

cljs.core/truth_ [Any -> Any]

cljs.core/coercive-= [Any Any -> cljs.core.typed/JSBoolean]

cljs.core/nil? (Pred nil)
cljs.core/undefined? (Pred JSUndefined)

cljs.core/array? (ReadOnlyArray Any)

cljs.core/object? [Any -> cljs.core.typed/JSBoolean]

cljs.core/native-satisfies? [Any Any -> Any]

cljs.core/is_proto_ [Any -> Any]

cljs.core/*main-cli-fn* (U nil [Any * -> Any])

cljs.core/missing-protocol [Any Any -> Any]
cljs.core/type->str [Any -> cljs.core.typed/JSString]

cljs.core/make-array (All [r] 
                          (IFn [cljs.core.typed/CLJSInteger -> (Array r)]
                               [Any cljs.core.typed/CLJSInteger -> (Array r)]))

cljs.core/array (All [r]
                     [r * -> (Array r)])

cljs.core/alength [(ReadOnlyArray Any) -> cljs.core.typed/CLJSInteger]

cljs.core/into-array (All [x] 
                          (IFn [(t/Seqable x) -> (Array x)]
                               [Any (t/Seqable x) -> (Array x)]))

cljs.core/pr-str* [Any -> cljs.core.typed/JSString]

cljs.core/clone [Any -> Any]

cljs.core/cloneable? (Pred cljs.core/ICloneable)


cljs.core/count
      ; TODO also accepts Counted
      ; FIXME should return integer
      [(t/Seqable Any) -> cljs.core.typed/CLJSInteger :object {:id 0, :path [Count]}]
cljs.core/prim-seq
      (All [x]
           [(t/Seqable x) -> (t/NilableNonEmptySeq x)])

cljs.core/key-test [Keyword Any -> cljs.core.typed/JSBoolean]

cljs.core/fn? [Any -> cljs.core.typed/JSBoolean]
cljs.core/ifn? [Any -> cljs.core.typed/JSBoolean]

;;pop needs to be defined here because
;;definition of List differs between clj and cljs
cljs.core/pop (All [x]
                      (IFn
                        [(IList x) -> (IList x)]
                        [(Vec x) -> (Vec x)]
                        [(Stack x) -> (Stack x)]))

cljs.core/clj->js [Any -> Any]
cljs.core/js->clj [Any -> Any]
cljs.core/js-obj  [Any * -> Any]

;;pseudo-private vars
cljs.core/-conj [Any Any -> Any]
;cljs.core.List.Empty (IList Any)
)))

(delay-and-cache-env ^:private init-var-nochecks
  (set (keys (init-var-env))))

(delay-and-cache-env init-jsvar-env
  (reset-jsnominal-env!)
  (h/js-var-mappings)
  #_
  (h/js-var-mappings
;; js
    
js/document js/Document

;; goog.dom

goog.dom/setTextContent [js/Element (U cljs.core.typed/JSString cljs.core.typed/JSnumber) -> js/Window]
goog.dom/getElementsByTagNameAndClass 
      [(U nil cljs.core.typed/JSString) (U nil cljs.core.typed/JSString) (U nil js/Document js/Element) -> (t/Seqable js/Element)]
goog.dom.classes/set [(U js/Node nil) cljs.core.typed/JSString -> Any]
goog.dom.classes/add [(U js/Node nil) (U nil cljs.core.typed/JSString) * -> cljs.core.typed/JSBoolean]
goog.dom.classes/remove [(U js/Node nil) (U nil cljs.core.typed/JSString) * -> cljs.core.typed/JSBoolean]
goog.style/getPageOffsetLeft [(U nil js/Element) -> cljs.core.typed/JSnumber]
goog.style/getPageOffsetTop [(U nil js/Element) -> cljs.core.typed/JSnumber]
goog.events/listen [(U nil js/EventTarget goog.events.EventTarget goog.events.Listenable)
                    (U nil cljs.core.typed/JSString (ReadOnlyArray cljs.core.typed/JSString)) -> cljs.core.typed/JSnumber]

goog.events.EventType.KEYUP   cljs.core.typed/JSString
goog.events.EventType.KEYDOWN cljs.core.typed/JSString
goog.events.EventType.KEYPRESS cljs.core.typed/JSString
goog.events.EventType.CLICK   cljs.core.typed/JSString
goog.events.EventType.DBLCLICK cljs.core.typed/JSString
goog.events.EventType.MOUSEOVER cljs.core.typed/JSString
goog.events.EventType.MOUSEOUT cljs.core.typed/JSString
goog.events.EventType.MOUSEMOVE cljs.core.typed/JSString
    )))))

#_
(defn reset-envs! []
  (ucljs/with-cljs-typed-env
    (impl/with-cljs-impl
      ;(var-env/reset-var-type-env! (init-var-env) (init-var-nochecks))
      (var-env/reset-jsvar-type-env! (init-jsvar-env))
      (reset-jsnominal-env!)
      )
    )
  nil)

;;FIXME hack
#_
(reset-envs!)

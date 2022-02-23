;From David Nolen's blog
(ns cljs.core.typed.test.dnolen.utils.dom
  (:require [goog.style :as style]
            [goog.dom :as dom]
            [goog.dom.classes :as classes])
  (:require [cljs.core.typed :as t :refer-macros [ann]]))

(ann by-id [t/JSstring -> (t/U nil js/HTMLElement)])
(defn by-id [id]
  (.getElementById js/document id))

(ann set-html! [js/HTMLElement t/JSstring -> t/JSstring])
(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(ann set-text! [js/Element (t/U t/JSstring t/JSnumber) -> js/Window])
(defn set-text! [el s]
  (dom/setTextContent el s))

(ann set-class! [(t/U js/Node nil) t/JSstring -> t/Any])
(defn set-class! [el name]
  (classes/set el name))

(ann add-class! [(t/U nil js/Node) t/JSstring -> t/JSboolean])
(defn add-class! [el name]
  (classes/add el name))

(ann remove-class! [(t/U js/Node nil) (t/U nil t/JSstring) -> t/JSboolean])
(defn remove-class! [el name]
  (classes/remove el name))

(ann tag-match [t/JSstring -> [js/HTMLElement -> t/Any]])
(defn tag-match [tag]
  (fn [el]
    (when-let [tag-name (.-tagName el)]
      (t/ann-form tag-name t/JSstring)
      (= tag (.toLowerCase tag-name)))))

(ann el-matcher [t/Any -> [t/Any -> t/Any]])
(defn el-matcher [el]
  (fn [other] (identical? other el)))

(ann by-tag-name [(t/U nil js/Document js/Element) (t/U nil t/JSstring) 
                  -> (t/U nil (ISeq js/Element))])
(defn by-tag-name [el tag]
  (prim-seq (dom/getElementsByTagNameAndClass tag nil el)))

(ann offset [(t/U nil js/Element) -> '[t/JSnumber t/JSnumber]])
(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

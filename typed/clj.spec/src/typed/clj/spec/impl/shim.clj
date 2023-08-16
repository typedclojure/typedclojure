;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.spec.impl.shim
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec :as s2]
            [clojure.spec.alpha :as s1]
            [clojure.alpha.spec.gen :as gen2]
            [clojure.alpha.spec.protocols :as protocols2]
            [typed.clj.spec1.util :as u1]))

(defn resolve-spec
  [qform {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (u1/resolve-spec qform)
    2 (s2/resolve-spec qform)))

(defn valid? 
  [{:keys [spec-version] :as version-info} s x]
  (case (int spec-version)
    1 (s1/valid? s x)
    2 (s2/valid? s x)))

(defn gen* [s {:keys [overrides path rmap]} {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (s1/gen* (s1/spec s) overrides path rmap)
    2 (protocols2/gen* s overrides path rmap)))

(defn gen
  ([spec {:keys [spec-version] :as version-info}]
   (case (int spec-version)
     1 (s1/gen spec)
     2 (s2/gen spec)))
  ([spec overrides {:keys [spec-version] :as version-info}]
   (case (int spec-version)
     1 (s1/gen spec overrides)
     2 (s2/gen spec overrides))))

(defn conform* [s {:keys [x settings-key settings]} {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (s1/conform* (s1/spec s) x)
    2 (protocols2/conform* s x settings-key settings)))

(defn explain* [s {:keys [path via in x settings settings-key]} {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (s1/explain* (s1/spec s) path via in x)
    2 (protocols2/explain* s path via in x settings settings-key)))

(defn invalid-kw [{:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 ::s1/invalid
    2 ::s2/invalid))

(defn invalid? [x {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (s1/invalid? x)
    2 (s2/invalid? x)))

(defn describe [s {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (s1/describe s)
    2 (s2/describe s)))

(defn form [s {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (s1/form s)
    2 (s2/form s)))

(defn conform [s x {:keys [spec-version] :as version-info}]
  (case (int spec-version)
     1 (s1/conform s x)
     2 (s2/conform s x)))

(defn explain-data [s x {:keys [spec-version] :as version-info}]
  (case (int spec-version)
     1 (s1/explain-data s x)
     2 (s2/explain-data s x)))

(defn explain-out [x {:keys [spec-version] :as version-info}]
  (case (int spec-version)
     1 (s1/explain-out x)
     2 (s2/explain-out x)))

(defn reg-resolve! [x {:keys [spec-version] :as version-info}]
  (case (int spec-version)
    1 (#'s1/reg-resolve! x)
    2 (#'s2/reg-resolve! x)))

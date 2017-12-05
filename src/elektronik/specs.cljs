(ns elektronik.specs
  (:require [cljs.spec.alpha :as s]
            [om.next :as om]))

(def default-validator s/valid?)

;; panel
;; ----------------------------------------------------------------------------
(s/def :panel/id keyword?)
(s/def :panel/name string?)
(s/def :panel/expanded? boolean?)

(s/def ::panel
  (s/keys :req [:panel/id :panel/name]
          :opt [:panel/expanded?]))


;; panels
;; ----------------------------------------------------------------------------
(s/def ::panels
  (s/coll-of ::panel :kind vector?))


;; factory
;; ----------------------------------------------------------------------------
(s/def :factory/type keyword?)
(s/def :factory/fn keyword?)
(s/def :factory/name string?)
(s/def :factory/desc string?)

(s/def ::factory
  (s/keys :req [:factory/type :factory/name]
          :opt [:factory/desc]))


;; instance
;; ----------------------------------------------------------------------------
(s/def :db/id om/tempid?)
(s/def :instance/factory ::factory)
(s/def :instance/x integer?)
(s/def :instance/y integer?)

(s/def ::instance
  (s/keys :req [:db/id :instance/factory :instance/x :instance/y]))
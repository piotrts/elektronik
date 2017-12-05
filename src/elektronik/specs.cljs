(ns elektronik.specs
  (:require [cljs.spec.alpha :as s]
            [om.next :as om]))

(s/def :factory/type keyword?)
(s/def :factory/name string?)
(s/def :factory/desc string?)

(s/def ::factory
  (s/keys :req [:factory/type :factory/name]
          :opt [:factory/desc]))

(s/def :db/id om/tempid?)
(s/def :instance/factory ::factory)
(s/def :instance/x integer?)
(s/def :instance/y integer?)

(s/def ::instance
  (s/keys :req [:db/id :instance/factory :instance/x :instance/y]))
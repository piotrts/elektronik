(ns elektronik.instance-inspector
  (:require [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui InstanceInspector
  Object
  (render [this]
    (dom/div #js{:id "instance-inspector"}
      "stub")))

(def instance-inspector (om/factory InstanceInspector))
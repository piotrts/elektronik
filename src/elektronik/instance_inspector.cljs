(ns elektronik.instance-inspector
  (:require [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui InstanceInspector
  static om/IQuery
  (query [this]
    [{:selection/list [:db/id]}])
  Object
  (render [this]
    (let [panel-data (get (om/props this) :panel/data)]
      (dom/div #js{:id "instance-inspector"}
        "Selected:" (str panel-data)))))

(def instance-inspector (om/factory InstanceInspector))
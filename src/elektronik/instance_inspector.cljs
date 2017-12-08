(ns elektronik.instance-inspector
  (:require [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui InstanceInspector
  static om/IQuery
  (query [this]
    [{:selection/list '[*]}])
  Object
  (render [this]
    (let [selected (get-in (om/props this) [:panel/data :selection/list 0])]
      (dom/div #js{:id "instance-inspector"}
        (if selected
          (str selected)
          "Nothing is selected")))))

(def instance-inspector (om/factory InstanceInspector))
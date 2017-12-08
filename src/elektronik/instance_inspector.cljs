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
          (map (fn [[param value]]
                 (dom/div #js{:key (str "instance-inspector-" (namespace param) "-" (name param))}
                   (dom/strong nil
                     (str param) ":")
                   (dom/input #js{:type "text"
                                  :value value})))
               selected)
          "Nothing is selected")))))

(def instance-inspector (om/factory InstanceInspector))
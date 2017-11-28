(ns elektronik.state-inspector
  (:require [clojure.pprint :as pprint]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui StateInspector
  Object
  (render [this]
    (let [props (om/props this)]
      (dom/div #js{:id "state-inspector"}
        (dom/button #js{:onClick #(.forceUpdate this)}
          "Refresh")
        (dom/pre nil
          (with-out-str
            (pprint/pprint (-> this om/get-reconciler om/app-state deref))))))))

(def state-inspector (om/factory StateInspector))
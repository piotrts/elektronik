(ns elektronik.state-inspector
  (:require [clojure.pprint :as pprint]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui StateInspector
  Object
  (initLocalState [this]
    {:expanded? true})
  (render [this]
    (let [props (om/props this)
          expanded? (om/get-state this :expanded?)]
      (dom/div #js{:id "state-inspector"}
        (dom/button #js{:onClick #(om/set-state! this {:expanded? (not expanded?)})}
          (if expanded?
            "Hide"
            "Show"))
        (dom/button #js{:onClick #(.forceUpdate this)}
          "Refresh")
        (when expanded?
          (dom/pre nil
            (with-out-str
              (pprint/pprint (-> this om/get-reconciler om/app-state deref)))))))))

(def state-inspector (om/factory StateInspector))
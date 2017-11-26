(ns elektronik.query-inspector
  (:require [clojure.pprint :as pprint]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui QueryInspector
  Object
  (initLocalState [this]
    {:expanded? true})
  (render [this]
    (let [props (om/props this)
          expanded? (om/get-state this :expanded?)]
      (dom/div #js{:id "query-inspector"}
        (dom/button #js{:onClick (fn [_]
                                   (om/set-state! this {:expanded? (not expanded?)}))}
          (if expanded?
            "Hide"
            "Show"))
        (when expanded?
          (dom/pre nil
            (with-out-str (pprint/pprint props))))))))

(def query-inspector (om/factory QueryInspector))
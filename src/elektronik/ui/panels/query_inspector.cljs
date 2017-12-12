(ns elektronik.ui.panels.query-inspector
  (:require [clojure.pprint :as pprint]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui QueryInspector
  Object
  (render [this]
    (let [reconciler (om/get-reconciler this)
          app-state (om/app-state reconciler)
          parser (-> reconciler :config :parser)
          root-query (om/get-query (om/app-root reconciler))]
      (dom/div #js{:id "query-inspector"}
        (dom/button #js{:onClick #(.forceUpdate this)}
          "Refresh")
        (dom/pre nil
          (with-out-str
            (pprint/pprint
              (parser {:state app-state} root-query))))))))

(def query-inspector (om/factory QueryInspector))
(ns elektronik.query-inspector
  (:require [clojure.pprint :as pprint]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(defui QueryInspector
  Object
  (render [this]
    (let [props (om/props this)]
      (dom/pre nil
        (with-out-str (pprint/pprint props))))))

(def query-inspector (om/factory QueryInspector))
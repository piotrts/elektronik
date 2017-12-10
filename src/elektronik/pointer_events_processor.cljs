(ns elektronik.pointer-events-processor
  (:require [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [goog.style :as gstyle]))

(def pointer-state (atom [:none]))
(def pointer-deltas (atom 0))

(defn get-element-data-instance-id [e]
  (aget (.-dataset e) "instanceId"))

(defn pointer-event->svg-rect [ev]
  (some-> ev
    .-target
    (gdom/getAncestor get-element-data-instance-id true 2)))

(defn pointer-event->instance-db-id [ev]
  (some-> ev
    pointer-event->svg-rect
    get-element-data-instance-id
    reader/read-string))

(defn pointer-event->pointer-state [ev]
  (let [state (last @pointer-state)
        type (.-type ev)]
    (case [state type]
      [:none "mousedown"] [:down]
      [:none "mousemove"] [:none]
      [:none "mouseup"] [:none]
      [:down "mousedown"] [:down]
      [:down "mousemove"] [:select :drag]
      [:down "mouseup"] [:select]
      [:drag "mousedown"] [:none]
      [:drag "mousemove"] [:drag]
      [:drag "mouseup"] [:none]
      [:select "mousedown"] [:down]
      [:select "mousemove"] [:none]
      [:select "mouseup"] [:none])))

(defn process-select-event [component pointer-state ev]
  (if-let [instance-db-id (pointer-event->instance-db-id ev)]
    (do
      (om/transact! component `[(selection/clear)
                                (selection/add-instance {:instance/id ~instance-db-id})])
      (reset! pointer-deltas (let [svg-rect-node (pointer-event->svg-rect ev)
                                   rel (gstyle/getRelativePosition ev svg-rect-node)]
                               [(.-x rel) (.-y rel)])))
    (om/transact! component '[(selection/clear)])))

(defn process-drag-event [component pointer-state ev]
  (let [reconciler (om/get-reconciler component)
        state @(om/app-state reconciler)]
    (when (seq (:selection/list state))
      (let [svg-node (om/react-ref component "svg-container")
            [dx dy] @pointer-deltas
            rel (gstyle/getRelativePosition ev svg-node)
            x (- (.-x rel) dx)
            y (- (.-y rel) dy)]
        (om/transact! component `[(selection/drag {:x ~x :y ~y})])))))

(defn pointer-events-processor [component ev]
  (let [new-pointer-state (pointer-event->pointer-state ev)]
    (when (some #{:select} new-pointer-state)
      (process-select-event component new-pointer-state ev))
    (when (some #{:drag} new-pointer-state)
      (process-drag-event component new-pointer-state ev))
    (reset! pointer-state new-pointer-state)))
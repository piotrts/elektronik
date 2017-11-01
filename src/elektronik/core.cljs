(ns elektronik.core
  (:require [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [goog.style :as gstyle]))

(enable-console-print!)

(def addition-component-factory
  #:factory{:type :math/addition
            :name "+"
            :desc "Addition"})

(def app-state
  {:ui/screen {:width "100%"
               :height "100%"}
   :instances/selected []
   :instances/list
   [{:instance/id 1
     :instance/factory addition-component-factory
     :instance/x 0
     :instance/y 0}]})

(def stylesheet
  {:svg #js{}
   :instance #js{:fill "gray"
                 :strokeWidth 1
                 :stroke "black"}})

(defui Factory
  static om/Ident
  (ident [this props]
    (let [{:keys [factory/type]} props]
      [:factories/by-type type]))
  static om/IQuery
  (query [this]
    [:factory/type :factory/name :factory/desc]))

(defui Instance
  static om/Ident
  (ident [this props]
    (let [{:keys [instance/id]} props]
      [:instances/by-id id]))
  static om/IQuery
  (query [query]
    (let [factory-query (om/get-query Factory)]
      `[:instance/id {:instance/factory ~factory-query}]))
  Object
  (render-rect [this]
    (let [{:keys [instance/id]} (om/props this)]
      (dom/rect #js{:style (:instance stylesheet)
                    :onClick (fn [_]
                               (om/transact! this `[(selection/add-instance {:instance/id ~id})]))
                    :width 50
                    :height 50})))
  (render-text [this]
    (let [{:keys [instance/id]
           {:keys [factory/name factory/desc]} :instance/factory} (om/props this)]
      (dom/text #js{:x 0 :y 0 :stroke "none" :fill "red" :alignmentBaseline "hanging" :fontSize 15}
        (str id) ", " name ", " desc)))
  (render [this]
    (dom/g nil
      (.render-rect this)
      (.render-text this))))

(def instance (om/factory Instance))

(defui Root
  static om/IQuery
  (query [this]
    (let [instance-query (om/get-query Instance)]
      `[:ui/screen
        {:instances/list ~instance-query}]))
  Object
  (render [this]
    (let [{:keys [instances/list]
           {:keys [width height]} :ui/screen} (om/props this)]
        (dom/svg #js{:ref "svg-container"
                     :style (:svg stylesheet)
                     :width "100%"
                     :height "100%"
                     :onDoubleClick (fn [ev]
                                      (let [svg-node (om/react-ref this "svg-container")
                                            target   (.-target ev)]
                                        (when (= svg-node target)
                                          (let [position (gstyle/getRelativePosition ev svg-node)
                                                x (.-x position)
                                                y (.-y position)]
                                            (om/transact! this `[(instance/create {:instance/x ~x :instance/y ~y})])))))}
          (map instance list)))))

(defmulti read om/dispatch)
(defmulti mutate om/dispatch)

(defmethod read :instances/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :default [{:keys [state]} k params]
  (let [st @state]
    (if-let [[_ v] (find st k)]
      {:value v}
      {:value :not-found})))

(defmethod mutate 'selection/add-instance [{:keys [state]} _ {:keys [instance/id]}]
  (let [ident [:instances/by-id id]]
    {:action #(swap! state update :instances/selected conj ident)}))

(defmethod mutate 'instance/create [{:keys [state]} _ {:keys [instance/x instance/y]}]
  (let [new-instance #:instance{:id (om/tempid)
                                :factory addition-component-factory
                                :x x
                                :y y}]
    {:action #(swap! state update :instances/list conj new-instance)}))

(def parser
  (om/parser {:read   read
              :mutate mutate
              :normalize true}))

(def reconciler
  (om/reconciler {:state  app-state
                  :parser parser}))

(om/add-root! reconciler Root (gdom/getElement "app"))

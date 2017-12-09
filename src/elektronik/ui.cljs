(ns elektronik.ui
  (:require [elektronik.query-inspector :as query-inspector]
            [elektronik.state-inspector :as state-inspector]
            [elektronik.instance-inspector :as instance-inspector]
            [elektronik.specs :as specs]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [goog.style :as gstyle]))

(def stylesheet
  {:instance
   {:rect #js{:fill "gray"
              :strokeWidth 1
              :stroke "black"}
    :text #js{:pointerEvents "none"}}})

(def panel-id->component
  {:query-inspector query-inspector/QueryInspector
   :state-inspector state-inspector/StateInspector
   :instance-inspector instance-inspector/InstanceInspector})

(def panel-id->factory
  {:query-inspector query-inspector/query-inspector
   :state-inspector state-inspector/state-inspector
   :instance-inspector instance-inspector/instance-inspector})

(def shared
  {:panel-id->component panel-id->component
   :panel-id->factory panel-id->factory})

(defn toolbar-button [x factory]
  (let [{:keys [factory/name factory/id]} factory]
    (dom/button #js{:key (str "toolbar-factory-" name)
                    :onClick (fn [_]
                               (om/transact! x `[(instance/create {:factory/id ~id})]))}
      name)))

(defui Toolbar
  Object
  (render [this]
    (let [{:keys [factories/list]} (om/props this)]
      (dom/div nil
        (map #(toolbar-button this %) list)))))

(def toolbar (om/factory Toolbar))

(defui Factory
  static om/Ident
  (ident [this props]
    (let [{:keys [factory/id]} props]
      [:factories/by-id id]))
  static om/IQuery
  (query [this]
    [:factory/id :factory/name :factory/desc]))

(defui Instance
  static om/Ident
  (ident [this props]
    (let [{:keys [instance/id]} props]
      [:instances/by-id id]))
  static om/IQuery
  (query [this]
    (let [factory-query (om/get-query Factory)]
      `[:instance/id :instance/x :instance/y {:instance/factory ~factory-query}]))
  Object
  (render [this]
    (let [{:keys [render-instance]} (om/get-computed this)]
      (render-instance this))))

(def instance (om/factory Instance {:validator #(specs/default-validator ::specs/instance %)}))

(defui Link
  static om/IQuery
  (query [this]
    `[{:link/from [:instance/x :instance/y]}
      {:link/to [:instance/x :instance/y]}])
  Object
  (render [this]
    (let [{{from-x :instance/x
            from-y :instance/y} :link/from
           {to-x :instance/x
            to-y :instance/y} :link/to} (om/props this)]
      (dom/line #js{:x1 from-x
                    :y1 from-y
                    :x2 to-x
                    :y2 to-y
                    :style #js{:stroke "rgb(0,0,0)"
                               :strokeWidth 1}}))))

(def link (om/factory Link))

(defui Links
  static om/IQuery
  (query [this]
    (let [link-query (om/get-query Link)]
      `[~@link-query]))
  Object
  (render [this]
    (let [{:keys [links/list]} (om/props this)]
      (dom/g nil
        (mapv link list)))))

(def links (om/factory Links))

(def utf-arrow-collapse \u25B4)
(def utf-arrow-expand \u25BE)

(defui Panel
  static om/Ident
  (ident [this props]
    (let [{:keys [panel/id]} props]
      [:panels/by-id id]))
  static om/IQuery
  (query [this]
    (let [subcomponents (vals panel-id->component)]
      `[:panel/id :panel/name :panel/expanded? ~@(mapcat om/get-query subcomponents)]))
  Object
  (render [this]
    (let [{:keys [panel/id panel/expanded? panel/name] :as panel-props} (om/props this)
          panel-subfactory ((om/shared this :panel-id->factory) id)]
      (dom/div #js{:id id
                   :className "panel"}
        (dom/button #js{:onClick #(om/transact! this `[(panel/toggle {:panel/id ~id})])}
          (if expanded?
            utf-arrow-collapse
            utf-arrow-expand)
          " "
          name)
        (when expanded?
          (panel-subfactory panel-props))))))

(def panel (om/factory Panel {:validator #(specs/default-validator ::specs/panel %)}))

(defui Panels
  Object
  (render [this]
    (let [{:keys [panels/list]} (om/props this)]
      (dom/div #js{:id "panels"}
        (map panel list)))))

(def panels (om/factory Panels {:validator #(specs/default-validator ::specs/panels (:panels/list %))})) ; temporary

(def pointer-state (atom :none))

(defn get-element-data-instance-id [e]
  (aget (.-dataset e) "instanceId"))

(defn pointer-event->instance-db-id [ev]
  (some-> ev
    .-target
    (gdom/getAncestor get-element-data-instance-id true 2)
    get-element-data-instance-id
    reader/read-string))

(defn pointer-event->pointer-state [ev]
  (let [state @pointer-state
        type (.-type ev)]
    (case [state type]
      [:none "mousedown"] :down
      [:none "mousemove"] :none
      [:none "mouseup"] :none
      [:down "mousedown"] :down
      [:down "mousemove"] :drag
      [:down "mouseup"] :select
      [:drag "mousedown"] :none
      [:drag "mousemove"] :drag
      [:drag "mouseup"] :none
      [:select "mousedown"] :down
      [:select "mousemove"] :drag
      [:select "mouseup"] :select)))

(defn pointer-events-processor [component ev]
  (let [new-pointer-state (pointer-event->pointer-state ev)]
    (if (= :select new-pointer-state)
      (let [instance-db-id (pointer-event->instance-db-id ev)]
        (om/transact! component (cond-> '[(selection/clear)]
                                  instance-db-id (conj `(selection/add-instance {:instance/id ~instance-db-id}))))))
    (reset! pointer-state new-pointer-state)))

(defui SVGRenderer
  Object
  (render-instance [_ this]
    (let [{:keys [instance/id instance/x instance/y]
           {:keys [factory/name factory/desc]} :instance/factory} (om/props this)]
      (dom/g #js{:data-instance-id (str id)}
        (dom/rect #js{:style (get-in stylesheet [:instance :rect])
                      :x x
                      :y y
                      :width 50
                      :height 50})
        (dom/text #js{:style (get-in stylesheet [:instance :text])
                      :x x
                      :y y
                      :stroke "none"
                      :fill "white"
                      :alignmentBaseline "hanging"
                      :fontSize 20}
          name))))
  (render [this]
    (let [{:keys [instances/list]
           {:keys [width height]} :ui/screen
           :as props} (om/props this)]
      (dom/svg #js{:ref "svg-container"
                   :style (:svg stylesheet)
                   :width "100%"
                   :height "100%"
                   :onMouseDown #(pointer-events-processor this %)
                   :onMouseMove #(pointer-events-processor this %)
                   :onMouseUp #(pointer-events-processor this %)}
        (map (fn [instance-props]
               (instance (om/computed
                           instance-props
                           {:render-instance (.-render-instance this)})))
             list)
        (links props)))))

(def svg-renderer (om/factory SVGRenderer))

(defui Root
  static om/IQuery
  (query [this]
    (let [factory-query (om/get-query Factory)
          instance-query (om/get-query Instance)
          links-query (om/get-query Links)
          panel-query (om/get-query Panel)]
      [:ui/screen
       {:factories/list factory-query}
       {:instances/list instance-query}
       {:links/list links-query}
       {:panels/list panel-query}]))
  Object
  ;(on-double-click [this ev])
  ;  (let [svg-node (om/react-ref this "svg-container")]))
  ;        target   (.-target ev)]))
  ;    (when (= svg-node target))))
  ;      (let [position (gstyle/getRelativePosition ev svg-node)]))))
  ;            x (.-x position)]))))
  ;            y (.-y position)]))))
  ;        (om/transact! this `[(instance/create #:instance{:type :math/addition :x ~x :y ~y})])))))
  (render [this]
    (let [props (om/props this)]
      (dom/div #js{:id "elektronik"}
        (toolbar props)
        (panels props)
        (svg-renderer props)))))

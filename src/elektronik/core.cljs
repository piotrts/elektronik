(ns elektronik.core
  (:require [elektronik.query-inspector :as query-inspector]
            [elektronik.state-inspector :as state-inspector]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [goog.style :as gstyle]))

(enable-console-print!)

(def addition-component-factory
  #:factory{:type :math/addition
            :name "+"
            :desc "Addition"})

(def subtraction-component-factory
  #:factory{:type :math/subtraction
            :name "-"
            :desc "Subtraction"})

(def app-state
  (let [instance-1-id (om/tempid)
        instance-2-id (om/tempid)]
    {:ui/screen {:width "100%"
                 :height "100%"}
     :panels/list [{:panel/id :query-inspector
                    :panel/name "Query Inspector"
                    :panel/expanded? true}
                   {:panel/id :state-inspector
                    :panel/name "State Inspector"
                    :panel/expanded? true}]
     :instances/selected []
     :instances/list [{:db/id instance-1-id
                       :instance/factory addition-component-factory
                       :instance/x 10
                       :instance/y 10}
                      {:db/id instance-2-id
                       :instance/factory subtraction-component-factory
                       :instance/x 110
                       :instance/y 10}]
     :links/list [[[:instances/by-id instance-1-id] [:instances/by-id instance-2-id]]]}))

(def stylesheet
  {:instance #js{:fill "gray"
                 :strokeWidth 1
                 :stroke "black"}})

(defn toolbar-button [x factory]
  (let [{:keys [factory/name factory/type]} factory]
    (dom/button #js{:key (str "toolbar-factory-" name)
                    :onClick (fn [_]
                               (om/transact! x `[(instance/create {:instance/type ~type})]))}
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
    (let [{:keys [factory/type]} props]
      [:factories/by-type type]))
  static om/IQuery
  (query [this]
    [:factory/type :factory/name :factory/desc]))

(defui Instance
  static om/Ident
  (ident [this props]
    (let [{:keys [db/id]} props]
      [:instances/by-id id]))
  static om/IQuery
  (query [query]
    (let [factory-query (om/get-query Factory)]
      `[:db/id :instance/x :instance/y {:instance/factory ~factory-query}]))
  Object
  (render [this]
    (let [{:keys [db/id]} (om/props this)
          {:keys [instance-render-rect instance-render-text]} (om/get-computed this)]
      (dom/g #js{:data-instance-id (str id)}
        (instance-render-rect this)
        (instance-render-text this)))))

(def instance (om/factory Instance))

(defui Panel
  static om/Ident
  (ident [this props]
    (let [{:keys [panel/id]} props]
      [:panels/by-id id]))
  static om/IQuery
  (query [this]
    [:panel/id :panel/name :panel/expanded?])
  Object
  (componentWillUpdate [this next-props _]
    (let [{:keys [panel/id]} next-props
          panel-subcomponent ((om/shared this :panel-id->component) id)
          panel-subquery (om/get-query panel-subcomponent)]
      (when (seq panel-subquery)
        (om/update-query! this update :query conj panel-subquery))))
  (render [this]
    (let [{:keys [panel/id panel/expanded? panel/name] :as panel-props} (om/props this)
          panel-subfactory ((om/shared this :panel-id->factory) id)]
      (dom/div #js{:id id
                   :className "panel"}
        (dom/button #js{:onClick #(om/transact! this `[(panel/toggle {:panel/id ~id})])}
          (if expanded?
            "[-]"
            "[+]")
          " "
          name)
        (when expanded?
          (panel-subfactory panel-props))))))

(def panel (om/factory Panel))

(defui Panels
  Object
  (render [this]
    (let [{:keys [panels/list]} (om/props this)]
      (dom/div #js{:id "panels"}
        (map panel list)))))

(def panels (om/factory Panels))

(def pointer-state (atom :none))

(reader/register-tag-parser! 'om/id #(first %))

(defn get-element-data-instance-id [e]
  (aget (.-dataset e) "instanceId"))

(defn pointer-event->instance-db-id [ev]
  (some-> ev
    .-target
    (gdom/getAncestor get-element-data-instance-id true 2)
    get-element-data-instance-id
    reader/read-string
    om/tempid))

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
        (om/transact! component `[(selection/add-instance {:db/id ~instance-db-id}) :instance/list])))
    (reset! pointer-state new-pointer-state)))

(defui SVGRenderer
  Object
  (instance-render-rect [_ this]
    (let [{:keys [db/id instance/x instance/y]} (om/props this)]
      (dom/rect #js{:style (:instance stylesheet)
                    :x x
                    :y y
                    :width 50
                    :height 50})))
  (instance-render-text [_ this]
    (let [{:keys [db/id instance/x instance/y]
           {:keys [factory/name factory/desc]} :instance/factory} (om/props this)]
      (dom/text #js{:x x :y y :stroke "none" :fill "white" :alignmentBaseline "hanging" :fontSize 20}
        name)))
  (render [this]
    (let [{:keys [instances/list]
           {:keys [width height]} :ui/screen} (om/props this)]
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
                           {:instance-render-rect (.-instance-render-rect this)
                            :instance-render-text (.-instance-render-text this)})))
             list)))))

(def svg-renderer (om/factory SVGRenderer))

(defui Root
  static om/IQuery
  (query [this]
    (let [factory-query (om/get-query Factory)
          instance-query (om/get-query Instance)
          panel-query (om/get-query Panel)]
      `[:ui/screen
        {:factories/list ~factory-query}
        {:instances/list ~instance-query}
        {:panels/list ~panel-query}]))
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

(defmulti read om/dispatch)
(defmulti mutate om/dispatch)

(defmethod read :factories/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (vals (get st :factories/by-type))}))

(defmethod read :instances/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :panels/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :default [{:keys [state]} k params]
  (let [st @state]
    (if-let [[_ v] (find st k)]
      {:value v}
      {:value :not-found})))

(defmethod mutate 'selection/add-instance [{:keys [state]} _ {:keys [db/id]}]
  (let [ident [:instances/by-id id]]
    {:action #(swap! state update :instances/selected conj ident)}))

(defmethod mutate 'instance/create [{:keys [query state]} _ {:keys [instance/type instance/x instance/y]}]
  (let [x (or x (rand-int 500))
        y (or y (rand-int 500))
        tempid (om/tempid)
        instance-ident [:instances/by-id tempid]
        factory-ident [:factories/by-type type]
        new-instance #:instance{:db/id tempid
                                :factory factory-ident
                                :x x
                                :y y}]
    {:action (fn []
               (swap! state assoc-in instance-ident new-instance)
               (swap! state update :instances/list conj instance-ident))}))

(defmethod mutate 'panel/toggle [{:keys [state]} _ {:keys [panel/id]}]
  (let [ident [:panels/by-id id]]
    {:action #(swap! state update-in (conj ident :panel/expanded?) not)}))

;(defn transpile-static [state])
;  (let [resolve-instance (fn [ident])]))
;                           (get-in state ident))]))
;        {:keys [links/list]} state]))
;    (resolve-instance (ffirst list))))

(def panel-id->component
  {:query-inspector query-inspector/QueryInspector
   :state-inspector state-inspector/StateInspector})

(def panel-id->factory
  {:query-inspector query-inspector/query-inspector
   :state-inspector state-inspector/state-inspector})

(def parser
  (om/parser
    {:read read
     :mutate mutate}))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser parser
     :normalize true
     :shared {:panel-id->component panel-id->component
              :panel-id->factory panel-id->factory}
     :id-key :db/id}))

(om/add-root! reconciler Root (gdom/getElement "app"))

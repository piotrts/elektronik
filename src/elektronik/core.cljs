(ns elektronik.core
  (:require [elektronik.query-inspector :as query-inspector]
            [elektronik.state-inspector :as state-inspector]
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
  (render-rect [this]
    (let [{:keys [db/id instance/x instance/y]} (om/props this)]
      (dom/rect #js{:style (:instance stylesheet)
                    :onClick (fn [_]
                               (om/transact! this `[(selection/add-instance {:db/id ~id}) :instance/list]))
                    :x x
                    :y y
                    :width 50
                    :height 50})))
  (render-text [this]
    (let [{:keys [db/id instance/x instance/y]
           {:keys [factory/name factory/desc]} :instance/factory} (om/props this)]
      (dom/text #js{:x x :y y :stroke "none" :fill "white" :alignmentBaseline "hanging" :fontSize 20}
        name)))
  (render [this]
    (dom/g nil
      (.render-rect this)
      (.render-text this))))

(def instance (om/factory Instance))

(def panel-id->component
  {:query-inspector query-inspector/QueryInspector
   :state-inspector state-inspector/StateInspector})

(def panel-id->factory
  {:query-inspector query-inspector/query-inspector
   :state-inspector state-inspector/state-inspector})

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
          component (panel-id->component id)
          subquery (om/get-query component)]
      (when (seq subquery)
        (om/update-query! this update :query conj subquery))))
  (render [this]
    (let [{:keys [panel/id panel/expanded? panel/name] :as panel-props} (om/props this)]
      (dom/div #js{:id id
                   :className "panel"}
        (dom/button #js{:onClick #(om/transact! this `[(panel/toggle {:panel/id ~id})])}
          (if expanded?
            "[-]"
            "[+]")
          " "
          name)
        (when expanded?
          (case id
            :query-inspector (query-inspector/query-inspector panel-props)
            :state-inspector (state-inspector/state-inspector panel-props)))))))

(def panel (om/factory Panel))

(defui Panels
  Object
  (render [this]
    (let [{:keys [panels/list] :as props} (om/props this)]
      (dom/div #js{:id "panels"}
        (map panel list)))))

(def panels (om/factory Panels))

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
  (on-double-click [this ev]
    (let [svg-node (om/react-ref this "svg-container")
          target   (.-target ev)]
      (when (= svg-node target)
        (let [position (gstyle/getRelativePosition ev svg-node)
              x (.-x position)
              y (.-y position)]
          (om/transact! this `[(instance/create #:instance{:type :math/addition :x ~x :y ~y})])))))
  (render [this]
    (let [props (om/props this)
          {:keys [instances/list]
           {:keys [width height]} :ui/screen} props]
      (dom/div #js{:id "elektronik"}
        (toolbar props)
        (panels props)
        (dom/svg #js{:ref "svg-container"
                     :style (:svg stylesheet)
                     :width "100%"
                     :height "100%"
                     :onDoubleClick #(.on-double-click this %)}
          (map instance list))))))

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

(def parser
  (om/parser
    {:read read
     :mutate mutate}))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser parser
     :normalize true
     :id-key :db/id}))

(om/add-root! reconciler Root (gdom/getElement "app"))

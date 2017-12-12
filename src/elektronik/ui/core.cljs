(ns elektronik.ui.core
  (:require [elektronik.ui.panels.query-inspector :as query-inspector]
            [elektronik.ui.panels.state-inspector :as state-inspector]
            [elektronik.ui.panels.instance-inspector :as instance-inspector]
            [elektronik.utils :as utils]
            [elektronik.specs :as specs]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]
            [goog.style :as gstyle]))

(def stylesheet
  {:instance
   {:rect {:default #js{:fill "gray"
                        :strokeWidth 1
                        :stroke "black"}
           :selected #js{:fill "blue"}}
    :text #js{:pointerEvents "none"}}
   :socket #js{:fill "black"}
   :link #js{:stroke "rgb(0,0,0)"
             :strokeWidth 1}})

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
    (let [factories-list (om/props this)]
      (dom/div #js{:className "toolbar"}
        (map #(toolbar-button this %) factories-list)))))

(def toolbar (om/factory Toolbar))

(defui Socket
  static om/Ident
  (ident [this props]
    (let [{:keys [socket/id socket/type]} props]
      [type id]))
  static om/IQuery
  (query [this]
    [:socket/id :socket/type]))

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
    (let [factory-query (om/get-query Factory)
          socket-query (om/get-query Socket)]
      `[:instance/id :instance/x :instance/y
        {:instance/factory ~(conj factory-query {:factory/sockets socket-query})}
        {[:selection/list 0] [:instance/id]}])) ; TODO this assumes selection/list contains only one entry
  Object
  (render [this]
    (let [{:keys [render-instance render-sockets]} (om/get-computed this)]
      (dom/g nil
        (render-instance this)
        (render-sockets this)))))

(def instance (om/factory Instance {:validator #(specs/default-validator ::specs/instance %)}))

(defui Link
  static om/IQuery
  (query [this]
    (let [link-subquery [{:instance/ident [:instance/x :instance/y]}
                         {:socket/ident [:socket/id :socket/type]}]]
      [{:link/from link-subquery}
       {:link/to link-subquery}]))
  Object
  (render [this]
    (let [{{{from-x :instance/x from-y :instance/y} :instance/ident
            from-socket :socket/ident} :link/from
           {{to-x :instance/x to-y :instance/y} :instance/ident
            to-socket :socket/ident} :link/to} (om/props this)]
      (dom/line #js{:x1 from-x
                    :y1 from-y
                    :x2 to-x
                    :y2 to-y
                    :style (:link stylesheet)}))))

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
    (let [panels-list (om/props this)]
      (dom/div #js{:className "panels"}
        (map panel panels-list)))))

(def panels (om/factory Panels {:validator #(specs/default-validator ::specs/panels %)}))

(defn calculate-socket-positions [sockets]
  (let [sockets-count (count sockets)]
    (if (= 1 sockets-count)
      (list (utils/lerp -5 45 0.5))
      (map (fn [idx]
             (utils/lerp -5 45 (* idx (/ 1 (max 1 (dec sockets-count))))))
           (range sockets-count)))))

(defn render-socket [instance socket x y]
  (let [{instance-id :instance/id} instance
        {:keys [socket/type socket/id]} socket
        style (get-in stylesheet [:socket])]
    (dom/rect #js{:key (str "socket-" instance-id "-" (name type) "-" (name id))
                  :style style
                  :x x
                  :y y
                  :width 10
                  :height 10})))

(defui SVGRenderer
  Object
  (render-instance [_ this]
    (let [{{selected-instance-id :instance/id} [:selection/list 0]
           :keys [instance/id instance/x instance/y]
           {:keys [factory/name factory/desc]} :instance/factory} (om/props this)
           selected? (= (second (om/get-ident this)) selected-instance-id)
           style (get-in stylesheet [:instance :rect :default])
           style (if selected?
                   (utils/js-merge style (get-in stylesheet [:instance :rect :selected]))
                   style)]
      (dom/g #js{:data-instance-id (str id)}
        (dom/rect #js{:style style
                      :x x
                      :y y
                      :width 50
                      :height 50})
        (dom/text #js{:style (get-in stylesheet [:instance :text])
                      :x (+ 25 x)
                      :y (+ 25 y)
                      :stroke "none"
                      :fill "white"
                      :alignmentBaseline "middle"
                      :textAnchor "middle"
                      :fontSize 20}
          name))))
  (render-sockets [_ this]
    (let [{:keys [instance/x instance/y]
           {:keys [factory/sockets]} :instance/factory
           :as instance} (om/props this)
          {inputs :input outputs :output} (group-by :socket/type sockets)
          input-xs (calculate-socket-positions inputs)
          output-xs (calculate-socket-positions outputs)]
      (dom/g nil
        (map #(render-socket instance %1 (+ x %2) (- y 5)) inputs input-xs)
        (map #(render-socket instance %1 (+ x %2) (+ 45 y)) outputs output-xs))))
  (render [this]
    (let [{:keys [instances/list]
           {:keys [width height]} :ui/screen
           :as props} (om/props this)
          pointer-events-processor (om/shared this :pointer-events-processor)]
      (dom/svg #js{:ref "svg-container"
                   :className "svg-container"
                   :style (:svg stylesheet)
                   :width "100%"
                   :height "100%"
                   :onMouseDown #(pointer-events-processor this %)
                   :onMouseMove #(pointer-events-processor this %)
                   :onMouseUp #(pointer-events-processor this %)}
        (map (fn [instance-props]
               (instance (om/computed
                           instance-props
                           {:render-instance (.-render-instance this)
                            :render-sockets (.-render-sockets this)})))
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
    (let [{factories-list :factories/list
           panels-list :panels/list
           :as props} (om/props this)]
      (dom/div #js{:className "elektronik"}
        (toolbar factories-list)
        (panels panels-list)
        (svg-renderer props)))))

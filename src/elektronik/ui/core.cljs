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
  #:panel.id{:query-inspector query-inspector/QueryInspector
             :state-inspector state-inspector/StateInspector
             :instance-inspector instance-inspector/InstanceInspector})

(def panel-id->factory
  (zipmap (keys panel-id->component)
          (map om/factory (vals panel-id->component))))

(def shared
  {:panel-id->component panel-id->component
   :panel-id->factory panel-id->factory
   :utf-collapse-indicator \u25B4
   :utf-expand-indicator \u25BE})

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

(defn calculate-socket-x-position [idx cnt]
  (if (= 1 cnt)
    (utils/lerp -5 45 0.5)
    (utils/lerp -5 45 (* idx (/ 1 (max 1 (dec cnt)))))))

(defn calculate-sockets-x-positions [sockets]
  (let [cnt (count sockets)]
    (map (fn [idx]
           (calculate-socket-x-position idx cnt))
         (range cnt))))

(defn find-socket-position [socket sockets]
  (let [grouped-sockets (group-by :socket/type sockets)
        {inputs :socket.type/input outputs :socket.type/output} grouped-sockets]
     (or (some-> inputs
           (zipmap (calculate-sockets-x-positions inputs))
           (get socket)
           (vector -5))
         (some-> outputs
           (zipmap (calculate-sockets-x-positions outputs))
           (get socket)
           (vector 45)))))

(defui Link
  static om/IQuery
  (query [this]
    (let [link-subquery [{:instance/ident
                          [:instance/x :instance/y
                           {:instance/factory [{:factory/sockets [:socket/id :socket/type]}]}]}
                         {:socket/ident [:socket/id :socket/type]}]]
      [{:link/from link-subquery}
       {:link/to link-subquery}]))
  Object
  (render [this]
    ;; TODO cleanup
    (let [{{{{from-sockets :factory/sockets} :instance/factory
             from-x :instance/x
             from-y :instance/y} :instance/ident
            from-socket :socket/ident} :link/from
           {{{to-sockets :factory/sockets} :instance/factory
             to-x :instance/x
             to-y :instance/y} :instance/ident
            to-socket :socket/ident} :link/to} (om/props this)
          [socket-from-x socket-from-y] (find-socket-position from-socket from-sockets)
          [socket-to-x socket-to-y] (find-socket-position to-socket to-sockets)]
      (dom/line #js{:x1 (+ from-x socket-from-x)
                    :y1 (+ from-y socket-from-y)
                    :x2 (+ to-x socket-to-x)
                    :y2 (+ to-y socket-to-y)
                    :style (:link stylesheet)}))))

(def link (om/factory Link))

(defui Links
  static om/IQuery
  (query [this]
    (let [link-query (om/get-query Link)]
      `[~@link-query]))
  Object
  (render [this]
    (let [links-list (om/props this)]
      (dom/g nil
        (mapv link links-list)))))

(def links (om/factory Links))

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
          {:keys [panel-id->factory utf-collapse-indicator utf-expand-indicator]} (om/shared this)
          panel-subfactory (panel-id->factory id)]
      (dom/div #js{:id id
                   :className "panel"}
        (dom/button #js{:onClick #(om/transact! this `[(panel/toggle {:panel/id ~id})])}
          (if expanded?
            utf-collapse-indicator
            utf-expand-indicator)
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
          {inputs :socket.type/input outputs :socket.type/output} (group-by :socket/type sockets)
          input-xs (calculate-sockets-x-positions inputs)
          output-xs (calculate-sockets-x-positions outputs)]
      (dom/g nil
        (map #(render-socket instance %1 (+ x %2) (- y 5)) inputs input-xs)
        (map #(render-socket instance %1 (+ x %2) (+ 45 y)) outputs output-xs))))
  (render [this]
    (let [{instances-list :instances/list
           links-list :links/list
           {:keys [width height]} :ui/screen} (om/props this)
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
             instances-list)
        (links links-list)))))

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

(ns elektronik.core
  (:require [elektronik.query-inspector :as query-inspector]
            [elektronik.state-inspector :as state-inspector]
            [elektronik.ui :as ui]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]))

(enable-console-print!)

(def addition-component-factory
  #:factory{:type :math/addition
            :fn :+
            :name "+"
            :desc "Addition"})

(def subtraction-component-factory
  #:factory{:type :math/subtraction
            :fn :-
            :name "-"
            :desc "Subtraction"})

(def app-state
  (let [instance-1-id (om/tempid)
        instance-2-id (om/tempid)
        instance-3-id (om/tempid)]
    {:ui/screen {:width "100%"
                 :height "100%"}
     :panels/list [{:panel/id :query-inspector
                    :panel/name "Query Inspector"
                    :panel/expanded? true}
                   {:panel/id :state-inspector
                    :panel/name "State Inspector"
                    :panel/expanded? true}]
     :selection/list []
     :instances/list [{:db/id instance-1-id
                       :instance/factory addition-component-factory
                       :instance/x 10
                       :instance/y 10}
                      {:db/id instance-2-id
                       :instance/factory subtraction-component-factory
                       :instance/x 110
                       :instance/y 50}
                      {:db/id instance-3-id
                       :instance/factory subtraction-component-factory
                       :instance/x 210
                       :instance/y 10}]
     :links/list [{:from [:instances/by-id instance-1-id]
                   :to [:instances/by-id instance-2-id]}
                  {:from [:instances/by-id instance-3-id]
                   :to [:instances/by-id instance-2-id]}]}))

(defmulti read om/dispatch)
(defmulti mutate om/dispatch)

(defmethod read :factories/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (vals (get st :factories/by-type))}))

(defmethod read :instances/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :selection/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :panels/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :links/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :default [{:keys [state]} k params]
  (let [st @state]
    (if-let [[_ v] (find st k)]
      {:value v}
      {:value :not-found})))

(defmethod mutate 'selection/clear [{:keys [state]} _ _]
  {:value {:keys [:selection/list]}
   :action #(swap! state update :selection/list empty)})

(defmethod mutate 'selection/add-instance [{:keys [state]} _ {:keys [db/id]}]
  (let [ident [:instances/by-id id]]
    {:value {:keys [:selection/list]}
     :action #(swap! state update :selection/list conj ident)}))

;(defmethod mutate 'selection/drag [{:keys [state parser]} _ {:keys [x y]}]
;  (println (parser {:state state} '[{:selection/list[:db/id]}])))

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
    {:value {:keys [:instances/list]}
     :action (fn []
               (swap! state assoc-in instance-ident new-instance)
               (swap! state update :instances/list conj instance-ident))}))

(defmethod mutate 'panel/toggle [{:keys [state]} _ {:keys [panel/id]}]
  (let [ident [:panels/by-id id]]
    {:value {:keys [:panels/list]}
     :action #(swap! state update-in (conj ident :panel/expanded?) not)}))

;; work in progress, quick and dirty
(defn collect-instances [links]
  (mapcat (fn [link]
            (let [{[_ from-id] :from
                   [_ to-id] :to} link]
              [from-id to-id]))
          links))

(defn collect-singletons [dependency-graph]
  (keep (fn [e]
          (let [id (key e)
                deps (:deps (val e))]
            (when-not (seq deps)
              id)))
        dependency-graph))

(defn create-dependency-graph [links]
  (let [empty-graph (into {}
                      (map (fn [instance]
                             [instance {}])
                           (collect-instances links)))]
    (reduce (fn [graph link]
              (let [{[_ from-id] :from
                     [_ to-id] :to} link]
                (update-in graph [to-id :deps] conj from-id)))
            empty-graph
            links)))

(defn transpile-static [state]
  (let [{links :links/list} state
        dependency-graph (create-dependency-graph links)
        singletons (collect-singletons dependency-graph)
        fns (into {}
              (map (fn [singleton]
                     (let [factory-ident (get-in state [:instances/by-id singleton :instance/factory])
                           factory-fn (get-in state (conj factory-ident :factory/fn))]
                       [singleton factory-fn]))
                   singletons))]
    (into {}
      (keep (fn [e]
              (let [id (key e)
                    deps (:deps (val e))]
                (when (seq deps)
                  [id (replace fns deps)])))
            dependency-graph))))

;(transpile-static @(om/app-state reconciler))

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

(om/add-root! reconciler ui/Root (gdom/getElement "app"))

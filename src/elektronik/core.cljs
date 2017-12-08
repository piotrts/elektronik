(ns elektronik.core
  (:require [elektronik.ui :as ui]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]))

(enable-console-print!)

(reader/register-tag-parser! 'om/id #(-> % first uuid om/tempid))

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
                    :panel/expanded? true}
                   {:panel/id :instance-inspector
                    :panel/name "Instance Inspector"
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

(defmethod read :panels/data [{:keys [query ast state parser]} k _]
  (let [st @state]
    {:value (reduce-kv (fn [acc k v]
                         (assoc acc k (parser {:state state} v)))
                       {}
                       query)}))

(defmethod read :links/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmethod read :default [{:keys [query state]} k params]
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

(def parser
  (om/parser
    {:read read
     :mutate mutate}))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser parser
     :normalize true
     :shared ui/shared
     :id-key :db/id}))

(om/add-root! reconciler ui/Root (gdom/getElement "app"))

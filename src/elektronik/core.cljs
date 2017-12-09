(ns elektronik.core
  (:require [elektronik.ui :as ui]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]))

(enable-console-print!)

(reader/register-tag-parser! 'om/id #(-> % first uuid om/tempid))

(def addition-component-factory
  #:factory{:id :math/addition
            :name "+"
            :desc "Addition"})

(def subtraction-component-factory
  #:factory{:id :math/subtraction
            :name "-"
            :desc "Subtraction"})

(def app-state
  (let [instance-1-id (om/tempid)
        instance-2-id (om/tempid)
        instance-3-id (om/tempid)]
    {:ui/screen {:width "100%"
                 :height "100%"}
     :panels/list [#:panel{:id :query-inspector
                           :name "Query Inspector"
                           :expanded? true}
                   #:panel{:id :state-inspector
                           :name "State Inspector"
                           :expanded? true}
                   #:panel{:id :instance-inspector
                           :name "Instance Inspector"
                           :expanded? true}]
     :selection/list []
     :instances/list [#:instance{:id instance-1-id
                                 :factory addition-component-factory
                                 :x 10
                                 :y 10}
                      #:instance{:id instance-2-id
                                 :factory subtraction-component-factory
                                 :x 110
                                 :y 50}
                      #:instance{:id instance-3-id
                                 :factory subtraction-component-factory
                                 :x 210
                                 :y 10}]
     :links/list [#:link{:from [:instances/by-id instance-1-id]
                         :to [:instances/by-id instance-2-id]}
                  #:link{:from [:instances/by-id instance-3-id]
                         :to [:instances/by-id instance-2-id]}]}))

(defmulti read om/dispatch)

(defmulti mutate om/dispatch)

(defmethod read :factories/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (vals (get st :factories/by-id))}))

(defmethod read :default [{:keys [query ast state]} k params]
  (let [st @state]
    {:value (let [getter (if (om.util/ident? (:key ast))
                           get-in
                           get)]
              (om/db->tree query (getter st k) st))}))

(defmethod mutate 'selection/clear [{:keys [state]} _ _]
  {:value {:keys [:selection/list]}
   :action #(swap! state update :selection/list empty)})

(defmethod mutate 'selection/add-instance [{:keys [state]} _ {:keys [instance/id]}]
  (let [ident [:instances/by-id id]]
    {:value {:keys [:selection/list]}
     :action #(swap! state update :selection/list conj ident)}))

;(defmethod mutate 'selection/drag [{:keys [state parser]} _ {:keys [x y]}]
;  (println (parser {:state state} '[{:selection/list[:db/id]}])))

(defmethod mutate 'instance/create [{:keys [query state]} _ {:keys [factory/id instance/x instance/y]}]
  (let [x (or x (rand-int 500))
        y (or y (rand-int 500))
        tempid (om/tempid)
        instance-ident [:instances/by-id tempid]
        factory-ident [:factories/by-id id]
        new-instance #:instance{:id tempid
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

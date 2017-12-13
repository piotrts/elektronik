(ns elektronik.core
  (:require [elektronik.pointer-events-processor :as pointer-events-processor]
            [elektronik.ui.core :as ui]
            [cljs.reader :as reader]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]))

(enable-console-print!)

(reader/register-tag-parser! 'om/id #(-> % first uuid om/tempid))

(def addition-component-factory
  #:factory{:id :math/addition
            :name "+"
            :desc "Addition"
            :sockets [#:socket{:id :socket.id/math.addition.x
                               :type :socket.type/input}
                      #:socket{:id :socket.id/math.addition.y
                               :type :socket.type/input}
                      #:socket{:id :socket.id/math.addition.result
                               :type :socket.type/output}]})

(def subtraction-component-factory
  #:factory{:id :math/subtraction
            :name "-"
            :desc "Subtraction"
            :sockets [#:socket{:id :socket.id/math.subtraction.x
                               :type :socket.type/input}
                      #:socket{:id :socket.id/math.subtraction.y
                               :type :socket.type/input}
                      #:socket{:id :socket.id/math.subtraction.result
                               :type :socket.type/output}]})

(def number-component-factory
  #:factory{:id :type/number
            :name "1" ;; TODO temporary, make this editable
            :desc "A number"
            :sockets [#:socket{:id :socket.id/type.number.value
                               :type :socket.type/output}]})

(def app-state
  (let [instance-1-id (om/tempid)
        instance-2-id (om/tempid)
        instance-3-id (om/tempid)]
    {:ui/screen {:width "100%"
                 :height "100%"}
     :panels/list [#:panel{:id :panel.id/query-inspector
                           :name "Query Inspector"
                           :expanded? true}
                   #:panel{:id :panel.id/state-inspector
                           :name "State Inspector"
                           :expanded? true}
                   #:panel{:id :panel.id/instance-inspector
                           :name "Instance Inspector"
                           :expanded? true}]
     :selection/list []
     :instances/list [#:instance{:id instance-1-id
                                 :factory number-component-factory
                                 :x 10
                                 :y 10}
                      #:instance{:id instance-2-id
                                 :factory subtraction-component-factory
                                 :x 110
                                 :y 50}
                      #:instance{:id instance-3-id
                                 :factory number-component-factory
                                 :x 210
                                 :y 10}]
     :links/list [#:link{:from {:instance/ident [:instances/by-id instance-1-id]
                                :socket/ident [:socket.type/output :socket.id/type.number.value]}
                         :to {:instance/ident [:instances/by-id instance-2-id]
                              :socket/ident [:socket.type/input :socket.id/math.subtraction.x]}}
                  #:link{:from {:instance/ident [:instances/by-id instance-3-id]
                                :socket/ident [:socket.type/output :socket.id/type.number.value]}
                         :to {:instance/ident [:instances/by-id instance-2-id]
                              :socket/ident [:socket.type/input :socket.id/math.subtraction.y]}}]}))

(defmulti read om/dispatch)

(defmulti mutate om/dispatch)

(defmethod read :factories/list [{:keys [query state]} k _]
  (let [st @state]
    {:value (vals (get st :factories/by-id))}))

(defmethod read :default [{:keys [query ast state]} k params]
  (let [st @state
        data (if (om.util/ident? (:key ast))
               (get-in st (:key ast))
               (get st k))]
    {:value (om/db->tree query data st)}))

(defmethod mutate 'selection/clear [{:keys [state]} _ _]
  {:value {:keys [:selection/list]}
   :action #(swap! state update :selection/list empty)})

(defmethod mutate 'selection/add-instance [{:keys [state]} _ {:keys [instance/id]}]
  (let [ident [:instances/by-id id]]
    {:value {:keys [:selection/list]}
     :action #(swap! state update :selection/list conj ident)}))

(defmethod mutate 'selection/drag [{:keys [state parser]} _ {:keys [x y]}]
  (let [selection-list (:selection/list @state)]
    {:action (fn []
               (run! (fn [selection-ident]
                       (swap! state update-in selection-ident assoc :instance/x x :instance/y y))
                     selection-list))}))

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
     :shared (merge
               {:pointer-events-processor (pointer-events-processor/make-pointer-events-processor)}
               ui/shared)
     :id-key :db/id}))

(om/add-root! reconciler ui/Root (gdom/getElement "app"))

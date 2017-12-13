(ns elektronik.transpiler)

;; work in progress, quick and dirty
(defn collect-instances [links]
  (mapcat (fn [link]
            (let [{{[_ from-id] :instance/ident}  :link/from
                   {[_ to-id] :instance/ident} :link/to} link]
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
              (let [{{[_ from-id] :instance/ident}  :link/from
                     {[_ to-id] :instance/ident} :link/to} link]
                (update-in graph [to-id :deps] (fnil conj []) from-id)))
            empty-graph
            links)))

(defn state->ast [state]
  (let [{links :links/list} state
        dependency-graph (create-dependency-graph links)
        singletons (collect-singletons dependency-graph)
        fns (into {}
              (map (fn [singleton]
                     (let [factory-ident (get-in state [:instances/by-id singleton :instance/factory])
                           factory-fn (get-in state (conj factory-ident :factory/id))]
                       [singleton factory-fn]))
                   singletons))]
    (into {}
      (keep (fn [e]
              (let [id (key e)
                    deps (:deps (val e))]
                (when (seq deps)
                  {:factory/id id
                   :fn/args (replace fns deps)})))
            dependency-graph))))

;(state->ast @(om.next/app-state elektronik.core/reconciler))

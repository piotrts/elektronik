(ns elektronik.transpiler)

;; work in progress, quick and dirty
(defn collect-instances [links]
  (mapcat (fn [link]
            (let [{[_ from-id] :link/from
                   [_ to-id] :link/to} link]
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
              (let [{[_ from-id] :link/from
                     [_ to-id] :link/to} link]
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
                           factory-fn (get-in state (conj factory-ident :factory/id))]
                       [singleton factory-fn]))
                   singletons))]
    (into {}
      (keep (fn [e]
              (let [id (key e)
                    deps (:deps (val e))]
                (when (seq deps)
                  [id (replace fns deps)])))
            dependency-graph))))

(transpile-static @(om/app-state reconciler))

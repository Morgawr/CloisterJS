(ns cloisterjs.systems
  (:require [cloisterjs.utils]
            [cloisterjs.image :as img])
  ;(:require-macros [cloisterjs.macros :as macros])
)


;(defn renderme [state depth [id pos]]
;  (img/prepare-render (:img (id (:sprite (:containers state))))
;                      [(:x pos) (:y pos)]
;                      depth)
;  nil
;)
;
;(defn ruletest [state [_ _]]
;  true
;)
;
;(macros/defsystem renderer
;  [:position]
;  [ruletest]
;  renderme
;)

;(defn get-containers
;  "Given a set of keys and the whole collection of components, returns a 
;  cartesian
;

(defn get-containers
  "Given a set of keys and the whole collection of components, returns a 
  cartesian product of all possible combinations of the given keys taken from
  the given collection"
  [names all]
  (doall
    (let [stuff (map #(% (select-keys all names)) names)]
      (apply cloisterjs.utils/cartesian-product stuff)
    )
  )
)

(defn filter-components
  "Given the pairs of components, filter them following a proper ruleset."
  [rules state comps]
  (doall
    (filter (fn [c] (every? #(apply % state c) rules)) comps)
  )
)

(defn remap-containers
  "Given the list of containers, remap it with the new containers, removing 
  deleted components"
  [map1 map2]
  (apply dissoc (merge map1 map2) (map first (remove second map2)))
)

(defn reflow-state
  "Merge all the different temporary containers and put them back into the
  state"
  [state in]
  (if (empty? in)
    state
    (assoc state :containers 
           (reduce (partial merge-with #(remap-containers %1 %2))
                   (:containers state) in))
  )
)

(defn map-unlazy
  "Map all the components to their specific handler with side-effects in a 
  non-lazy way"
  [h state depth comps]
  (doall
    (map (partial apply h state depth) comps)
  )
)

(defn set-handler 
  "Sets a new handler for the given system, useful for binding closures to 
  the handler at runtime."
  [system handler]
  (assoc system :handler handler)
)

(defn run-system 
  "Executes a system on the given gamestate"
  [sys depth state]
  (doall
    ((:run sys) state (:rules sys) (:handler sys) depth)
  )
)


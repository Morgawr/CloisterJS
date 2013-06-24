(ns cloisterjs.systems
  (:require [cloisterjs.utils]
            [cloisterjs.image :as img])
)

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
  "Filter the current group of components given a proper ruleset."
  [rules state comps]
  (if (every? #(apply % state comps) rules)
    comps
    nil
  )
)

(defn apply-on-component
  "Given a handler, the state, the depth and the group of components, check if 
  the group is not nil (aka it passed filtering successfully) and calls the 
  handler on that. If it's nil, it returns an empty list."
  [handler state depth cgroup]
  (if-not (nil? cgroup)
    (apply handler state depth cgroup)
    []
  )
)

(defn re-assign-components 
  "Given the state, a list of IDs and a list of component names, return the 
  proper pairs of ID + component for each of them."
  [state cnames ids]
  (let [containers (:containers state)]
    (reverse (map (fn [c] [(second c) ((first c) (second c))]) 
                  (zipmap (map containers cnames) ids)))
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

; 1) take only the IDs from the get-containers result
; 2) map those IDs with their respective component from the current state
; 3) check if that group of components meet the validations from the ruleset
; 4) apply handler
; 5) merge back into state
; 6) GOTO 1 until finished
(defn operate-on-components 
  [rules state depth handler compnames comps]
  (let [idlist (map (partial map first) comps) ; step 1
        change-state 
        (fn [ids state]
          (->> ids 
               (re-assign-components state compnames) ; step 2
               (filter-components rules state) ; step 3
               (apply-on-component handler state depth) ; step 4
               (#(reflow-state state [%])) ; step 5
          )
        )]
    (loop [ids idlist s state]
      (if-not (empty? ids)
        (recur (rest ids) (change-state (first ids) s)) ; step 6
        s
      )
    )
  )
)

(defn set-handler 
  "Sets a new handler for the given system, useful for binding closures to 
  the handler at runtime."
  [system handler]
  (assoc system :handler handler)
)

(defn mini-system-filter 
  [handler state depth message msglist]
  (if (not (nil? (message msglist)))
    (handler state depth)
    nil
  )
)

(defn run-system 
  "Executes a system on the given gamestate"
  [sys depth state]
  (doall
    (if-not (nil? (:rules sys))
      ((:run sys) state (:rules sys) (:handler sys) depth)
      ((:run sys) state (:handler sys) depth)
    )
  )
)


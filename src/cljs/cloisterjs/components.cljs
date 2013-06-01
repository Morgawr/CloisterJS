(ns cloisterjs.components
  (:require [clojure.walk])
  (:require-macros [cloisterjs.macros :refer [defcomponent map-v]])
)

; This file contains definitions for the more generic components and small
; utility functions to operate with them.

; A simple collection of coordinates for a point in space
(defcomponent position [x y] [[x x]
                              [y y]])

; Name of an entity expressed inside the component, should only be called from 
; inside the init-entity function
(defcomponent ename [n] [[name n]])

; DO NOT USE DIRECTLY
(def id_counter nil)

(defn gen-id []
  "Custom implementation of gensym but with a number and not a symbol"
  (when (nil? id_counter)
    (set! id_counter (atom 0)))
  (swap! id_counter inc)
)

(defn swap-around [[id comps]]
  "Turn an entity from a vector of components with an id into a map of named 
  components paired with an id"
  (->> comps
      (map (fn [el] [(keyword (:cname el)) el]))
      (into {})
      (map-v (fn [n] [id n]))
  )
)

(defn init-containers [entities]
  "Given a list of entities, extracts the components with the same type and 
  create a dictionary with those components grouped together"
  (let [names (set (map #(keyword (:cname %)) (filter map? (flatten entities))))
        containers (zipmap names (take (count entities) (repeat [])))]
    (->> entities
        (map swap-around)
        (reduce #(merge-with merge %1 %2) containers)
    )
  )
)


(defn init-entity [name components]
  "Return a new group of components bound together by a unique ID. Components 
  must be a collection."
  (when (not (coll? components))
    (throw (js/Error. "An entity requires a collection of components")))
  [(gen-id) (conj components (ename name))]
)


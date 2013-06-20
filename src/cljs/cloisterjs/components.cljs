(ns cloisterjs.components
  (:require [clojure.walk]
            [cloisterjs.uniqueid :refer [id_counter]])
  (:require-macros [cloisterjs.macros :refer [defcomponent map-v]])
)

; This file contains definitions for the more generic components and small
; utility functions to operate with them.

; A simple collection of coordinates for a point in space
(defcomponent position [x y] [[x x]
                              [y y]])

(defcomponent sprite [img] [[image img]])

; Name of an entity expressed inside the component, should only be called from 
; inside the init-entity function
(defcomponent ename [n] [[name n]])

(defn gen-id 
  "Custom implementation of gensym but with a number and not a symbol"
  []
  (when (nil? id_counter)
    (set! id_counter (atom 0)))
  (swap! id_counter inc)
)


; swap-around example:
; [1 { :cname "test" :x 3 :y 5} { :cname "test2" :a 34 :b 53 } ]
;                            || 
;                            \/
; { 
;   :test  { 1 { :cname "test" :x 3 :y 5} } 
;   :test2 { 1 { :cname "test2" :a 34 :b 53 } }
; }

(defn swap-around 
  "Turn an entity from a vector of components with an id into a map of named 
  components paired with an id"
  [[id comps]]
  (->> comps
      (map (fn [el] [(keyword (:cname el)) el]))
      (into {})
      (map-v (fn [n] {id n}))
  )
)

; init-containers example:
; [
;   [ 1 { :cname "test" :x 3 :y 5} { :cname "test2" :a 34 :b 53 } ]
;   [ 2 { :cname "test2" :a 4 :b 6} { :cname "test3" :c 4 :d 4 } ]
; ]
;                            ||
;                            \/
; {
;   :test  { 
;            1 { :cname "test" :x 3 :y 5 }
;          }
;
;   :test2 {
;            1 { :cname "test2" :a 34 :b 53 }
;            2 { :cname "test2" :a 4 :b 6 }
;          }
;
;   :test3
;          { 
;            2 { :cname "test3" :c 4 :d 4 }
;          }
; }

(defn init-containers 
  "Given a list of entities, extracts the components with the same type and 
  create a dictionary with those components grouped together."
  [entities]
  (let [names (set (map #(keyword (:cname %)) (filter map? (flatten entities))))
        containers (zipmap names (take (count entities) (repeat {})))]
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

(defn add-component [[eid others] component]
  "Add a component to the specific entity, this only works before the entity
  is actually added to the container list"
  [eid (conj others component)]
)



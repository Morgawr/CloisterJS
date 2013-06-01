(ns cloisterjs.components
  (:require-macros [cloisterjs.macros :refer [defcomponent]])
)

; This file contains definitions for the more generic components and small
; utility functions to operate with them.


; A simple collection of coordinates for a point in space
(defcomponent position [x y] [[x x]
                              [y y]])


; DO NOT USE DIRECTLY
(def id_counter nil)

(defn gen-id []
  "Custom implementation of gensym but with a number and not a symbol"
  (when (nil? id_counter)
    (set! id_counter (atom 0)))
  (swap! id_counter inc)
)

(defn init-entity [components]
  "Return a new group of components bound together by a unique ID. Components 
  must be a collection."
  (when (not (coll? components))
    (throw (js/Error. "An entity requires a collection of components")))
  [(gen-id) components]
)


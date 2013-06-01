(ns cloisterjs.macros
  ;(:require [clojure.walk :refer [walk]])
)

(defmacro map-v [func m]
  "Execute a function on the values of the map, returning a new map with the 
  same keys but updated values"
  ;`(zipmap (keys ~m) (map ~func (vals ~m))) <- old implementation
  `(clojure.walk/walk (fn [[k# v#]] [k# (~func v#)]) identity ~m)
)

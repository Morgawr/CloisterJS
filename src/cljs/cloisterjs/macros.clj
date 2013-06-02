(ns cloisterjs.macros)

(defmacro map-v 
  "Execute a function on the values of the map, returning a new map with the 
  same keys but updated values"
  [func m]
  ;`(zipmap (keys ~m) (map ~func (vals ~m))) <- old implementation
  `(clojure.walk/walk (fn [[k# v#]] [k# (~func v#)]) identity ~m)
)

(defmacro defcomponent 
  "Define a new component, this is a wrapping around defrecord and an automated
  call to its constructor with the appropritely bound parameters"
  [name params initmap]
  (let [tname (gensym (str name))
        nname (str name)
        tctor (symbol (str (clojure.core/name tname) "."))]
   `(do
     (defrecord ~tname ~(conj (map first initmap) (symbol "cname")))
     (defn ~name ~params (~tctor (str ~nname) ~@(map second initmap)))
    )
  )
)

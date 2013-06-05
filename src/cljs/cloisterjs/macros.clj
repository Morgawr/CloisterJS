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

(defmacro get-containers
  "Given a set of keys and the whole collection of components, returns a 
  cartesian product of all possible combinations of the given keys taken from
  the given collection"
  [names all]
  `(apply cloisterjs.utils/cartesian-product
         (map #(% (select-keys ~all ~names)) ~names))
)

(defmacro filter-components
  "Given the pairs of components, filter them following a proper ruleset."
  [rules comps]
  `(filter (fn [c#] (every? #(apply % c#) ~rules)) ~comps)
)

(defmacro remap-containers
  "Given the list of containers, remap it with the new containers, removing 
  deleted components"
  [map1 map2]
  `(apply dissoc (merge ~map1 ~map2) (map first (remove second ~map2)))
)

(defmacro reflow-state
  "Merge all the different temporary containers and put them back into the
  state"
  [state in]
  `(if (empty? ~in)
     ~state
     (reduce (partial merge-with remap-containers ~state) ~in)
  )
)

(defmacro defsystem
  "Define a new system, this is a wrapping around defrecord and an automated 
  call to its constructor. It creates a new system specification with the
  proper name and parameters which consist of components (input interface), 
  set of rules for validating said components and a body to manipulate each 
  interaction."
  [name components ruleset handler]
  (let [tname (gensym (str name))
        nname (str name)
        tctor (symbol (str (clojure.core/name tname) "."))]
   `(do
      (defrecord ~tname [~'actors ~'rules ~'handler ~'run])
      (defn ~name []
        (~tctor 
          ~components 
          ~ruleset
          ~handler
          (fn [state# r#]
            (doall
              (->> state#
                   :containers ; extract containers
                   (get-containers ~components) ; retrieve only the ones we need
                   (filter-components r#) ; only those who meet the validation
                   (#(map (partial apply ~handler) %))
                   ;(reflow-state state#)
              )
            )
          )
        )
      )
    )
  )
)

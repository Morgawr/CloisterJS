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
          (fn [state# r# h# depth#]
            (->> state#
                 ; extract containers
                 (:containers)
                 ; retrieve only the ones we need
                 (cloisterjs.systems/get-containers ~components)
                 (cloisterjs.systems/operate-on-components r# state# depth#
                                                           h# ~components)
                 ; only those who meet the validation
                 ;(cloisterjs.systems/filter-components r# state#)
                 ;(cloisterjs.systems/map-unlazy h# state# depth#)
                 ;(cloisterjs.systems/reflow-state state#)
            )
          )
        )
      )
    )
  )
)

(defmacro defminisystem
  "Define a new mini-system, a mini-system is a system that doesn't actively 
  act on entities/components, it just listens to events/messages and responds
  to those events. This macro acts like defsystem but with a simplified 
  approach."
  [name message handler]
  (let [tname (gensym (str name))
        nname (str name)
        tctor (symbol (str (clojure.core/name tname) "."))]
    `(do
       (defrecord ~tname [~'message ~'handler ~'run])
       (defn ~name []
         (~tctor
           ~message
           ~handler
           (fn [state# h# depth#]
             (->> state#
                  (:messages)
                  (cloisterjs.systems/mini-system-filter h# state# depth#
                                                         ~message)
                  (cloisterjs.systems/reflow-state state#)
            )
          )
        )
      )
    )
  )
)


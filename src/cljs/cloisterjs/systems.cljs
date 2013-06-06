(ns cloisterjs.systems
  (:require [cloisterjs.utils]
            [cloisterjs.image :as img])
  (:require-macros [cloisterjs.macros :as macros])
)


(defn renderme [[_ pos] [_ sp]]
  (img/prepare-render (:img sp)
                      [(:x pos) (:y pos)]
                      0)
  nil
)

(defn ruletest [[id1 _] [id2 _]]
  (= id1 id2)
)

(macros/defsystem renderer
  [:position :sprite]
  [ruletest]
  renderme
)

(defn set-handler 
  "Sets a new handler for the given system, useful for binding closures to 
  the handler at runtime."
  [system handler]
  (assoc system :handler handler)
)

(defn run-system 
  "Executes a system on the given gamestate"
  [state sys]
  ((:run sys) state (:rules sys) (:handler sys))
)







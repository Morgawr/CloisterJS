(ns cloisterjs.systems
  (:require [cloisterjs.utils]
            [cloisterjs.image :as img])
  (:require-macros [cloisterjs.macros :as macros])
)


(defn renderme [state [id pos]]
  (img/prepare-render (:img (id (:sprite (:containers state))))
                      [(:x pos) (:y pos)]
                      0)
  nil
)

(defn ruletest [state [_ _]]
  true
)

(macros/defsystem renderer
  [:position]
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


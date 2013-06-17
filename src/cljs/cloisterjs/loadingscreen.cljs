(ns cloisterjs.loadingscreen
  (:require [cloisterjs.scheduler :as sched]
            [cloisterjs.components :as comps]
            [cloisterjs.input :refer [mouse-state]]
            [cloisterjs.image :refer [load-images
                                      prepare-render]]
            [cloisterjs.systems :refer [run-system]])
  (:require-macros [cloisterjs.macros :refer [defcomponent
                                              defsystem]])
)


; Example of a simple loading screen

(defcomponent image-group [images] [[images images]])
(defcomponent loaded-images [func] [[loaded func]])
(defcomponent mouse-listenable [] [])

(defsystem mouse-listener
  [:mouse-listenable]
  []
  (fn [state depth _]
    (let [state (mouse-state)
          lstate (:lbutton state)]
      (when (:drag-ended lstate)
        (.log js/console "Drag ended!"))
      nil
    )
  )
)

(defsystem loader 
  [:image-group]
  []
  ; Load the images and create new component to check them out
  (fn [state depth [id img]] 
    (let [f (load-images (:images img))
          loaded (loaded-images f)]
      { :image-group { id nil } :loaded-images { id loaded } }
    )
  )
)

(defn finished-loading? [state [id loaded]]
  (= ((:loaded loaded)) 100)
)

(defsystem checker
  [:loaded-images]
  [finished-loading?]
  ; Print to screen after it finishes loading
  (fn [state depth [id loaded]]
    (sched/destroy-entity! id) ; Kill the entity after we're done
    (sched/add-entity! (comps/init-entity "image"
                                          [(comps/position 100 100)
                                           (comps/sprite :sun)]))
    { :loaded-images { id nil } }
  )
)

(defsystem renderer
  [:sprite]
  []
  (fn [state depth [id sprite]]
    (let [position ((:position (:containers state)) id)]
      (when (not (nil? position))
        (prepare-render (:image sprite) [(:x position) (:y position)] depth))
      {}
    )
  )
)

; TODO - we ignore the audio atm
(defn init-screen 
  "Init function, just initialize basic entities and systems and start loading
  them"
  [screen state images audio]
  (sched/add-entity! (comps/init-entity "loader" 
                                        [(image-group images)]))
  (sched/add-entity! (comps/init-entity "listener"
                                        [(mouse-listenable)]))
  (assoc screen 
         :loader (loader)
         :checker (checker)
         :renderer (renderer)
         :mouse (mouse-listener))
)

(defn fini-screen
  "Fini function, just remove the loadables from the entities."
  [screen state]
  (let [checkers (:loaded-images (:containers state))
        toremove (keys checkers)]
    (doall
      (map sched/destroy-entity! toremove)
    )
  )
)

(defn handle-screen 
  "Handler of this specific screen"
  [screen state depth]
  (->> state
       (run-system (:mouse screen) depth)
       (run-system (:loader screen) depth)
       (run-system (:checker screen) depth)
       (run-system (:renderer screen) depth)
  )
)

(defn LoadingScreen 
  []
  (sched/CloisterScreen. false init-screen handle-screen fini-screen)
)


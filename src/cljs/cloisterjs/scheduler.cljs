(ns cloisterjs.scheduler
  (:require [cloisterjs.image :refer [do-render fill-clear flush-pipeline]]
            [cloisterjs.components :as comps]
            [goog.dom :as dom])
)

; This file contains all the screen scheduling functions. It takes the game
; state, loops it for each screen and passes the list of components to each
; screen's systems. It takes care of swapping screens, removing/adding them and
; timing the frame succession.


; Global atom defining IDs of entities that have to be removed every frame
(def _entity-recycler (atom []))

; Global atom defining entities that have to be added to the game state every
; frame
(def _entity-spawner (atom []))

(defrecord CloisterState [time ; How much time since last frame
                          containers ; Dictionary of lists of common components
                          screens ; List of screens in the gamestate
                          hooks ; TODO - these should be hooks for the REPL
                          ])

(defn get-animation-method 
  "This function checks for the suitable frame scheduling method depending on 
  the specific browser being used."
  []
  (let [window (dom/getWindow)
        methods ["requestAnimationFrame"
                 "webkitRequestAnimationFrame"
                 "mozRequestAnimationFrame"
                 "oRequestAnimationFrame"
                 "msRequestAnimationFrame"]
        options (map (fn [name] #(aget window name)) methods)]
    ((fn [[current & remaining]]
       (cond
         (nil? current) #((.-setTimeout window) % (/ 1000 30))
         (fn? (current)) (current)
         :else (recur remaining)
       )) options
    )
  )
)

(defn get-reschedule-frame 
  "This function defines a high-performance optimization method for better 
  framerate in browsers."
  []
  (let [window (dom/getWindow)
        nextFrame (atom [])
        messagename "zero-timeout"
        zeroTimeout (fn [func]
                      (swap! nextFrame conj func)
                      (.postMessage window messagename "*")
                    )
        handle-msg (fn [e]
                     (let [source (.-source e)
                           data (.-data e)]
                       (if (and (= source window) (= data messagename))
                         (do
                           (.stopPropagation e)
                           (when (> (count @nextFrame) 0)
                             (let [n (first @nextFrame)]
                               (swap! nextFrame rest)
                               (n)
                             ))
                         )
                         nil
                       )
                     ))]
    (.addEventListener window "message" handle-msg true)
    zeroTimeout
  )
)

(def anim-method (get-animation-method))

(defn start-render 
  "This function is called to set up and start the rendering engine, which will 
  execute endlessly"
  [canvas]
  (let [ctx (.getContext canvas "2d")]
    (anim-method (fn rendering []
                   (fill-clear ctx [(.-width canvas) (.-height canvas)] "white")
                   (do-render ctx)
                   (anim-method rendering)
                 ))
  )
)

(defn remove-entities 
  "Function called every frame iteration, at the end of the frame. It removes
  all the enities scheduled for removal by _entity-recycler. Has the side 
  effect of emptying _entity-recycler."
  [state]
  (let [entities @_entity-recycler
        containers (:containers state)]
    (reset! _entity-recycler [])
    (if (empty? entities)
      state
      (assoc state :containers (zipmap (keys containers)
                                       (map #(apply dissoc 
                                                    (second %) 
                                                    entities) containers)))
    )
  )
)

(defn add-entities
  "Function called at the beginning of every frame iteration. It retrieves all
  the newly added entities from _entity-spawner and splits their components
  into the proper containers. Has the side effect of emptying _entity-spawner."
  [state]
  (let [entities @_entity-spawner
        containers (:containers state)]
    (reset! _entity-spawner [])
    (->> entities
         (comps/init-containers)
         (merge-with merge containers)
         (assoc state :containers)
    )
  )
)

(defn start 
  "Given a list of entities and a starting screen, initialize the gamestate and
  start the main loop."
  [entities screen]
  ; TODO - add the proper code for screen handling
  (let [state (CloisterState 0 (comps/init-containers entities) nil nil)]
    (anim-method #(do-update state))
  )
)


(defn do-update
  "This function is called every update frame, it handles the basic CloisterJS
  functions for screen scheduling, entity recycling and components filtering. 
  It manages REPL hooks and handles frame succession."
  [state]
  (flush-pipeline) ; clear screen 
  (->> state
       (remove-entities) ; clear old entities
       ; TODO - register time since last update
       ; TODO - REPL hooks go here
       ; TODO - update screen stuff goes here
       (add-entities) ; add newly spawned entities
       ((fn [s] (anim-method #(do-update s)))) ; schedule next frame
  )
)


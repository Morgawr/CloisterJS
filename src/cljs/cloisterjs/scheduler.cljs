(ns cloisterjs.scheduler
  (:require [cloisterjs.image :refer [do-render fill-clear flush-pipeline]]
            [cloisterjs.components :as comps]
            [cloisterjs.input :as input]
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

; Global atom defining screens to remove
(def _screen-recycler (atom []))

; Global atom defining screens to add on top, it contains pairs of screen +
; parameters to pass to the screen's init function
(def _screen-spawner (atom []))


(defrecord CloisterState [dtime ; How much time since last frame
                          time ; Number of milliseconds of the last update
                          containers ; Dictionary of lists of common components
                          screens ; List of screens in the gamestate
                          hooks ; TODO - these should be hooks for the REPL
                          ])

(defrecord CloisterScreen [popup ; if it's a popup screen or not
                           init ; function used to initialize the screen
                           handler ; function used to call the update
                           fini ; function used to free screen resources
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


(defn remove-screens
  "Function called every frame iteration, at the end of teh frame. It removes
  all the screens scheduled for removal by _screen-recycler. Has the side 
  effect of emptying _screen-recycler."
  [state]
  (let [toremove @_screen-recycler
        screens (:screens state)]
    (reset! _screen-recycler [])
    (if (empty? toremove)
      state
      (doall
        (map #((:fini %) % state) toremove)
        (assoc state :screens (filterv (complement (set toremove)) screens))
      )
    )
  )
)

(defn create-screen
  "Given an individual screen and state, initialize it and return it to the
  collection of screens"
  [screen state]
  (apply (:init (first screen)) (first screen) state (second screen))
)

(defn add-screens
  "Function called at the beginning of every frame iteration. It retrieves all
  the newly added screens from _screen-spawner and initializes them, adding 
  them to the actual game state's screen list. Has the side effect of emptying
  _screen-spawner."
  [state]
  (let [toadd @_screen-spawner
        screens (:screens state)]
    (reset! _screen-spawner [])
    (if (empty? toadd)
      state
      (doall
        (assoc state :screens (apply conj screens 
                                     (map #(create-screen % state) toadd)))
      )
    )
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
    (if (empty? entities)
      state
      (->> entities
           (comps/init-containers)
           (merge-with merge containers)
           (assoc state :containers)
      )
    )
  )
)

(defn add-entity!
  "Adds a single entity to the list of entities to be added in the next frame."
  [entity]
  (swap! _entity-spawner conj entity)
  entity
)

(defn destroy-entity!
  "Specifies the ID of a single entity that will be removed in the next frame."
  [entityID]
  (swap! _entity-recycler conj entityID)
)

(defn add-screen!
  "Adds a single screen on top of the screen list of the state."
  [screen params]
  (swap! _screen-spawner conj [screen params])
)

(defn destroy-screen!
  "Adds a single screen on top of the screen list to be removed."
  [screen]
  (swap! _screen-recycler conj screen)
)

(defn pop-screen
  "Removes the top-most screen from the list of screens in the state."
  [state]
  (let [toremove (last (:screens state))]
    (destroy-screen! toremove)
  )
)


(defn update-time 
  "Update the time passed since last update"
  [state]
  (let [current (.getTime (js/Date.))]
    (assoc state :dtime (- current (:time state)) :time current)
  )
)

(defn update-screen
  "Update a single screen"
  [screen state depth]
  ((:handler screen) screen state depth)
)

(defn update-screens
  "Update each single screen currently running in the state"
  [state]
  (doall
    (reduce-kv #(update-screen %3 %1 %2) state (:screens state))
  )
)

(defn clear-input-wrapper 
  "This function is just a wrapper to properly clean the global input states of
  mouse and keyboard while keeping track of the local gamestate."
  [state]
  (input/clear-mouse)
  state
)

(defn do-update
  "This function is called every update frame, it handles the basic CloisterJS
  functions for screen scheduling, entity recycling and components filtering. 
  It manages REPL hooks and handles frame succession."
  [state]
  (flush-pipeline) ; clear screen 
  (->> state
       (add-screens) ; take care of screen initialization
       (add-entities) ; add newly spawned entities
       (update-time) ; register time since last update
       ; TODO - REPL hooks go here
       (update-screens)
       (remove-screens) ; take care of screen removal
       (remove-entities) ; clear old entities
       (clear-input-wrapper)
       ((fn [s] (anim-method #(do-update s)))) ; schedule next frame
  )
)

(defn start 
  "Given a list of entities and a starting screen, initialize the gamestate and
  start the main loop."
  [entities screen screen-params]
  (add-screen! screen screen-params)
  (let [state (CloisterState. (.getTime (js/Date.)) (.getTime (js/Date.))
                              (comps/init-containers entities) [] nil)
        window (dom/getWindow)]
    (do-update state)
  )
)

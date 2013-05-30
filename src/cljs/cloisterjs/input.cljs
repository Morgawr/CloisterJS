(ns cloisterjs.input
  (:require [goog.events :refer [listen]]
            [goog.events.EventType :as etype])
)

; This is the mouse state, it hooks into the default mouse events for the game
; and holds the relevant data and correct positions of the canvas.
(def _mouse (atom {:x 0
                   :y 0
                   :lclick false
                   :rclick false}))

(defn mmove-listener [event]
  "OnMouseMove listener"
  (swap! _mouse assoc :x (.-clientX event) :y (.-clientY event))
)

(defn mlclick-listener [event]
  "OnMouseLeftClick listener"
  (swap! _mouse assoc :lclick true)
)

(defn mrclick-listener [event]
  "OnMouseRightClick listener"
  (swap! _mouse assoc :rclick true)
  false ; stop event propagation
)

(defn bind-mouse [element]
  "Binds all the related mouse events to the given HTML element"
  (listen element etype/MOUSEMOVE mmove-listener)
  (listen element etype/CLICK mlclick-listener)
  (set! (.-oncontextmenu element) mrclick-listener)
)

; This is how we should access the mouse state
(defn mouse-state [] @_mouse)

(defn clear-mouse []
  "This function should be called at the end of the game loop to remove
  click state"
  (swap! _mouse assoc :lclick false :rclick false)
)
  

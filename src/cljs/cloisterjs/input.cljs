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

(defn obtain-real-coords 
  "Given a mouse movement event, extract the X and Y coordinates relative to 
  the canvas' coordinates."
  [element event]
  { :x (- (.-clientX event) (.-offsetLeft element))
    :y (- (.-clientY event) (.-offsetTop element)) }
)

(defn mmove-listener 
  "OnMouseMove listener"
  [event]
   (this-as element (swap! _mouse conj (obtain-real-coords element event)))
)

(defn mlclick-listener 
  "OnMouseLeftClick listener"
  [event]
  (swap! _mouse assoc :lclick true)
)

(defn mrclick-listener 
  "OnMouseRightClick listener"
  [event]
  (swap! _mouse assoc :rclick true)
  false ; stop event propagation
)

(defn bind-mouse 
  "Binds all the related mouse events to the given HTML element"
  [element]
  (listen element etype/MOUSEMOVE mmove-listener)
  (listen element etype/CLICK mlclick-listener)
  (set! (.-oncontextmenu element) mrclick-listener)
)

; This is how we should access the mouse state
(defn mouse-state [] @_mouse)

(defn clear-mouse 
  "This function should be called at the end of the game loop to remove
  click state"
  []
  (swap! _mouse assoc :lclick false :rclick false)
)
  

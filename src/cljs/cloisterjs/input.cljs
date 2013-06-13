(ns cloisterjs.input
  (:require [goog.events :refer [listen]]
            [goog.events.EventType :as etype])
)

; This is the mouse state, it hooks into the default mouse events for the game
; and holds the relevant data and correct positions of the canvas.
; 
; More details: 
;
;   If (dragging && drag-ended) during a scheduler run, then the drag'n'drop 
; operation has just ended and the program should retrieve start-pos and end-pos
; to calculate the offset. The dragging state will be cleared at the end of the
; current scheduler operation so all components should react before then.
(def _lclick-default { 
                      :down false
                      :clicked false
                      :dragging false
                      :drag-ended false
                      :start-x 0
                      :start-y 0
                      :end-x 0
                      :end-y 0
                     })

(def _mouse (atom {:x 0
                   :y 0
                   :lbutton _lclick-default
                   :rbutton { :clicked false }
                  }))

(defn clear-mouse 
  "This function should be called at the end of the game loop to remove
  click state"
  []
  (let [lstate (:lbutton @_mouse)
        rstate (:rbutton @_mouse)]
    (swap! _mouse 
           assoc :rbutton (assoc rstate :clicked false))
    (if (and (:dragging lstate) (:drag-ended lstate))
      (swap! _mouse assoc :lbutton _lclick-default)
      (swap! _mouse assoc :lbutton (assoc lstate :clicked false))
    )
  )
)

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
  (this-as element 
    (let [lstate (:lbutton @_mouse)
          coords (obtain-real-coords element event)]
      (when (:down lstate)
        (swap! _mouse assoc :lbutton (assoc lstate 
                                            :dragging true
                                            :start-x (:x coords)
                                            :start-y (:y coords)
                                      )
        )
      )
      (swap! _mouse conj coords)
    )
  )
)

(defn mrclick-listener 
  "OnMouseRightClick listener"
  [event]
  (let [rstate (:rbutton @_mouse)
        lstate (:lbutton @_mouse)]
    (when (not (:dragging lstate))
      (swap! _mouse assoc :rbutton (assoc rstate :clicked true)))
    false ; stop event propagation
  )
)

(defn mouse-down [event]
  (this-as element
    (let [button (.-button event)
          {x :x y :y} (obtain-real-coords element event)]
      (when (zero? button)
        (swap! _mouse assoc :lbutton (assoc _lclick-default 
                                            :down true
                                            :start-x x
                                            :start-y y
                                            ))
      )
    )
  )
)

(defn mouse-up [event]
  (this-as element
    (let [button (.-button event)
          lstate (:lbutton @_mouse)
          {x :x y :y} (obtain-real-coords element event)]
      (when (zero? button)
        (if (:dragging lstate)
          (swap! _mouse assoc :lbutton (assoc lstate
                                              :down false
                                              :drag-ended true
                                              :end-x x
                                              :end-y y))
          (swap! _mouse assoc :lbutton (assoc _lclick-default 
                                              :down false
                                              :clicked true))
        )
      )
    )
  )
)

(defn mouse-leave [event]
  (this-as element
    (let [lstate (:lbutton @_mouse)
          {x :x y :y} (obtain-real-coords element event)]
      (if (:dragging lstate)
        (swap! _mouse assoc :lbutton (assoc lstate
                                            :down false
                                            :drag-ended true
                                            :end-x x
                                            :end-y y))
        (swap! _mouse assoc :lbutton (assoc lstate :down false))
      )
    )
  )
)

(defn bind-mouse 
  "Binds all the related mouse events to the given HTML element"
  [element]
  (listen element etype/MOUSEMOVE mmove-listener)
  (listen element "mousedown" mouse-down)
  (listen element "mouseup" mouse-up)
  (listen element "mouseleave" mouse-leave)
  (set! (.-oncontextmenu element) mrclick-listener)
)

; This is how we should access the mouse state
(defn mouse-state [] @_mouse)


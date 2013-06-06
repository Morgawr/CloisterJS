(ns cloisterjs.scheduler
  (:require [cloisterjs.image :refer [do-render fill-clear]]
            [goog.dom :as dom])
)

; This file contains all the screen scheduling functions. It takes the game
; state, loops it for each screen and passes the list of components to each
; screen's systems. It takes care of swapping screens, removing/adding them and
; timing the frame succession.

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

(defn start-render 
  "This function is called to set up and start the rendering engine, which will 
  execute endlessly"
  [canvas]
  (let [ctx (.getContext canvas "2d")
        frameloop (get-animation-method)]
    (frameloop (fn rendering []
                 (fill-clear ctx [(.-width canvas) (.-height canvas)] "white")
                 (do-render ctx)
                 (frameloop rendering)
               ))
  )
)

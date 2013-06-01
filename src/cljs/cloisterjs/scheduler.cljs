(ns cloisterjs.scheduler
  (:require [cloisterjs.image :refer [do-render]]
            [goog.dom :as dom])
)

; This file contains all the screen scheduling functions. It takes the game
; state, loops it for each screen and passes the list of components to each
; screen's systems. It takes care of swapping screens, removing/adding them and
; timing the frame succession.

(defn get-animation-method []
  "This function checks for the suitable frame scheduling method depending on 
  the specific browser being used."
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


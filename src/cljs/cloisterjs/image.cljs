(ns cloisterjs.image
  (:require [goog.dom :as dom]
            [cloisterjs.utils :as utils])
)

; This is the image pipeline, it builds a list of pairs: an index and a
; collection of image identifier + position. When it is time to
; render all the images, it sorts from the smallest to the biggest index and
; then renders each image (starting from the smallest) at the specified
; coordinates.
(def _pipeline (atom [])) 

; The bank is a map of keywords and image data, it is used for quick lookup of
; loaded image resources.
(def _bank (atom {}))

(declare _load-image)
(defn _load-error 
  "In case of a load error, retry after 200 ms"
  [sym uri temp-bank space]
  (let [window (dom/getWindow)]
    (. window setTimeout #(_load-image sym uri temp-bank space) 200)
  )
)

(defn _load-image 
  "Load an image into the temp-bank and set up a callback to check if the 
  bank is full. When it is full, it flushes the temp-bank into the actual _bank"
  [sym uri temp-bank space]
  (let [myimg (js/Image.)]
    (set! (. myimg -onload)
          (fn []
            (let [new-bank (assoc @temp-bank sym myimg)]
              (swap! temp-bank 
                     utils/test-plus-swap! 
                     assoc [sym myimg] ; first atom
                     (= (count new-bank) space) ; condition
                     _bank conj [new-bank] ; second atom
              )
            )
          ))
    (set! (. myimg -onerror) #(_load-error sym uri temp-bank space))
    (set! (. myimg -src) uri)
  )
)

(defn load-images 
  "Receives a collection of keys and uris of images to load, returns a function 
  returning the percentage of loaded images, 100% = done"
  [key-values]
  (let [loaded (atom {})
        total (count key-values)]
    (doseq [[k v] key-values] (_load-image k v loaded total))
    (fn [] 
      (* (/ (count @loaded) total) 100)
    )
  )
)

(defn flush-pipeline 
  "Empty the _pipeline"
  []
  (reset! _pipeline [])
)

(defn prepare-render 
  "Adds the image and coordinates to the _pipeline at a specified depth"
  [image pos depth]
  (swap! _pipeline conj (list depth [image pos]))
)

(defn immediate-render 
  "Immediately render the given image at the specified coordinates"
  [ctx image pos]
  (let [img (image @_bank)]
    (.drawImage ctx img (first pos) (second pos))
  )
)

(defn do-render 
  "Start the render process for the images in the _pipeline"
  [ctx]
  (let [imagedata (map second (sort #(<= (first %1) (first %2)) @_pipeline))]
    (doall
      (map #(immediate-render ctx (first %) (second %)) imagedata)
    )
  )
)

(defn fill-clear
  "Clears the whole surface and fills it with a new color"
  [ctx [width height] color]
  (set! (. ctx -fillStyle) color)
  (.fillRect ctx 0 0 width height)
)

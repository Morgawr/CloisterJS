(ns cloisterjs.image)

; This is the image pipeline, it builds a list of pairs: an index and a
; collection of image identifier + position. When it is time to
; render all the images, it sorts from the smallest to the biggest index and
; then renders each image (starting from the smallest) at the specified
; coordinates.
(def _pipeline (atom [])) 

; The bank is a map of keywords and image data, it is used for quick lookup of
; loaded image resources.
(def _bank (atom {}))

(defn flush-pipeline []
  "Empty the _pipeline"
  (reset! _pipeline [])
)

(defn prepare-render [image pos depth]
  "Adds the image and coordinates to the _pipeline at a specified depth"
  (swap! _pipeline conj (list depth [image pos]))
)

(defn immediate-render [ctx image pos]
  "Immediately render the given image at the specified coordinates"
  (let [img (image @_bank)]
    (.drawImage ctx img (first pos) (second pos))
  )
)

(defn do-render [ctx]
  "Start the render process for the images in the _pipeline"
  (let [imagedata (map second (sort #(< (first %1) (first %2)) @_pipeline))]
    (doall
      (map #(immediate-render ctx (first %) (second %)) imagedata)
    )
  )
)


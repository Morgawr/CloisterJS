(ns cloisterjs.audio
  (:require [cloisterjs.utils :as utils])
)

; The bank is a map of keywords and sound data, it is used for quick lookup of
; loaded sound resources.
(def _bank (atom {}))

(defn _can-play? [type]
  "Checks if the browser supports the specific audio type"
  (not (empty? (. (js/Audio.) canPlayType type)))
)

; Type of audio extension as supported by the browser, when calling init-audio
; and passing a series of extensions, the library will check which is the
; preferrable extension to be used and will set the proper extension. All audio
; files should have the same name with multiple extensions and be located at the
; same location, for a quick audio-lookup.
; i.e
; /var/www/resources/audio/mysong.ogg
; /var/www/resources/audio/mysong.mp3
; /var/www/resources/audio/mysong.wav
; Then only "/var/www/resources/audio/mysong" will be passed to the actual audio
; bank and it will be loaded with the proper extension.
(def _audio-ext (atom ""))

(declare _load-sound)
(defn _load-error [sym uri temp-bank space]
  "In case of a load error, retry after 200 ms"
  (let [window (dom/getWindow)]
    (. window setTimeout #(_load-sound sym uri temp-bank space) 200)
  )
)

(defn _load-sound [sym uri temp-bank space]
  "Load an audio file into the temp-bank and set up a callback to check if the 
  bank is full. When it is full, it flushes the temp-bank into the actual _bank"
  (let [myaudio (js/Audio.)]
    (. myaudio addEventListener "loadeddata"
       (fn []
         (let [new-bank (assoc @temp-bank sym myaudio)]
           (swap! temp-bank 
                  utils/test-plus-swap!
                  assoc [sym myaudio] ; first atom
                  (= (count new-bank) space) ; condition
                  _bank conj [new-bank] ; second atom
           )
          )
        ))
    (set! (. myaudio -onerror) #(_load-error sym uri temp-bank space))
    (set! (. myaudio -src) uri)
  )
)
     

(defn load-sounds [key-values]
  "Receives a colletion of keys and uris of the sounds to load, returns a 
  function returning the percentage of loaded sounds, 100% = done"
  (let [loaded (atom {})
        total (count key-values)]
    (doseq [[k v] key-values] (_load-sound k v loaded total))
    (fn []
      (* (/ (count @loaded) total) 100)
    )
  )
)






(ns cloisterjs.audio
  (:require [cloisterjs.utils :as utils])
)

; The bank is a map of keywords and sound data, it is used for quick lookup of
; loaded sound resources.
(def _bank (atom {}))

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

; Set of currently playing (with loop enabled) audio files.
(def _playing (atom #{}))

; Global check if the game is muted or not.
(def _muted (atom false))

(declare _load-sound)
(defn _load-error 
  "In case of a load error, retry after 200 ms"
  [sym uri temp-bank space]
  (let [window (dom/getWindow)]
    (. window setTimeout #(_load-sound sym uri temp-bank space) 200)
  )
)

(defn _load-sound 
  "Load an audio file into the temp-bank and set up a callback to check if the 
  bank is full. When it is full, it flushes the temp-bank into the actual _bank"
  [sym uri temp-bank space]
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

(defn load-sounds
  "Receives a colletion of keys and uris of the sounds to load, returns a 
  function returning the percentage of loaded sounds, 100% = done"
  [key-values]
  (let [loaded (atom {})
        total (count key-values)]
    (doseq [[k v] key-values] (_load-sound k (str v @_audio-ext) loaded total))
    (fn []
      (* (/ (count @loaded) total) 100)
    )
  )
)

(defn _can-play? 
  "Checks if the browser supports the specific audio type"
  [type]
  (not (empty? (. (js/Audio.) canPlayType type)))
)

(defn set-extensions 
  "Tests a number of extensions to find the one that works in the current 
  browser. Returns true if it succeds, else fails. Exts is a list of pairs 
  with extension attribute and proper mime type."
  [exts]
  (let [supported (filter #(_can-play? (second %)) exts)]
    (if (empty? supported) false
      (do
        (reset! _audio-ext (first (first supported)))
        true
      )
    )
  )
)

(defn pause
  "Pauses the sound"
  [sound]
  (. (sound @_bank) pause)
)

(defn stop 
  "Stops the sound and removes it from the _playing queue"
  [sound]
  (pause sound)
  (set! (. (sound @_bank) -currentTime) 0)
  (swap! _playing disj sound)
)

(defn play 
  "Plays the sound, setting the loop flag if required and adding the audio to 
  the _playing queue if looping."
  [sound looping]
  (when looping
    (swap! _playing conj sound))
  (set! (. (sound @_bank) -loop) looping)
  (if (not @_muted)
    (. (sound @_bank) play)
    nil
  )
)

(defn _play-all 
  "Resume all the currently paused _playing sounds."
  []
  (doall (map #(play % true) @_playing))
)

(defn _pause-all 
  "Pause all the currently playing _playing sounds."
  []
  (doall (map pause @_playing))
)

(defn stop-all 
  "Stop all the currently playing _playing sounds."
  []
  (doall (map stop @_playing))
)

(defn toggle-audio 
  "Sets and unsets the muted status of the game."
  []
  (let [toggled (swap! _muted not)]
    (if toggled
      (_pause-all)
      (_play-all)
    )
  )
)

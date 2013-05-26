(ns cloisterjs.utils)

(defn key-list [key nums]
  "Generates a list of identifiers appending each number to the 
  keyword provided"
  (map #(keyword (str key %)) nums)
)

(defn key-range [key min max]
  "Generates a list of identifiers appending each number from the range
  min-max to the keyword provided"
  (key-list key (range min max))
)

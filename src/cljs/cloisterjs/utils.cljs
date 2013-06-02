(ns cloisterjs.utils)

(defn key-list 
  "Generates a list of identifiers appending each number to the 
  keyword provided"
  [key nums]
  (map #(keyword (str key %)) nums)
)

(defn key-range 
  "Generates a list of identifiers appending each number from the range
  min-max to the keyword provided"
  [key min max]
  (key-list key (range min max))
)

(defn test-plus-swap! 
  "When condition is true, calls swap! on atom2 with func2 and body2. In all 
  cases it also returns the result expected from a proper swap! with atom1, 
  func1 and body1. Both body1 and body2 parameters are supposed to be 
  collections of actual parameters passed to swap!"
  [atom1 func1 body1 condition atom2 func2 body2]
  (when condition
    (swap! atom2 #(apply func2 %1 %2) body2))
  (apply func1 atom1 body1)
)


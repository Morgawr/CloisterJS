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


(defn test-and-swap! [condition temp-bank space sym image]
  "Test if temp-bank is completely filled, if it is then it flushes the contents
  to the real _bank"
  (let [new-bank (assoc temp-bank sym image)]
    (when (= (count new-bank) space)
     (swap! _bank conj new-bank))
    new-bank
  )
)

(defn test-plus-swap! [atom1 func1 body1 condition atom2 func2 body2]
  "When condition is true, calls swap! on atom2 with func2 and body2. In all 
  cases it also returns the result expected from a proper swap! with atom1, 
  func1 and body1. Both body1 and body2 parameters are supposed to be 
  collections of actual parameters passed to swap!"
  (when condition
    (swap! atom2 #(apply func2 %1 %2) body2) 
  (apply func1 atom1 body1)
)











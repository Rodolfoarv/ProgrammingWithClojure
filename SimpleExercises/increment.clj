; Recur version

(defn add1
  "Adds one to the element of x"
  [x]
  (loop [lst x
         result ()]
    (if (empty? lst)
      (reverse result)
      (recur (rest lst)
              (cons (inc (first lst)) result)))))

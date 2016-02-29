
(defn div-3-or-5?
  [x]
  (or (zero? (mod x 3))
      (zero? (mod x 5))))
(defn problem1
  [n]
  (->>
    (range 1 n)
    (filter div-3-or-5?)
    (reduce +)))

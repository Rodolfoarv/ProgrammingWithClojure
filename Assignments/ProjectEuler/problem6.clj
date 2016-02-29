
; Rodolfo Andrés Ramírez Valenzuela
; A01169701

(defn sum-of-square
  [n]
  (reduce + (map-indexed (fn [x y] (* x x)) (range 1 (+ 2 n)))))

(defn square-of-the-sum
  [n]
  (let [x (reduce + (range (inc n)))]
    (* x x)))

(defn sum-square-numbers
  "Returns the difference between the sum of the squares of the first one hundred
  numbers and the square of the sum"
  [n]
  (Math/abs (- (square-of-the-sum n) (sum-of-square n))))

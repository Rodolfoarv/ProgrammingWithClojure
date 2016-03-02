;Author: Rodolfo Andrés Ramírez Valenzuela

; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn divisible-all?
  [n m]
   (if (= n (count (take-while #(= 0 (mod m %)) (range 1 (inc n))))) true false))

(defn smallest-number
  [n]
  (loop [count 1]
    (if (divisible-all? n count)
      count
      (recur (inc count)))))
  ; (drop-while #(divisible-all? n %) (iterate inc 1)))

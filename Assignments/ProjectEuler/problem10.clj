(defn is-prime?
  [n]
  (empty? (filter #(= 0 (mod n %)) (range 2 n))))

;Sum of the primes
(defn sum-of-prime
  "Calculate the sum of the primes below the number n"
  [n]
  (reduce +(take-while #(< % n) (filter #(is-prime? %) (iterate inc 2)))))

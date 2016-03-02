(defn is-prime?
  [n]
  (empty? (filter #(= 0 (mod n %)) (range 2 n))))

(defn nth-prime
  [n]
  (last (take n (filter #(is-prime? %) (iterate inc 2)))))

;Largest prime factor
;Problem 3
;Author: Rodolfo Andrés Ramírez Valenzuela

(defn is-prime?
  [n]
  (empty? (filter #(= 0 (mod n %)) (range 2 n))))

(defn prime-factors
  [n]
  (last
    (for [i (range 1 n)
    :let [n n]
    :when (and (is-prime? i) (= 0 (mod n i))) ] i)))

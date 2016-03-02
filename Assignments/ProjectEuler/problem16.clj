
;Function must be executed as a big Integer
(defn exp [x n]
  (reduce * (repeat n x)))

(defn digits [n]
    (if (pos? n)
      (conj (digits (quot n 10)) (mod n 10) )
      []))

(defn sum-of-digits
  [n]
  (reduce + (digits (exp 2M n))))

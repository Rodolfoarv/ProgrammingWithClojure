(defn digits [n]
    (if (pos? n)
      (conj (digits (quot n 10)) (mod n 10) )
      []))


(defn factorial [n]
    (reduce * (range 1N (inc n))))

(defn sum-of-digits
  [n]
  (reduce + (digits (factorial n))))

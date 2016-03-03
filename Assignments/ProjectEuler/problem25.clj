(defn fib [a b] (lazy-seq (cons a (fib b (+ b a)))))

(defn fib-index
  [n]
  (last (take n (fib 1N 1))))

(defn digits [n]
    (if (pos? n)
      (conj (digits (quot n 10)) (mod n 10) )
      []))

(defn digit-fib
  [n]
  (loop [idx 1]
      (if (= n (count (digits (fib-index idx))))
        idx
        (recur (inc idx)))))

  ;
  ; (drop-while #(= n (count (digits %))) (take 100 (fib 1N 1))))

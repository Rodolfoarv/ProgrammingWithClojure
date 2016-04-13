(defmacro debug
  [expr]
  `(let [temp# ~expr]
      (printf "debug %s => %s%n" '~expr temp#)
      temp#))

(defn fact
  [n]
  (if (zero? n)
      1
      (debug (* n (fact (dec n))))))

(println (fact 5))

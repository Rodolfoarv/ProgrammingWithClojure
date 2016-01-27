(defn pow_recursion
  "Raises b to the power of e"
  [b e]
  (if (zero? e)
    1N ;gives a Big Integer
    (* b (pow b(dec e)))))

(defn pow_loop_recur
  "Raises b to the power of e"
  [b e]
  (loop [ i e
          r 1N]
    (if (zero? i)
      r
      (recur (dec i) (* b r)))))

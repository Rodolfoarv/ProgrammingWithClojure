; ******* Rodolfo Andrés Ramírez Valenzuela


(defn dup_recursive
  "Returns a list with all the elements of x duplicated"
  [x]
  (if(empty? x)
    ()
    (cons (first x)
      (cons (first x)
       (dup_recursive (rest x))))))


(defn dup_loop_recur
  [x]
  (loop [lst x
         result ()]
    (if (empty? lst)
      (reverse result)
      (recur (rest lst)
                (cons (first lst)
                  (cons (first lst) result))))))

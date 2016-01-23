; ******* Rodolfo Andrés Ramírez Valenzuela


(defn dup
  "Returns a list with all the elements of x duplicated"
  [x]
  (if(empty? x)
    ()
    (cons (first x)
      (cons (first x)
       (dup (rest x))))))

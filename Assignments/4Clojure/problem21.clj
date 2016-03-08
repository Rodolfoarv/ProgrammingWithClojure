; Write a function which returns the Nth element from a sequence.

(fn f [s n]
  (if (zero? n)
    (first s)
    (f (rest s) (dec n))))

(fn f [ s n ]
  (letfn [(f [s n]
            (if (zero? n)
              (first s)
              (f (rest s) (dec n))))]
  (f s n)))            

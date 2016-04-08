; Rodolfo Andrés Ramírez Valenzuela

;Takes three parameters as input, two values x and y and a sequence s
;It returns a new sequence in which every ocurrence of x is swapped to y
;and viceversa
(use 'clojure.test)

(defn swapper
  [x y lst]
  (map (fn [value]
          (cond
            (= value x) y
            (= value y) x
            :else
            value))
                    lst))
(deftest test-swapper
    (is (= ()
      (swapper 1 2 ())))
    (is (= '(4, 3, 4, 9, 9, 3, 3, 3, 9, 9, 7, 9, 3, 7, 8, 7, 8, 4, 5, 6)
      (swapper 1 2 [4, 3, 4, 9, 9, 3, 3, 3, 9, 9, 7, 9, 3, 7, 8, 7, 8, 4, 5, 6])))
    (is (= '(4 4 5 1 4 8 1 5 6 4 5 2 9 5 9 9 2 1 1 4)
      (swapper 1 2 [4, 4, 5, 2, 4, 8, 2, 5, 6, 4, 5, 1, 9, 5, 9, 9, 1, 2, 2, 4])))
    (is (= '(soft purr warm purr little ball of fur
                                    happy purr sleepy purr kitty kitty kitty)
                             (swapper 'purr 'kitty
                                   '(soft kitty warm kitty little ball of fur
                                          happy kitty sleepy kitty purr purr purr)))))


(run-tests)

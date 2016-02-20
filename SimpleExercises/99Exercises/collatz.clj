(use 'clojure.test)

;The Collatz conjecture is a conjecture in mathematics named after Lothar Collatz, who first proposed it in 1937.
;The conjecture is also known as the 3n + 1 conjecture

;Implement the Collatz conjecture, when it has an even number divide it by 2, if it is odd multiply it by 3 and sum 1
;eventually it will reach 1. Return the list of elements done by the conjecture.
(defn collatz
  "Returns a list with the collatz sequence"
  [x]
  (loop [number x result []]
    (if (= number 0)
      (reverse result)
      (if (= number 1)
        (recur 0 (cons 1 result))
        (if (even? number)
          (recur (/ number 2) (cons number result))
          (recur (+ (* 3 number) 1) (cons number result)))))))




(deftest test-collatz
  (is (= '(1) (collatz 1)))
  (is (= '(2 1) (collatz 2)))
  (is (= '(3 10 5 16 8 4 2 1) (collatz 3)))
  (is (= '(6 3 10 5 16 8 4 2 1) (collatz 6)))
  (is (= '(1024 512 256 128 64 32 16 8 4 2 1) (collatz 1024)))
  (is (= '(1023 3070 1535 4606 2303 6910 3455 10366 5183 15550
           7775 23326 11663 34990 17495 52486 26243 78730 39365
           118096 59048 29524 14762 7381 22144 11072 5536 2768
           1384 692 346 173 520 260 130 65 196 98 49 148 74 37
           112 56 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8
           4 2 1) (collatz 1023))))


(run-tests)

(use 'clojure.test)

; ----------------------------------------------------------
;  * Date: 12-Feb-2016
;  * Author:
;  *           Rodolfo ANdrés Ramírez Valenzuela
;  *
;  *----------------------------------------------------------

(defn digits
  [n]
  "Takes a number as parameter, it returns the digits of a number"
  (if (zero? n)
    '(0)
    (loop [number n result []]
      (if (zero? number)
        result
        (recur (quot number 10) (cons (mod number 10) result))))))


(deftest test-digits
  (is (= '(0) (digits 0)))
  (is (= '(7) (digits 7)))
  (is (= '(1 2 3) (digits 123)))
  (is (= '(7 9 1 9) (digits 7919)))
  (is (= '(1 0 0 0 0 0 0)) (digits 1000000))
  (is (= '(1 2 3 4 5 6 7 8 9 0) (digits 1234567890))))


(run-tests);

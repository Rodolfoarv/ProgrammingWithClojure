; ----------------------------------------------------------
;  * Date: 12-Feb-2016
;  * Author:
;  *           Rodolfo ANdrés Ramírez Valenzuela
;  *
;  *----------------------------------------------------------
(use 'clojure.test)

(defn gcd
  "Returns the gcd of two numbers"
  [x y]
  (if (= x y)
    x
    (if (> x y)
      (gcd (- x y) y)
      (gcd x (- y x)))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))



(run-tests)

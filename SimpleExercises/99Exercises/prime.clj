(use 'clojure.test)

(defn is-prime
  "Tests if a number is prime or not"
  [x]
  (cond
    (= x 3) true
    (= x 2) true
    (= 0 (mod x 2)) false
    (= 0 (mod x 3)) false
    :else
    true))


(deftest test-is-prime
  (is (= true (is-prime 7))))


(run-tests)

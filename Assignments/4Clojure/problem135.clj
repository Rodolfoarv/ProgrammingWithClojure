; Your friend Joe is always whining about Lisps using the prefix notation for math.
; how him how you could easily write a function that does math using the infix notation.
; Is your favorite language that flexible, Joe? Write a function that accepts a variable
; length mathematical expression consisting of numbers and the operations +, -, *, and /.
; Assume a simple calculator that does not do precedence and instead just calculates left to right.
(use 'clojure.test)

(defn infix-calculator
  [& args]
  (reduce (fn [x [f y]] (f x y))
          (first args)
          (partition 2 (rest args))))

(deftest test-infix-calculator
  (is (= 7  (infix-calculator 2 + 5)))
  (is (= 42 (infix-calculator 38 + 48 - 2 / 2)))
  (is (= 8  (infix-calculator 10 / 2 - 1 * 2)))
  (is (= 72 (infix-calculator 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))



(run-tests)

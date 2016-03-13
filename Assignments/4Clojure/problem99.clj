(use 'clojure.test)

(def digits
  (fn
  [x y]
  (loop [result [] value (* x y)]
    (if (= value 0)
      result
      (recur (cons (mod value 10) result) (quot value 10))))))

(deftest test-digits
  (is (= (digits 1 1) [1]))
  (is (= (digits 99 9) [8 9 1]))
  (is (= (digits 999 99) [9 8 9 0 1])))

(run-tests)

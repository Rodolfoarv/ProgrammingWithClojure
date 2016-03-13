; ; Write a function which returns a sequence of digits of a non-negative number
; (first argument) in numerical system with an arbitrary base (second argument).
; Digits should be represented with their integer values, e.g. 15 would be [1 5] in base 10,
;  [1 1 1 1] in base 2 and [15] in base 16.
(use 'clojure.test)

(defn digit-base
  [num base]
  (if (= 0 num)
    [num]
    (loop [result [] value num]
      (if (= 0 value)
        result
        (recur (cons (mod value base) result) (quot value base))))))

(deftest test-digit-base
  (is (= [1 2 3 4 5 0 1] (digit-base 1234501 10)))
  (is (= [0] (digit-base 0 11)))
  (is (= [1 0 0 1] (digit-base 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)](digit-base n n))))
  (is (= [16 18 5 24 15 1] (digit-base Integer/MAX_VALUE 42))))

(run-tests)

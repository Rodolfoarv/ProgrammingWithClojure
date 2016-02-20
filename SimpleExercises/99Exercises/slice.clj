
(use 'clojure.test)

(defn slice
  "Given two indices, I and K, returns a list containing between the Ith and Kth element (both limits included)"
  [list i k]
  (loop [i i
         k k
         count 1
         sliced false
         list list
         result []]
         (if (empty? list)
            (reverse result)
            (cond
              (= count (+ 1 k)) (recur i k (inc count) true () result)
              (= sliced true) (recur i k (inc count) true (rest list) (cons (first list) result))
              (= count i) (recur i k (inc count) true (rest list) (cons (first list) result) )
              :else
              (recur i k (inc count) false (rest list) result)))))


(deftest test-slice
  (is (= '(c d e f g) (slice '(a b c d e f g h i k) 3 7))))


(run-tests)

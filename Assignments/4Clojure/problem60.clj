;Write a function which behaves like reduce, but returns each intermediate value of the reduction.
;Your function must accept either two or three arguments, and the return sequence must be lazy.

(use 'clojure.test)

(defn reduction
  ([f s [x & xs]] (cons s (lazy-seq (when x (reduction f (f s x) xs)))))
  ([f coll] (reduction f (first coll) (rest coll))))

(deftest test-reduction
  (is (= (take 5 (reduction + (range))) [0 1 3 6 10]))
  (is (= (reduction conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (reduction * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))


(run-tests)

;Given a function f and a sequence s, write a function which returns a map.
; The keys should be the values of f applied to each item in s.
; The value at each key should be a vector of corresponding items in the order they appear in s.

(use 'clojure.test)

(defn group-sequence
  [f s]
  (into {}
    (map #(vector (f (first %)) (vec %)) (partition-by f (sort s)))))


(deftest test-group-sequence
  (is (= (group-sequence #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group-sequence #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (group-sequence count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(run-tests)

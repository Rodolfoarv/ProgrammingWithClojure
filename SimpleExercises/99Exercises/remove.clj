(use 'clojure.test)

(defn remove-at
  "Removes the kth element of a list"
  [lst k]
  (loop [list lst
         count 1
         result []]
         (if (empty? list)
            (reverse result)
            (if (= count k)
              (recur (rest list) (inc count) result)
              (recur (rest list) (inc count) (cons (first list) result))))))


(deftest test-remove-at
  (is (= '(a c d)  (remove-at '(a b c d) 2))))


(run-tests)

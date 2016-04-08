(use 'clojure.test)

(defn gcd
  [a b]
  (first (last
  (let [biggest (if (> a b) a b)]
  (take biggest
    (iterate (fn [[a b]]
        (cond
          (> a b) [(- a b) b]
          (< a b) [a (- b a)]
          :else
          [a b])) [a b]))))))

(deftest test-gcd
    (is (= 1 (gcd 13 7919)))
    (is (= 4 (gcd 20 16)))
    (is (= 6 (gcd 54 24)))
    (is (= 7 (gcd 6307 1995)))
    (is (= 12 (gcd 48 180)))
    (is (= 14 (gcd 42 56))))

(run-tests)

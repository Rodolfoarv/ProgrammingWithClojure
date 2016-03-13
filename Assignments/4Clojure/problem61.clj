
;Write a function which takes a vector of keys and a vector of values and constructs a map from them.

(use 'clojure.test)

(defn map-construction
  [keys values]
  (loop [keys keys values values result {}]
    (cond
      (empty? keys) result
      (empty? values) result
      :else
      (recur (rest keys) (rest values) (merge result (hash-map (first keys) (first values)))))))

(deftest test-map-construction
  (is (= (map-construction [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (map-construction [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (map-construction [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(run-tests)

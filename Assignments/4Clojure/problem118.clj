; Map is one of the core elements of a functional programming language. Given a function f and an input sequence s,
; return a lazy sequence of (f x) for each element x in s.

(use 'clojure.test)

;Execute this on 4Clojure
; (fn my-map [f s]
;   (if (false? (empty? s))
;     (lazy-seq (cons (f (first s)) (my-map f (rest s))))))


  ;General Solution
(def my-map
   (fn my-map [f s]
     (if (false? (empty? s))
       (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(deftest test-my-map
  (is (= [3 4 5 6 7]
  (my-map inc [2 3 4 5 6])))

  (is (= (repeat 10 nil)
  (my-map (fn [_] nil) (range 10))))

  (is (= [1000000 1000001]
   (->> (my-map inc (range))
        (drop (dec 1000000))
        (take 2)))))






(run-tests)

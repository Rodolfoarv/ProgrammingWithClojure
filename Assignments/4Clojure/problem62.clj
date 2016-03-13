;Given a side-effect free function f and an initial value x write a function which
;returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.

(use 'clojure.test)

;Execute this on 4Clojure
; (fn [f x]
;   (letfn [ (my-iterate [f x] (lazy-seq (cons x (my-iterate f (f x)))))] (my-iterate f x)))

;General Solution
(def my-iterate
  (fn
  [f x]
  (lazy-seq (cons x (my-iterate f (f x))))))

  (deftest test-my-iterate
    (is (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16]))
    (is (= (take 100 (my-iterate inc 0)) (take 100 (range))))
    (is (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(run-tests)

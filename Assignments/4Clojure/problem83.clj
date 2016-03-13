;  Write a function which takes a variable number of booleans.
; Your function should return true if some of the parameters are true, but not all of the parameters are true.
; Otherwise your function should return false.

(use 'clojure.test)

(defn half-truth
  [& args]
  (cond
    (every? true? args) false
    (= nil (some true? args)) false
    :else
    true))

(deftest test-half-truth
  (is (= false (half-truth false false)))
  (is (= true (half-truth true false)))
  (is (= false (half-truth true)))
  (is (= true (half-truth false true false)))
  (is (= false (half-truth true true true)))
  (is (= true (half-truth true true true false))))

(run-tests)

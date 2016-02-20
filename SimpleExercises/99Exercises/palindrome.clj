; Find out whether a list is palindrome
(use 'clojure.test)

(defn palindrome
  "Find out whether a list is palindrome or not"
  [list]
  (= list (reverse list)))

(deftest test-palindrome
  (is (= true (palindrome '(s u g u s))))
  (is (= false (palindrome '(s u g u))))
  (is (= true (palindrome '(a n i t a l a v a l a t i n a))))
  (is (= true (palindrome '(x a m a x)))))


(run-tests)

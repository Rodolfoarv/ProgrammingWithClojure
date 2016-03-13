;Write a predicate which checks whether or not a given sequence represents a binary tree.
;Each node in the tree must have a value, a left child, and a right child
(use 'clojure.test)


(defn tree?
  [t]
  (if (= 3 (count t))
    (cond
      (list? (second t)) (tree? (second t))
      (list? (last t)) (tree? (last t))
      :else
      true
      )false))


(deftest test-tree?
  (is (= (tree? '(:a (:b nil nil) nil))true))
  (is (= (tree? '(:a (:b nil nil)))
   false))
  (is (= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
   true))
  (is (= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false))
  (is (= (tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
   true))
  (is (= (tree? [1 [2 [3 [4 false nil] nil] nil] nil]) true)))
  (is (= (tree? '(:a nil ()))
   false))



  (run-tests)

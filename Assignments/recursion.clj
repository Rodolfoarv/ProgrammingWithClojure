; ----------------------------------------------------------
;  * Recursive Functions
;  * Date: 23-Jan-2016
;  * Author:
;  *           Rodolfo ANdrés Ramírez Valenzuela
;  *
;  *----------------------------------------------------------

(use 'clojure.test)

; 1.- The function my-count returns the number of elements contained in its input list
; Do not use the predifined count function.

;Recursive mode
(defn my-count
  "Returns the number of elements contained in its input list"
  [x]
  (if (empty? x)
    0
    (do
      (+ 1 (my-count(rest x))))))


; 2.-The function add-list returns the sum of all the elements in its input list
; or 0 if it is empty, Assume that all elements in the list are numbers.

(defn add-list [x]
  "Returns the sum of all elements in its input list or 0 if it is empty"
  (if (empty? x)
    0
    (do
      (+ (first x) (add-list (rest x))))))

; 3.- The function member? takes two arguments, any data x and a list lst.
; Returns true if x is contained in 1st, false otherwise.

(defn member? [x lst]
  "Returns true if x is contained in lst, false otherwise"
  (if (= x (first lst))
    true
    (if (empty? lst)
      false
      (member? x (rest lst)))))

; 4.- The function list-of-symbols? takes a lst as its argument. It returns true if all elements
; (possibly zero) contained in lst are symbols, or false otherwise. Use the symbol? predicate
; to determine if something is a symbol.

(defn list-of-symbols? [lst]
  "Returns true if all elements contained in the list are symbols, false otherwise"
  (if (empty? lst)
    true
    (if (symbol? (first lst))
      (list-of-symbols? (rest lst))
      false)))

; 5.- The function my-last returns the last element of its input list, or nil if its empty.
; Do not use the predefined last function.

(defn my-last [lst]
  "Returns the last element of its input list, or nil if its empty"
  (if (empty? lst)
    nil
    (if (= (second lst) nil)
      (first lst)
      (my-last (rest lst)))))

;6.- The function cons-end takes two arguments, any data x and a list lst.
; Returns a list composed by the same elements of lst but with x at the end.

(defn cons-end [x lst]
  "Returns a list composed by the same elements of lst but with x at the end"
  (if (empty? lst)
    (cons x lst)
    (cons (first lst)
      (cons-end x (rest lst)))))


;7.- The function my-reverse takes a list as an argument. It returns another list
; with the same elements as the input list, but in reverse order.

(defn my-reverse [lst]
  "Returns a list in reverse order"
  (loop [result [] list lst]
    (if (empty? list)
      result
      (recur (cons (first list) result) (rest list)))))

; 8.- The function my-butlast returns a list with the same elements as its input list
; but excluding the last element, or nil if it is empty.

(defn my-butlast [lst]
  "Returns a list with the same elements as its input excluding the last element"
  (if (empty? lst)
    nil
    (loop [result [] list lst]
      (if (= (second list) nil)
        (my-reverse result)
        (recur (cons (first list) result) (rest list))))))

; 9.- The function my-concat returns the resulting list of appending the two lists it takes as input
; Do not use the predifined concat function










; *************** UNIT TESTS ******************
;1.-
(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

;2.-
(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

;3.-
(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

;4.-
(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

;5.-
(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))

;6.-
(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

; 7.-
(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

; 8.-
(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

; 9.-
(deftest test-my-concat
    (is (= '(a b c) (my-concat '(a b c) ())))
    (is (= '(1 2 3) (my-concat () '(1 2 3))))
    (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))


(run-tests) ; Command used to run the tests

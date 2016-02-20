; ----------------------------------------------------------
;  * Recursive Functions
;  * Date: 23-Jan-2016
;  * Author:
;  *           A01169701 Rodolfo ANdrés Ramírez Valenzuela
;  *
;  *----------------------------------------------------------

(use 'clojure.test)

;1.- The function my-repeat takes a number n and any data x as its arguments. It returns
; a list that contains n copies of x.

(defn my-repeat [n x]
  "Returns a list that containts n copies of x"
  (loop [count n result []]
    (if (= 0 count)
      result
      (recur (dec count) (cons x result)))))

; 2.- The function invert-pairs takes as an argument a list of vectors containing two elements each.
; It returns a new list with every vector pair inverted.

(defn invert-pairs [lst]
  "Returns a new list with every vector pair inverted:"
  (loop [result [] list lst]
    (if (empty? list)
      (reverse result)
      (recur (cons (butlast (cons (second (first list)) (first lst))) result) (rest list)))))

; 3.- The function enlist surrounds in a list every upper-level element of the list it takes as input

(defn enlist [lst]
  "Surrounds in a list every upper level element of the list it takes input"
  (loop [result [] list lst]
    (if (empty? list)
      (reverse result)
      (recur (cons (cons (first list) ()) result) (rest list)))))

; 4.-The function my-interleave takes two arguments: the lists a and b.
 ; It returns a list containing the first element of a, followed by the first element of b, followed by the second element of a,
 ;  followed by the second element of b, and so on. The lists a and b don't have to be of the same size.
 ;  The interleaving of the elements stops when either a or b is exhausted. Do not use the predefined interleave function.

(defn my-interleave [lst1 lst2]
  "Returns a list containing the first element of a, followed by the first element of b, followed in sequence"
  (loop [result[] list1 lst1 list2 lst2]
    (if (empty? list1)
      (flatten (reverse result))
      (if (empty? list2)
        (flatten (reverse result))
        (recur (cons (list (first list1) (first list2)) result) (rest list1) (rest list2))))))

; 5.- The function my-flatten removes all the interior parenthesis of the list it takes
; as input.

; (defn my-flatten [lst]
;   (cond
;     (empty? lst) lst
;     (list? (first lst))
;     (cons (my-flatten (first lst)) (my-flatten (rest lst)))
;     :else
;     (cons (first lst) (my-flatten (rest lst)))))

; 6.- The function exchange takes three arguments: two non-list values x1 and x2, and a list lst.
; It returns a list with the same elements as lst, except that all occurrences of x1 are replaced by x2 and vice versa,
; including any occurrences inside nested lists

(defn exchange [x1 x2 lst]
  "Returns a list with the same elements as lst, except that all ocurrents of x2 are replaced by x2 and vice versa"
  (loop [result [] list lst]
    (if (empty? list)
      (reverse result)
      (if (= x1 (first list))
        (recur (cons x2 result) (rest list))
        (if (= x2 (first list))
          (recur (cons x1 result) (rest list))
          (recur (cons (first list) result) (rest list)))))))

; 7.- The function insert takes two arguments, a number n and a list of number lst in ascending order
; it returns a new list with the same elements as lst but insterting n in its corresponding place


;8.- The function my-sort takes an unordered list of numbers as an argument.
; and returns a new list with the same elements but in ascending order.
;You must use the insert function defined in the previous exercise to write the my-sort.


;9-The function binary takes an integer n as input (assume that n ≥ 0).
 ; If n is equal to zero, it returns an empty list. If n is greater than zero,
 ;  it returns a list with a sequence of ones and zeros equivalent to the binary representation of n.
 (defn log2 [n]
   (int (Math/ceil (/ (Math/log n) (Math/log 2)) )))

  (defn pow2 [n]
    (if (= n 0)
      1
      (* 2 (pow2 (dec n)))))

; (defn binary [n]
;   "If n is equal to zero, it returns an empty list. If n is great than zero it returns a list with a sequence
;   of ones and zeros equivalent to the binary representation of n"
;   (if (= n 0)
;     ()
;     (loop [result [] count (log2 n) number n]
;       (if (= n 0)
;         (reverse result)
;         (if (> quot))
;         (if (> (pow2 count) n)
;           (recur (cons 1 result) (dec count) (- number (pow2 count)))
;           (recur (cons 0 result) (dec count) number))))))
;


;The function encode-modified takes a list lst as its argument. It works the same as the previous problem,
; but if an element has no duplicates it is simply copied into the result list.
; Only elements with duplicates are converted to [n e] vectors. Unit tests:

(defn encode-modified [lst]
  "If an element has no duplicated it is simply copied into the result list,
  only elements with duplicates are converted to [n e] vectors"
  (loop [result []  count 0 lst list]
      (cond
        (empty? list) (reverse result)
        (= (first list) (second list)) (recur result  (inc count) (rest list))
        (= 0 count) (recur (cons (first list) result) 0 (rest list))
        :else
        (recur (cons [(inc count) (first list)] result) 0 (rest list)))))




; ********************************* List of Tests ******************************

;1.-
(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

; ;2.-
; (deftest test-invert-pairs
;   (is (= () (invert-pairs ())))
;   (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
;   (is (= '([1 January][2 February][3 March])
;          (invert-pairs '([January 1][February 2][March 3])))))

;3.-
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

;4.-
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

;5.-
; (deftest test-my-flatten
;   (is (= () (my-flatten ())))
;   (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
;   (is (= '(one two three four)
;          (my-flatten '(((one) ((two))) () (three (())) four)))))

;6.-
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
      (exchange true 42 '((true) 42 ((cool (42)) (true))))))

;7.-
; (deftest test-insert
;   (is (= '(14) (insert 14 ())))
;   (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
;   (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
;   (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

; ; 8.-
; (deftest test-my-sort
;   (is (= () (my-sort ())))
;   (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
;   (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
;   (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))
;

; ;9.-
; (deftest test-binary
;   (is (= () (binary 0)))
;   (is (= '(1 1 1 1 0) (binary 30)))
;   (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))
;
; ;10.-
; (deftest test-prime-factors
;   (is (= () (prime-factors 1)))
;   (is (= '(2 3) (prime-factors 6)))
;   (is (= '(2 2 2 2 2 3) (prime-factors 96)))
;   (is (= '(97) (prime-factors 97)))
;   (is (= '(2 3 3 37) (prime-factors 666))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(run-tests) ; Instruction used to run tests

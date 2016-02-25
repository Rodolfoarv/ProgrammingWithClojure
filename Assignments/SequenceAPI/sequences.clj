;----------------------------------------------------------
; Activity: Using the Sequence API
; Date: February 25, 2016.
; Authors:
;          A01169701 Rodolfo Andrés Ramírez Valenzuela
;----------------------------------------------------------

(use 'clojure.test)


;1.-
(defn positives
  "Takes a list of numbers lst as it arguments, returns a new list that contains only the positive numbers of lst"
  [lst]
  (remove neg? lst))

;2.-
(defn dot-product
  "Returns the result of performing the dot product of a times b"
  [a b]
  (reduce + (map * a b )))


;3.-
(defn pow
  "Returns the result of computing a raised to the power of b"
  [a b]
  (if (= 0 b)
    1
    (last (take b (iterate #(* % a) a)))))

;4.-
(defn replic
  "Returns a list that replicates n times each element contained n"
  [n lst]
  (mapcat #(repeat n %) lst))

;5.-
(defn expand
  "It returns a list where the first element of lst appears one time,
  the second elements appears two times, the third element appears three times, and so on."
  [lst]
  (flatten (map-indexed (fn [idx item] (repeat (inc idx) item)) lst)))

;6.-
(defn largest
  "Returns the largest value contained in lst. Use the reduce function to solve this problem"
  [lst]
  (reduce (fn [x y] (if (> x y) x y)) lst))

;7.-
(defn drop-every
  "Returns a new list that drops every n-th element from lst"
  [n lst]
  (if (< n 1)
    ()
    (flatten (map-indexed (fn [idx item] (if (= 0 (mod (inc idx) n)) () item)) lst))))

;8.-
(defn rotate-left
  "Returns the list that results from rotating lst a total n to the left and rotate
  to the right"
  [n lst]
  (if (empty? lst)
    ()
    (let [split (mod n (count lst))]
      (flatten (cons (last (split-at split lst)) (first (split-at split lst)))))))


;9.-
(defn gcd
  "Returns the greatest common divisor (GCD)"
  [a b]
  (first (drop-while #(not= 0 (+ (mod b %) (mod a %))) (reverse (range 1 b)))))

;10.-
(defn insert-everywhere
  [n lst]
   (map-indexed (fn [idx item] (flatten (cons (first (split-at idx lst))
                              (cons n (last (split-at idx lst)))))
                              ) (repeat (inc (count lst)) lst)))








; ***************** TESTS *******************



;1.-
(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))


;2.-
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0)))))

;3.-

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))

;4.-
(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) (replic 4 '(1 2 3 4)))))

;5.-
(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e) (expand '(a b c d e)))))

;6.-
(deftest test-largest
  (is (= 31 (largest '(31))))
  (is (= 5 (largest '(1 2 3 4 5))))
  (is (= -1 (largest '(-1 -2 -3 -4 -5))))
  (is (= 52 (largest '(32 -1 45 12 -42 52 17 0 21 2)))))

;7.-
(deftest test-drop-every
  (is (= () (drop-every 5 ())))
  (is (= '(1 2 3) (drop-every 4 '(1 2 3 4))))
  (is (= '(1 3 5 7) (drop-every 2 '(1 2 3 4 5 6 7 8))))
  (is (= '(1 3 5 7 9) (drop-every 2 '(1 2 3 4 5 6 7 8 9))))
  (is (= '(a b d e g h j) (drop-every 3 '(a b c d e f g h i j))))
  (is (= '(a b c d e f g h i j)
         (drop-every 20 '(a b c d e f g h i j))))
  (is (= () (drop-every 1 '(a b c d e f g h i j)))))

; ;8.-
(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

;9.-
(deftest test-gcd
  (is (= 4 (gcd 20 16)))
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
           (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
             (a 1 b c d e)
             (a b 1 c d e)
             (a b c 1 d e)
             (a b c d 1 e)
             (a b c d e 1))
            (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
             (1 x 2 3 4 5 6 7 8 9 10)
             (1 2 x 3 4 5 6 7 8 9 10)
             (1 2 3 x 4 5 6 7 8 9 10)
             (1 2 3 4 x 5 6 7 8 9 10)
             (1 2 3 4 5 x 6 7 8 9 10)
             (1 2 3 4 5 6 x 7 8 9 10)
             (1 2 3 4 5 6 7 x 8 9 10)
             (1 2 3 4 5 6 7 8 x 9 10)
             (1 2 3 4 5 6 7 8 9 x 10)
             (1 2 3 4 5 6 7 8 9 10 x))
            (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

(run-tests)

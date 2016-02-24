(use 'clojure.test)

(defn add-list
  "Returns the sum of all the elements of its input list,
  or 0 if its empty."
  [lst]
  ;(reduce + lst))
  (apply + lst))

(defn list-of-symbols?
  "Returns true if all the elements (possibly zero)
  contained in lst are symbols, or false otherwise."
  [lst]
  (every? symbol? lst))

(defn invert-pairs
  "Takes as an argument a list of vectors containing
  two elements each. It returns a new list with every
  vector pair inverted."
  [lst]
  (map (fn [[a b]] [b a]) lst))

(defn enlist
  "Surrounds in a list every upper-level element of
  the list it takes as input."
  [lst]
  (map list lst))

(defn insert
  "Returns a new list with the same elements as lst
  but inserting n in its corresponding place. lst should
  be in ascending order. "
  [n lst]
  (let [f #(< % n)]
    (concat (take-while f lst)
            (list n)
            (drop-while f lst))))

(defn binary
  "Takes an integer n as input (assume that n â‰¥ 0). If n is equal
  to zero, it returns an empty list. If n is greater than zero,
  it returns a list with a sequence of ones and zeros equivalent
  to the binary representation of n."
  [n]
  (->>
    [n ()]
    (iterate (fn [[n r]]
               [(quot n 2) (cons (rem n 2) r)]))
    (drop-while (fn [[n _]] (not (zero? n))))
    first
    second))

(defn pack
  "Returns consecutive repeated elements placed in separate sublists"
  [lst]
  (partition-by identity lst))

(defn compress
  "Replaces consecutive repeated elements replaced with a single copy"
  [lst]
  (->>
    (pack lst)
    (map first))) ;3rd parameter is the result of pack

(defn encode
  [lst]
  (->>
    (pack lst)
    (map #(vector (count %) (first %)))))

(defn encode-modified
  [lst]
    (->>
      (encode lst)
      (map (fn [[c,e]] (if (= 1 c) e [c e])))))



(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

  (deftest test-pack
    (is (= () (pack ())))
    (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
           (pack '(a a a a b c c a a d e e e e))))
    (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
    (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
    (is (= () (encode-modified ())))
    (is (= '([4 a] b [2 c] [2 a] d [4 e])
           (encode-modified '(a a a a b c c a a d e e e e))))
    (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
    (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(run-tests)

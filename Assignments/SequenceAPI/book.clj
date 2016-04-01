(use 'clojure.test)

(defrecord Book [author title year publisher])

;Write a function called books-in-range that takes three parameters
;as input, a sequence of book records s, a start year and an end year
;it returns a sequence of vectors of the form [title author]
;for every book contained in s where its year is between start and end inclusively
;The returned sequence must be sorted in ascending order by book title.
(defn books-in-range
  [seq start end]
  (sort
  (for [i seq :when (<=  start (:year i) end )] [(:title i) (:author i)])
  ))

(def book-seq
  [(->Book "Antoine de Saint-Exupéry" "Le Petit Prince" 1943 "Gallimard")
   (->Book "C.S. Lewis" "The Magician's Nephew" 1955 "The Bodley Head")
   (->Book "George Orwell" "Animal Farm" 1945 "Secker & Warburg")
   (->Book "George Orwell" "Nineteen Eighty-Four" 1949 "Secker & Warburg")
   (->Book "Isaac Asimov" "I, Robot" 1950 "Gnome Press")
   (->Book "J. K. Rowling" "Harry Potter and the Philosopher's Stone" 1997 "Bloomsbury")
   (->Book "J. R. R. Tolkien" "The Fellowship of the Ring" 1954 "George Allen & Unwin")
   (->Book "Lewis Carroll" "Alice's Adventures in Wonderland" 1865 "Macmillan")
   (->Book "Mary Shelley" "Frankenstein" 1818 "Lackington, Hughes, Harding, Mavor & Jones")
   (->Book "Pierre Boulle" "La Planète des Singes" 1963 "Le cercle du nouveau livre")
   (->Book "Suzanne Collins" "The Hunger Games" 2008 "Scholastic Press")
   (->Book "Vladimir Nabokov" "Lolita" 1955 "Olympia Press")])



(deftest test-books-in-range
  (is (= ()
         (books-in-range book-seq 1964 1996)))
  (is (= '(["The Hunger Games" "Suzanne Collins"])
         (books-in-range book-seq 2000 2015)))
  (is (= '(["Alice's Adventures in Wonderland" "Lewis Carroll"]
           ["Frankenstein" "Mary Shelley"])
         (books-in-range book-seq 1800 1900)))
  (is (= '(["Animal Farm" "George Orwell"]
           ["Le Petit Prince" "Antoine de Saint-Exupéry"]
           ["Nineteen Eighty-Four" "George Orwell"])
         (books-in-range book-seq 1943 1949)))
  (is (= '(["Alice's Adventures in Wonderland" "Lewis Carroll"]
           ["Animal Farm" "George Orwell"]
           ["Frankenstein" "Mary Shelley"]
           ["Harry Potter and the Philosopher's Stone" "J. K. Rowling"]
           ["I, Robot" "Isaac Asimov"]
           ["La Planète des Singes" "Pierre Boulle"]
           ["Le Petit Prince" "Antoine de Saint-Exupéry"]
           ["Lolita" "Vladimir Nabokov"]
           ["Nineteen Eighty-Four" "George Orwell"]
           ["The Fellowship of the Ring" "J. R. R. Tolkien"]
           ["The Hunger Games" "Suzanne Collins"]
           ["The Magician's Nephew" "C.S. Lewis"])
         (books-in-range book-seq 1800 2015))))

(run-tests)

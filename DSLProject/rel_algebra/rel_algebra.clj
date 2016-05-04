;----------------------------------------------------------
; Activity: Project: Relational Algebra DSL
; Date: May 4, 2016.
; Author:
;          A01169701 Rodolfo Andrés Ramírez Valenzuela
           ; 4CLOJURE nickname: rodolfoarv
;----------------------------------------------------------

(require '[clojure.string :as str])
(use 'clojure.test)

;--------------------------------------------------------------------
;--------------------------------------------------------------------
;                         CSV Functions
(defn read-csv
  "Function that returns a vector with the contents of the csv"
  [file-name]
  (str/split-lines (slurp (str (name file-name) ".csv"))))

(defn read-csv-keys
  "Function that returns the keys of a csv that will be mapped"
  [file-name]
  (map #(keyword %) (str/split (first (read-csv file-name)) #",")))

(defn read-csv-values
  "Function that returns the tuples of a csv that will be mapped"
  [file-name]
  (rest (read-csv file-name)))

;--------------------------------------------------------------------
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;--------------------------------------------------------------------
                          ; Auxiliary Methods

(defn parse-int [str]
  "Method that is used in relation algebra functions to parse a tuple value"
  (let [n (read-string str)]
       (if (number? n) n nil)))

 (defn check-argument
   "Check if condition is true. If not, throw an
   IllegalArgumentException with the given error-message."
   [condition error-message]
   (when (not condition)
     (throw (IllegalArgumentException. error-message))))

 (defn seq-contains?
   "Determine whether a sequence contains a given item"
   [sequence item]
   (if (empty? sequence)
     false
     (reduce #(or %1 %2) (map #(= %1 item) sequence))))

 (defn duplicates? [xs]
   "Determines if the given argument has duplicates"
   (not= (count (distinct xs)) (count xs)))

 (defn product-aux
   "Auxiliary method for the product in order to concatenate tuples"
   [val tuples]
   (let [total_tuples (repeat (count tuples) val)]
     (map (fn [r1 r2]
           (str r1 ","r2)) total_tuples tuples)))


 (defn project-aux
   "Auxiliary method for project"
   [tuple keys]
   (let [select (select-keys tuple keys)
         vals (str/join "," (vals select))]
         vals))

 (defn replace-keyword
   "Replaces a keyword with a expression in order to retrieve the value"
   [expression replacement]
   (loop [result [] lst expression]
     (let [head (first lst)
           tail (rest lst)]
     (cond
       (empty? lst) (reverse result)
       (keyword? head)  (recur (cons (replacement head) result) tail)
       (list? head)     (recur (cons (replace-keyword head replacement) result) tail)
       :else         (recur (cons head result) tail)))))

 (defn valid-keywords
   "Get the keywords that are contained on a expression"
   [expression relation]
   (let [keywords (filter keyword? (flatten expression))
         keys (distinct (into keywords (.keys relation)))]
   (check-argument
     (>= (count (.keys relation)) (count keys))
     (str "All keywords in expression must refer to an attribute in relation"))))

 (defn get-indices
   "Get the indices of the keywords"
   [keys]
     (->>
       (map-indexed #(vector %2 %1) keys)
       flatten
       (apply hash-map)))

 (defn int? [str]
   "Returns a number if its a number, or a String otherwise"
   (let [n (read-string str)]
        (if (number? n) n str)))

 (defn column-value
   "Returns the value of the given column"
   [row column keys]
   (let [indices (get-indices keys)
         aux (str/split row #"," )
         value   (nth aux (indices  column))]
      (int? value)))

 (defn switch-keyword-value
   "Method used to replace the keyword with the value in order to be compared"
   [expression record headers]
   (replace-keyword expression #(column-value record % headers)))

;--------------------------------------------------------------------
;--------------------------------------------------------------------
                        ;Print Table Functions

(defn length-array
  "Method that counts the largest record in order to create the table"
  [key dbrelation]
  (+ 2 (max (count (name key)) (count (last (sort-by count (map #(str (get % key)) dbrelation)))))))

(defn print-keys
  "Method to print the keys on the header of the table"
  [strlength keys]
  (str "|"
  (str/join '|
    (map (fn [length key]
            (let [newlength     (dec length)
                  splicestr     (str " %-" (str newlength) "s") ]
            (format splicestr (name key))))
             strlength keys)) "|\n"))

(defn print-line
  "Method that prints a single line on the table"
  [strlength]
      (str "+"
      (str/join '+
        (map #(str/join (repeat % '-)) strlength)) "+\n"))

(defn print-footer
  "Method that prints the footer of the table"
  [strlength]
  (str "+"
  (str/join '+
    (map #(str/join (repeat % '-)) strlength)) "+"))

(defn print-tuple
  "Method to print a tuple into the table"
  [strlength tuple]
  (let [columns (vals tuple)]
    (str "|"
    (str/join '|
      (map (fn [length value]
        (if (= nil (parse-int value))
          (let [newlength (dec length)
                splicestr (str " %-" (str newlength) "s")]
          (format splicestr (str value)))
          (let [newlength (dec length)
                splicestr (str "%" (str newlength) "s ")]
          (format splicestr (str value)))))
           strlength columns)) "|\n")))

;--------------------------------------------------------------------
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;--------------------------------------------------------------------
                              ;Relation Methods
(defn create-tuple
  "Function that returns a hash-map(tuple) with the structure of the csv"
  [keys values]
  (let [values (str/split values #",")]
    (apply assoc {} (interleave keys values))))

;"Create a Record specified by the keys and tuples of the CSV"
(defrecord Relation
  [keys tuples]
  Object
  (toString [this]
    (declare str-relation) ; str-relation is declared later.
    (str-relation this)))

(defmethod print-method Relation
  [this w]
  "print-method is used by the REPL to decide how to print
  an object of a specific type (in this case Rectangle).
  We use here the 'str' function (defined above as 'toString'
  to privide a consistent way of printing and converting to
  string."
  (print-simple (str this) w))

(defn str-relation
  "Create the String representation of the table"
  [relation]
  (check-argument
   (instance? Relation relation)
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class relation)))
  (let [
        keys (.keys relation)
        values (.tuples relation)
        dbrelation (map (fn [value] (create-tuple keys value)) values)
        strlength (map (fn [key] (length-array key dbrelation)) keys)
        ]
        (str
          (print-line strlength)
          (print-keys strlength keys)
          (print-line strlength)
          (str/join (apply concat (map #(print-tuple strlength % ) dbrelation)))
          (print-footer strlength))
        ))

(defn relation
  "Factory function for creating instances of Relation."
  [file-name]
  (check-argument
   (keyword? file-name)
   (str "Parameter 'file-name' must be a keyword. "
        "Value given: "
        file-name))
  (let [keys (read-csv-keys file-name)
        tuples (read-csv-values file-name)]
  (->Relation keys tuples)))


;--------------------------------------------------------------------
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;--------------------------------------------------------------------
                      ;Relational Algebra Functions

(defn union
    [r1 r2]
    "Returns a new relation object that contains all the rows in relation-a and relation-b"
    (check-argument
      (= (.keys r1) (.keys r2))
      (str "relations are not union-compatible (have the same name and same order)." ))
    (check-argument
     (and (instance? Relation r1) (instance? Relation r2))
     (str "Parameter 'relation' must be an instance of Relation, not "
          (class r1)))
    (->Relation (.keys r1) (distinct (concat (.tuples r1) (.tuples r2)))))

(defn difference
  "Returns a new relation object that contains the rows in relation-a that are not in relation-b."
  [r1 r2]
  (check-argument
    (= (.keys r1) (.keys r2))
    (str "relations are not union-compatible (have the same name and same order)." ))
  (check-argument
   (and (instance? Relation r1) (instance? Relation r2))
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class r1)))
  (->Relation (.keys r1) (filter #(not (seq-contains? (.tuples r2) %)) (.tuples r1))))

(defn intersection
  "Returns a new relation object that contains the rows in relation-a that are also in relation-b. "
  [r1 r2]
  (check-argument
    (= (.keys r1) (.keys r2))
    (str "relations are not union-compatible (have the same name and same order)." ))
  (check-argument
   (and (instance? Relation r1) (instance? Relation r2))
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class r1)))
  (->Relation (.keys r1) (filter #(seq-contains? (.tuples r2) %) (.tuples r1))))

(defn product
  "Returns a new relation object that contains the Cartesian product of relation-a times relation-b. "
  [r1 r2]
  (check-argument
   (and (instance? Relation r1) (instance? Relation r2))
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class r1)))
  (check-argument
    (not (duplicates? (concat (.keys r1) (.keys r2))))
    "Relations have an item in common")
    (let [new_keys (concat (.keys r1) (.keys r2))
          new_values (flatten (map (fn [val] (product-aux val (.tuples r2))) (.tuples r1)))]
      (->Relation new_keys new_values)))

(defn project
  "Returns a new relation object based on relation but only with the columns specified in attribute-vector."
  [v relation]
  (check-argument
   (vector? v)
   "Parameter is not a vector ")

  (check-argument
    (not (empty? v))
    "Vector is empty")

  (check-argument
   (instance? Relation relation)
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class relation)))

  (check-argument
    (every? keyword? v)
    "All items must be a keyword ")

  (check-argument
    (not (duplicates? v))
    "Duplicate value in vector")

  (check-argument
    (not (< (count (.keys relation)) (count (distinct (concat (.keys relation) v)))))
    "Vector includes an item that is not existent in the relation")

   (let
     [dbrelation     (map (fn [value] (create-tuple (.keys relation) value)) (.tuples relation))
      new_values     (distinct (flatten (map (fn [tuple] (project-aux tuple v)) dbrelation)))]
     (->Relation v new_values)))


(defn rename
  [v relation]
    (check-argument
     (vector? v)
     "Parameter is not a vector ")

   (check-argument
    (instance? Relation relation)
    (str "Parameter 'relation' must be an instance of Relation, not "
         (class relation)))

   (check-argument
    (= (count (distinct v)) (count (.keys relation)))
    "Parameter are repeated ")

   (check-argument
    (= (count v) (count (.keys relation)))
    "Parameter doesn't have the same keywords")

   (check-argument
    (every? keyword? v)
    "All items must be a keyword ")

  (->Relation v (.tuples relation)))



(defmacro select
  "It returns a new relation object containing all the rows in relation that meet
   the condition established in expression, which can be any Clojure expression.
   Any keyword used as part of expression must refer to an attribute in relation"
  [expression relation]

  `(let [
       expression#        '~expression
       keys#              (.keys ~relation)
       tuples#            (.tuples ~relation)
       keywords#          (valid-keywords expression# ~relation)
       data#              (filter #(eval (switch-keyword-value expression# % keys#)) tuples#)]
    (->Relation keys# data#)))



;--------------------------------------------------------------------
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;--------------------------------------------------------------------
                        ; UNIT TESTS



(def s1 (relation :students1))
(def s2 (relation :students2))
(def c (relation :courses))
(def e (relation :enrollments))
(def p (relation :pizzas))

(deftest test-relation
  (is (=
        (str "+-----+------------------+-----+\n"
             "| id  | name             | age |\n"
             "+-----+------------------+-----+\n"
             "| 199 | Gwen Stacy       |  18 |\n"
             "| 286 | Natalia Romanova |  25 |\n"
             "| 417 | Tony Stark       |  45 |\n"
             "| 430 | Peggy Carter     |  91 |\n"
             "| 447 | Peter Parker     |  18 |\n"
             "| 505 | Pepper Potts     |  32 |\n"
             "+-----+------------------+-----+")
        (str s1)))
  (is (=
        (str "+-----+------------------+------+\n"
             "| id  | name             | age  |\n"
             "+-----+------------------+------+\n"
             "| 286 | Natalia Romanova |   25 |\n"
             "| 430 | Peggy Carter     |   91 |\n"
             "| 505 | Pepper Potts     |   32 |\n"
             "| 528 | Steve Rogers     |   93 |\n"
             "| 559 | Jane Foster      |   29 |\n"
             "| 598 | MaryJane Watson  |   18 |\n"
             "| 666 | Damien Thorn     |   66 |\n"
             "| 824 | Bruce Banner     |   42 |\n"
             "| 993 | Diana Prince     | 3217 |\n"
             "+-----+------------------+------+")
        (str s2)))
  (is (=
        (str "+--------+----------------------------------+--------+------+\n"
             "| code   | name                             | time   | room |\n"
             "+--------+----------------------------------+--------+------+\n"
             "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
             "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
             "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
             "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
             "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
             "+--------+----------------------------------+--------+------+")
        (str c)))
  (is (=
        (str "+--------+-----+\n"
             "| code   | id  |\n"
             "+--------+-----+\n"
             "| TC2025 | 824 |\n"
             "| TC2025 | 286 |\n"
             "| TC2025 | 528 |\n"
             "| TC2006 | 528 |\n"
             "| TC2006 | 505 |\n"
             "| TC2006 | 993 |\n"
             "| TC2026 | 505 |\n"
             "| TC2026 | 666 |\n"
             "| TC2026 | 598 |\n"
             "| TC3049 | 430 |\n"
             "| TC3049 | 447 |\n"
             "| TC3049 | 666 |\n"
             "| TC3048 | 666 |\n"
             "| TC3048 | 417 |\n"
             "| TC3048 | 430 |\n"
             "+--------+-----+")
        (str e)))
  (is (= (str "+-----------+--------+\n"
              "| flavor    | size   |\n"
              "+-----------+--------+\n"
              "| Hawaiian  | Medium |\n"
              "| Pepperoni | Large  |\n"
              "| Supreme   | Small  |\n"
              "+-----------+--------+")
         (str p)))
  (is (thrown?
        IllegalArgumentException
        (relation 42))
      "Should throw exception if parameter to relation is not a keyword.")
  (is (thrown?
        java.io.FileNotFoundException
        (relation :inexistent))
      "Should throw exception if the requested file was not found."))

(deftest test-union
  (is (= (str "+-----+------------------+------+\n"
              "| id  | name             | age  |\n"
              "+-----+------------------+------+\n"
              "| 199 | Gwen Stacy       |   18 |\n"
              "| 286 | Natalia Romanova |   25 |\n"
              "| 417 | Tony Stark       |   45 |\n"
              "| 430 | Peggy Carter     |   91 |\n"
              "| 447 | Peter Parker     |   18 |\n"
              "| 505 | Pepper Potts     |   32 |\n"
              "| 528 | Steve Rogers     |   93 |\n"
              "| 559 | Jane Foster      |   29 |\n"
              "| 598 | MaryJane Watson  |   18 |\n"
              "| 666 | Damien Thorn     |   66 |\n"
              "| 824 | Bruce Banner     |   42 |\n"
              "| 993 | Diana Prince     | 3217 |\n"
              "+-----+------------------+------+")
         (str (union s1 s2))))
  (is (= (str "+-----+------------------+------+\n"
              "| id  | name             | age  |\n"
              "+-----+------------------+------+\n"
              "| 286 | Natalia Romanova |   25 |\n"
              "| 430 | Peggy Carter     |   91 |\n"
              "| 505 | Pepper Potts     |   32 |\n"
              "| 528 | Steve Rogers     |   93 |\n"
              "| 559 | Jane Foster      |   29 |\n"
              "| 598 | MaryJane Watson  |   18 |\n"
              "| 666 | Damien Thorn     |   66 |\n"
              "| 824 | Bruce Banner     |   42 |\n"
              "| 993 | Diana Prince     | 3217 |\n"
              "| 199 | Gwen Stacy       |   18 |\n"
              "| 417 | Tony Stark       |   45 |\n"
              "| 447 | Peter Parker     |   18 |\n"
              "+-----+------------------+------+")
         (str (union s2 s1))))
  (is (thrown?
        IllegalArgumentException
        (union c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (union c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-difference
  (is (= (str  "+-----+--------------+-----+\n"
               "| id  | name         | age |\n"
               "+-----+--------------+-----+\n"
               "| 199 | Gwen Stacy   |  18 |\n"
               "| 417 | Tony Stark   |  45 |\n"
               "| 447 | Peter Parker |  18 |\n"
               "+-----+--------------+-----+")
         (str (difference s1 s2))))
  (is (= (str "+-----+-----------------+------+\n"
              "| id  | name            | age  |\n"
              "+-----+-----------------+------+\n"
              "| 528 | Steve Rogers    |   93 |\n"
              "| 559 | Jane Foster     |   29 |\n"
              "| 598 | MaryJane Watson |   18 |\n"
              "| 666 | Damien Thorn    |   66 |\n"
              "| 824 | Bruce Banner    |   42 |\n"
              "| 993 | Diana Prince    | 3217 |\n"
              "+-----+-----------------+------+")
         (str (difference s2 s1))))
  (is (thrown?
        IllegalArgumentException
        (difference c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (difference c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-intersection
  (is (= (str "+-----+------------------+-----+\n"
              "| id  | name             | age |\n"
              "+-----+------------------+-----+\n"
              "| 286 | Natalia Romanova |  25 |\n"
              "| 430 | Peggy Carter     |  91 |\n"
              "| 505 | Pepper Potts     |  32 |\n"
              "+-----+------------------+-----+")
         (str (intersection s1 s2))))
  (is (= (str "+--------+----------------------------------+--------+------+\n"
              "| code   | name                             | time   | room |\n"
              "+--------+----------------------------------+--------+------+\n"
              "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+--------+----------------------------------+--------+------+")
         (str (intersection c c))))
  (is (thrown?
        IllegalArgumentException
        (intersection c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (intersection c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-product
  (is (= (str "+-----+------------------+-----+-----------+--------+\n"
              "| id  | name             | age | flavor    | size   |\n"
              "+-----+------------------+-----+-----------+--------+\n"
              "| 199 | Gwen Stacy       |  18 | Hawaiian  | Medium |\n"
              "| 199 | Gwen Stacy       |  18 | Pepperoni | Large  |\n"
              "| 199 | Gwen Stacy       |  18 | Supreme   | Small  |\n"
              "| 286 | Natalia Romanova |  25 | Hawaiian  | Medium |\n"
              "| 286 | Natalia Romanova |  25 | Pepperoni | Large  |\n"
              "| 286 | Natalia Romanova |  25 | Supreme   | Small  |\n"
              "| 417 | Tony Stark       |  45 | Hawaiian  | Medium |\n"
              "| 417 | Tony Stark       |  45 | Pepperoni | Large  |\n"
              "| 417 | Tony Stark       |  45 | Supreme   | Small  |\n"
              "| 430 | Peggy Carter     |  91 | Hawaiian  | Medium |\n"
              "| 430 | Peggy Carter     |  91 | Pepperoni | Large  |\n"
              "| 430 | Peggy Carter     |  91 | Supreme   | Small  |\n"
              "| 447 | Peter Parker     |  18 | Hawaiian  | Medium |\n"
              "| 447 | Peter Parker     |  18 | Pepperoni | Large  |\n"
              "| 447 | Peter Parker     |  18 | Supreme   | Small  |\n"
              "| 505 | Pepper Potts     |  32 | Hawaiian  | Medium |\n"
              "| 505 | Pepper Potts     |  32 | Pepperoni | Large  |\n"
              "| 505 | Pepper Potts     |  32 | Supreme   | Small  |\n"
              "+-----+------------------+-----+-----------+--------+")
         (str (product s1 p))))
  (is (= (str "+-----------+--------+--------+----------------------------------+--------+------+\n"
              "| flavor    | size   | code   | name                             | time   | room |\n"
              "+-----------+--------+--------+----------------------------------+--------+------+\n"
              "| Hawaiian  | Medium | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Hawaiian  | Medium | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Hawaiian  | Medium | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Hawaiian  | Medium | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Hawaiian  | Medium | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "| Pepperoni | Large  | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Pepperoni | Large  | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Pepperoni | Large  | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Pepperoni | Large  | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Pepperoni | Large  | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "| Supreme   | Small  | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Supreme   | Small  | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Supreme   | Small  | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Supreme   | Small  | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Supreme   | Small  | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+-----------+--------+--------+----------------------------------+--------+------+")
         (str (product p c))))
  (is (thrown?
        IllegalArgumentException
        (product c 42))
      "Should throw exception if a parameter is a not a relation.")
  (is (thrown?
        IllegalArgumentException
        (product c s1))
      "Should throw exception if attributes in both relations are not distinct."))

(deftest test-project
  (is (= (str "+-----+\n"
              "| age |\n"
              "+-----+\n"
              "|  18 |\n"
              "|  25 |\n"
              "|  45 |\n"
              "|  91 |\n"
              "|  32 |\n"
              "+-----+")
         (str (project [:age] s1))))
  (is (= (str "+------+----------------------------------+\n"
              "| room | name                             |\n"
              "+------+----------------------------------+\n"
              "| 5204 | Advanced Programming             |\n"
              "| 6307 | Programming Languages            |\n"
              "| 4207 | Web Applications Development     |\n"
              "| 4310 | Software Design and Architecture |\n"
              "| 4202 | Compiler Design                  |\n"
              "+------+----------------------------------+")
         (str (project [:room :name] c))))
  (is (= (str "+--------+\n"
              "| code   |\n"
              "+--------+\n"
              "| TC2025 |\n"
              "| TC2006 |\n"
              "| TC2026 |\n"
              "| TC3049 |\n"
              "| TC3048 |\n"
              "+--------+")
         (str (project [:code] e))))
  (is (= (str "+-----+------------------+-----+\n"
              "| id  | name             | age |\n"
              "+-----+------------------+-----+\n"
              "| 199 | Gwen Stacy       |  18 |\n"
              "| 286 | Natalia Romanova |  25 |\n"
              "| 417 | Tony Stark       |  45 |\n"
              "| 430 | Peggy Carter     |  91 |\n"
              "| 447 | Peter Parker     |  18 |\n"
              "| 505 | Pepper Potts     |  32 |\n"
              "+-----+------------------+-----+")
         (str (project [:id :name :age] s1))))
  (is (thrown?
        IllegalArgumentException
        (project '(:code) c))
      "Should throw exception if first parameter is not a vector.")
  (is (thrown?
        IllegalArgumentException
        (project [] c))
      "Should throw exception if attribute vector is empty.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code 'time :room] c))
      "Should throw exception if in the attribute vector there's a non-keyword.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code :name :time] c))
      "Should throw exception if the attribute vector has repeated values.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :inexistent :code] c))
      "Should throw exception if the attribute vector includes an attribute that does not exist in the relation.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code] 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-rename
  (is (= (str "+------------+------------------+-------------+\n"
              "| student-id | student-name     | student-age |\n"
              "+------------+------------------+-------------+\n"
              "|        199 | Gwen Stacy       |          18 |\n"
              "|        286 | Natalia Romanova |          25 |\n"
              "|        417 | Tony Stark       |          45 |\n"
              "|        430 | Peggy Carter     |          91 |\n"
              "|        447 | Peter Parker     |          18 |\n"
              "|        505 | Pepper Potts     |          32 |\n"
              "+------------+------------------+-------------+")
         (str (rename [:student-id :student-name :student-age] s1))))
  (is (= (str "+--------+----------------------------------+--------+------+\n"
              "| a      | b                                | c      | d    |\n"
              "+--------+----------------------------------+--------+------+\n"
              "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+--------+----------------------------------+--------+------+")
         (str (rename [:a :b :c :d] c))))
  (is (thrown?
        IllegalArgumentException
        (rename '(:c :n :t :r) c))
      "Should throw exception if first parameter is not a vector.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n 't :r] c))
      "Should throw exception if in the attribute vector there's a non-keyword.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :c :r] c))
      "Should throw exception if the attribute vector has repeated values.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :r] c))
      "Should throw exception if the number of attributes in the attribute vector and in the relation are not the same.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :t :r] 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-select
  (is (= (str "+-----+------------+-----+\n"
              "| id  | name       | age |\n"
              "+-----+------------+-----+\n"
              "| 417 | Tony Stark |  45 |\n"
              "+-----+------------+-----+")
         (str (select (= :id 417) s1))))
  (is (= (str "+-----+--------------+-----+\n"
              "| id  | name         | age |\n"
              "+-----+--------------+-----+\n"
              "| 199 | Gwen Stacy   |  18 |\n"
              "| 447 | Peter Parker |  18 |\n"
              "| 505 | Pepper Potts |  32 |\n"
              "+-----+--------------+-----+")
         (str (select (or (< :age 20) (> :id 500)) s1))))
  (is (= (str "+--------+-----------------------+--------+------+\n"
              "| code   | name                  | time   | room |\n"
              "+--------+-----------------------+--------+------+\n"
              "| TC2025 | Advanced Programming  | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages | We1900 | 6307 |\n"
              "+--------+-----------------------+--------+------+")
         (str (select
                (or (= :code "TC2025")
                    (and (zero? (rem :room 7))
                         (= "We" (subs :time 0 2))))
                c))))
  (is (thrown?
        IllegalArgumentException
        (select (= :inexistent 42)  s1) )
      "Should throw exception if using in select expression an attribute that does not exist in relation.")
  (is (thrown?
        IllegalArgumentException
        (select (= :age 18) 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-composed-1
  "In what room and at what time is the 'Web Applications Development' course taught?"
  (is (= (str "+------+--------+\n"
              "| room | time   |\n"
              "+------+--------+\n"
              "| 4207 | Tu1900 |\n"
              "+------+--------+")
         (str (->>
                (select (= :name "Web Applications Development") c)
                (project [:room :time]))))))

(deftest test-composed-2
  "List the name of all the students who are between 45 and 100 years old."
  (is (= (str "+--------------+\n"
              "| name         |\n"
              "+--------------+\n"
              "| Tony Stark   |\n"
              "| Peggy Carter |\n"
              "| Steve Rogers |\n"
              "| Damien Thorn |\n"
              "+--------------+")
         (str (->>
                (union s1 s2)
                (select (<= 45 :age 100))
                (project [:name]))))))

(deftest test-composed-3
  "For two pizzas, list all the possible flavor permutations."
  (is (= (str "+-----------+-----------+\n"
              "| pizza-1   | pizza-2   |\n"
              "+-----------+-----------+\n"
              "| Hawaiian  | Hawaiian  |\n"
              "| Hawaiian  | Pepperoni |\n"
              "| Hawaiian  | Supreme   |\n"
              "| Pepperoni | Hawaiian  |\n"
              "| Pepperoni | Pepperoni |\n"
              "| Pepperoni | Supreme   |\n"
              "| Supreme   | Hawaiian  |\n"
              "| Supreme   | Pepperoni |\n"
              "| Supreme   | Supreme   |\n"
              "+-----------+-----------+")
         (str (->>
                (project [:flavor] p)
                (rename [:f])
                (product p)
                (project [:flavor :f])
                (rename [:pizza-1 :pizza-2]))))))

(deftest test-composed-4
  "List the name of all the courses in which 'Damien Thorn' is enrolled."
  (is (= (str "+----------------------------------+\n"
              "| name                             |\n"
              "+----------------------------------+\n"
              "| Web Applications Development     |\n"
              "| Software Design and Architecture |\n"
              "| Compiler Design                  |\n"
              "+----------------------------------+")
         (str (->>
                (union s1 s2)
                (select (= :name "Damien Thorn"))
                (project [:id])
                (rename [:s-id])
                (product e)
                (select (= :id :s-id))
                (project [:code])
                (rename [:e-code])
                (product c)
                (select (= :code :e-code))
                (project [:name]))))))

(deftest test-composed-5
  "List the name of all the students who are not enrolled in the 'Programming Languages' course."
  (is (= (str "+------------------+\n"
              "| name             |\n"
              "+------------------+\n"
              "| Gwen Stacy       |\n"
              "| Natalia Romanova |\n"
              "| Tony Stark       |\n"
              "| Peggy Carter     |\n"
              "| Peter Parker     |\n"
              "| Jane Foster      |\n"
              "| MaryJane Watson  |\n"
              "| Damien Thorn     |\n"
              "| Bruce Banner     |\n"
              "+------------------+")
         (str (let [s (union s1 s2)]
                (->>
                  (select (= :name "Programming Languages") c)
                  (project [:code])
                  (rename [:c-code])
                  (product e)
                  (select (= :code :c-code))
                  (project [:id])
                  (rename [:e-id])
                  (product s)
                  (select (= :id :e-id))
                  (project [:id :name :age])
                  (difference s)
                  (project [:name])))))))

;Macro context issue
(deftest test-composed-6
  "List the id and name of all the students that are enrolled in both the 'Compiler Design' and 'Software Design and Architecture' courses."
  (is (= (str "+-----+--------------+\n"
              "| id  | name         |\n"
              "+-----+--------------+\n"
              "| 430 | Peggy Carter |\n"
              "| 666 | Damien Thorn |\n"
              "+-----+--------------+")
         (str (let [s (union s1 s2)]
                (->>
                  (intersection
                    (->>
                      (select (= :name "Compiler Design") c)
                      (project [:code])
                      (rename [:c-code])
                      (product e)
                      (select (= :code :c-code))
                      (project [:id])
                      (rename [:e-id])
                      (product s)
                      (select (= :id :e-id))
                      (project [:id :name]))

                    (->>
                      (select (= :name "Software Design and Architecture") c)
                      (project [:code])
                      (rename [:c-code])
                      (product e)
                      (select (= :code :c-code))
                      (project [:id])
                      (rename [:e-id])
                      (product s)
                      (select (= :id :e-id))
                      (project [:id :name])))))))))

(run-tests)

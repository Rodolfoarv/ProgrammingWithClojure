(require '[clojure.string :as str])

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

 (defn get-keywords
   "Get the keywords that are contained on a expression"
   [expression]
   (filter keyword? (flatten expression)))

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
       keywords#          (get-keywords expression#)
       data#              (filter #(eval (switch-keyword-value expression# % keys#)) tuples#)]
    (->Relation keys# data#)))


(def s1 (relation :students1))
(def s2 (relation :students2))
(def c (relation :courses))
(def e (relation :enrollments))
(def p (relation :pizzas))
:ok

(defn last-test
  [course-name]
  (let [s (union s1 s2  )]
  (->>
    (select (= :name course-name) c)
    (project [:code])
    (rename [:c-code])
    (product e)
    (select (= :code :c-code))
    (project [:id])
    (rename [:e-id])
    (product s)
    (select (= :id :e-id))
    (project [:id :name]))))

(require '[clojure.string :as str])

; (defmulti make-instance (fn [class & rest] class))
; (defmacro defrecord* [record-name fields]
;   `(do
;     (defrecord ~record-name ~fields)
;     (defmethod make-instance (quote ~record-name) [_# & {:keys ~fields}]
;       (new ~record-name ~@fields))))

; (defmacro defentity [name & values]
;   `(defrecord ~name ~@values))

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

(defn create-tuple
  "Function that returns a hash-map(tuple) with the structure of the csv"
  [keys values]
  (let [values (str/split values #",")]
    (apply assoc {} (interleave keys values))))

(defrecord Relation
  [keys tuples]
  Object
  (toString [this]
    (declare str-relation) ; str-relation is declared later.
    (str-relation this)))

;--------------------------------------------------------------------
;; print-method is used by the REPL to decide how to print
;; an object of a specific type (in this case Rectangle).
;; We use here the 'str' function (defined above as 'toString')
;; to privide a consistent way of printing and converting to
;; string.
(defmethod print-method Relation
  [this w]
  (print-simple (str this) w))

;--------------------------------------------------------------------

(defn check-argument
  "Check if condition is true. If not, throw an
  IllegalArgumentException with the given error-message."
  [condition error-message]
  (when (not condition)
    (throw (IllegalArgumentException. error-message))))


(defn length-array
  [key dbrelation]
  (+ 2 (max (count (name key)) (count (last (sort-by count (map #(str (get % key)) dbrelation)))))))

(defn print-keys
  [strlength keys]
  (str "|"
  (str/join '|
    (map (fn [length key]
            (let [newlength (dec length)
                  splicestr (str " %-" (str newlength) "s") ]

            (format splicestr (name key))))
             strlength keys)) "|\n"))

(defn print-line
  [strlength]
      (str "+"
      (str/join '+
        (map #(str/join (repeat % '-)) strlength)) "+\n"))

(defn print-footer
  [strlength]
  (str "+"
  (str/join '+
    (map #(str/join (repeat % '-)) strlength)) "+"))

(defn parse-int [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

(defn print-tuple
  [strlength tuple]
  (let [columns (vals tuple)]
    (str "|"
    (str/join '|
      (map (fn [length value]
        (if (= nil (parse-int value))
          (let [newlength (dec length)
                splicestr (str " %-" (str newlength) "s")]
          (format splicestr (str value)))
          ;else
          (let [newlength (dec length)
                splicestr (str "%" (str newlength) "s ")]
          (format splicestr (str value)))))
           strlength columns)) "|\n")))


(defn str-relation
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

; dbrelation (map (fn [value] (create-tuple keys value)) values)

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

(defn seq-contains?
  "Determine whether a sequence contains a given item"
  [sequence item]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))

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
  nil)


(def s1 (relation :students1))
(def s2 (relation :students2))
(def c (relation :courses))
(def e (relation :enrollments))
(println (str s1))
(println (union s1 s2))
(println (intersection s1 s2))
:ok

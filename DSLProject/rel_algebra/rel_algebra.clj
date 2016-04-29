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
  [file-name]
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
  [keys strlength]
  (map #() strlength)
  (format "+"))

(defn print-header
  [strlength]
      (str "+"
      (str/join '+
        (map #(str/join (repeat % '-)) strlength)) "+\n"))


(defn str-relation
  [relation]
  (check-argument
   (instance? Relation relation)
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class relation)))
  (let [
        keys (read-csv-keys (.file-name relation))
        values (read-csv-values (.file-name relation))
        dbrelation (map (fn [value] (create-tuple keys value)) values)
        strlength (map (fn [key] (length-array key dbrelation)) keys)
        ]
        (print-header strlength)))

; dbrelation (map (fn [value] (create-tuple keys value)) values)

(defn relation
  "Factory function for creating instances of Relation."
  [file-name]
  (->Relation file-name))

(def s1 (relation :students1))
(println (str s1))
(str s1)

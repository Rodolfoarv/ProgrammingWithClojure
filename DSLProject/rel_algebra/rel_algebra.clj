(require '[clojure.string :as str])

(defn read-csv
  "Function that returns a vector with the contents of the csv"
  [file-name]
  (str/split-lines (slurp (str (name file-name) ".csv"))))

(defn read-csv-keys
  [file-name]
  (map #(keyword %) (str/split (first (read-csv file-name)) #",")))

(defn read-csv-values
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

(defn check-argument
  "Check if condition is true. If not, throw an
  IllegalArgumentException with the given error-message."
  [condition error-message]
  (when (not condition)
    (throw (IllegalArgumentException. error-message))))

(defn largest-values
  [keys values]
  (count (first values)))


(defn str-relation
  [relation]
  (check-argument
   (instance? Relation relation)
   (str "Parameter 'relation' must be an instance of Relation, not "
        (class relation)))
  (let [
        keys (read-csv-keys (.file-name relation))
        values (read-csv-values (.file-name relation))
        largest (largest-values keys values)]
        (print largest)))

; dbrelation (map (fn [value] (create-tuple keys value)) values)

(defn relation
  "Factory function for creating instances of Rectangle."
  [file-name]
  (->Relation file-name))

(str (relation :students1))
; :ok

(defmulti make-instance (fn [class & rest] class))
(defmacro defrecord* [record-name fields]
  `(do
    (defrecord ~record-name ~fields)
    (defmethod make-instance (quote ~record-name) [_# & {:keys ~fields}]
      (new ~record-name ~@fields))))
(defrecord* Person [name age])
(def f1 (make-instance 'Person :age 99 :name "bob"))
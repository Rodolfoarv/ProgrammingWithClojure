(require '[clojure.string :as str])

(defn read-csv
  "Function that returns a vector with the contents of the csv"
  [file-name]
  (str/split-lines (slurp (str file-name ".csv"))))

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

(defn relation
  "Function that creates a record with the contents of the csv"
  [file-name]
  (let [keys (read-csv-keys file-name) values (read-csv-values file-name)]
    (map (fn [value] (create-tuple keys value)) values)))


(first (relation "students1"))


; :ok

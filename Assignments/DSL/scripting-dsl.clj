(defmulti emit-bash
  (fn [form]
    (class form)))

(defmethod emit-bash
  clojure.lang.PersistentList
  [form]
  (let [n (name (first form))]
    (cond
      (= n "println") (str "echo " (second form))
      :else
      nil)))

(defmethod emit-bash
  java.lang.String
  [form]
  form)

(defmethod emit-bash
  java.lang.Integer
  [form]
  (str form))

(defmethod emit-bash
  java.lang.Double
  [form]
  (str form))

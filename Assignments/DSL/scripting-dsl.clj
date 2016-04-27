(defn emit-bash-form [a]
  (let [cl (class a)]
    (cond
      (= cl clojure.lang.PersistentList)
        (let [n (name (first a))]
          (cond
            (= n "println") (str "echo " (second a))
            :else
            nil))
      (= cl java.lang.String) a
      (= cl java.lang.Long) (str a)
      (= cl java.lang.Double) (str a)
      :else
      nil)))

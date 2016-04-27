(defn emit-bash-form [a]
  (let [cl (class a)]
    (cond
      (= cl java.lang.String) a
      (= cl java.lang.Long) (str a)
      (= cl java.lang.Double) (str a)
      :else
      nil)))

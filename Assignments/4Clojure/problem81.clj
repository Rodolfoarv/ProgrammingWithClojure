(defn my-intersection
  [x y]
  (s
  (for [i x
        :when (contains? y i) ] i)))

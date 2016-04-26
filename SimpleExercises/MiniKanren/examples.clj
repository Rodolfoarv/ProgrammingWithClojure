(use 'clojure.core.logic)

(defn my-last
  [lst]
  (cond
    (empty? (rest lst)) (first lst)
    (not (empty? (rest lst))) (my-last (rest lst))))

(defn lasto
  [lst result]
  (fresh [h t]
    (conde
      [(conso result t lst) (== t [])] ;unification with empty list to ask if t is empty
      ;first option [(conso h t lst) (== t []) (== h result)  ]
      [(conso h t lst) (!= t []) (lasto t result)])))

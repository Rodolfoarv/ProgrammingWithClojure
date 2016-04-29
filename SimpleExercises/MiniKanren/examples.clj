(use 'clojure.core.logic)

(defn my-last
  [lst]
  (cond
    (empty? (rest lst)) (first lst)
    (not (empty? (rest lst))) (my-last (rest lst))))

; (defn lasto
;   [lst result]
;   (fresh [h t]
;     (conde
;       [(conso result t lst) (== t [])] ;unification with empty list to ask if t is empty
;       ;first option [(conso h t lst) (== t []) (== h result)  ]
;       [(conso h t lst) (!= t []) (lasto t result)])))

;Pattern matching

(defne lasto
  [lst result]
  ([[h] h])
  ([[h . t] result]
    (!= t [])
    (lasto t result)))

(defn reverseo
  [lst result]
  (fresh [h t x ]
    (conde
      [(== lst []) (== result [])]
      [(!= lst [])
      (conso h t lst)
      (reverseo t x)
      (appendo x [h] result)])))

; First implementation of dup

(defn dup
  [lst]
  (cond
    (empty? lst) ()
    (not (empty? lst)) (concat [(first lst) (first lst)] (dup (rest lst)))))

;Logic function

(defn dupo
  [lst result]
  (conde
    [(== lst []) (== result [])]
    [(!= lst [])
      (fresh [h t x]
        (conso h t lst)
        (dupo t x)
        (appendo [h h] x result))]))

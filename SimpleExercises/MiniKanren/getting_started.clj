(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)



;Use of conde
; (run* [q]
;   (conde
;     [(== q 1)]
;     [(== q 2) (== q 3)]
;     [(== q :abc)]))

;Dissecting a spell
; (run* [q] (conso :a [:b :c] q))
; (run* [q] (conso :a q [:a :b :c]))
; (run* [q] (fresh [h t] (conso h t [:a :b :c]) (== q [h t])))

; (defn insideo [e l]
;   (conde
;     [(fresh [h t]
;       (conso h t l)
;       (== h e))]
;     [(fresh [h t]
;       (conso h t l)
;       (insideo e t))]))
;
; (run* [q] (insideo q [:a :b :c]))
; (run 3 [q] (insideo :a q))
; (run* [q] (insideo :d [:a :b :c q]))





;Day 2

; Matche
;
; (defn insideo [e l]
;   (matche [l]
;     ([[e . _]])
;     ([[_ . t]] (insideo e t))))
;
; (defne exampleo [a b c]
;   ([:a _ _])
;   ([_ :b x] (membero x [:x :y :z])))
;
; (defne insideo [e l]
;   ([_ [e . _]])
;   ([_ [_ . t]] (insideo e t)))
;

;Maps
(run* [q]
  (fresh [m a b]
    (== m {:a 1 :b 2})
    (conde
      [(featurec m {:a a}) (== q [:found-a a])]
      [(featurec m {:b b}) (== q [:found-b b])]
      [(featurec m {:a a :b b}) (== q [:found-a-and-b a b])])))

;Other kinds of cond

; (defn whicho [x s1 s2 r]
;   (conde
;     [(membero x s1) (== r :one)]
;     [(membero x s2) (== r :two)]
;     [(membero x s1) (membero x s2) (== r :both)]))

;Using conda

(defn whicho [x s1 s2 r]
  (conda
  [(all (membero x s1) (membero x s2) (== r :both))]
  [(all (membero x s1) (== r :one))]
  [(all (membero x s2) (== r :two))]))

;Reimplementing insideo with condu

(defn insideo [e l]
  (condu
    [(fresh [h t]
      (conso h t l)
      (== h e))]
    [(fresh [h t]
      (conso h t l)
      (insideo e t))]))


;Day 3
;Introducing finite domains

(require '[clojure.core.logic.fd :as fd])

(run* [q]
  (fd/in q (fd/interval 0 10))
  (fd/<= q 1))

(run* [q]
  (fresh [x y z a]
    (== q [x y z])
    (fd/in x y z a (fd/interval 1 100))
    (fd/distinct [x y z])
    (fd/< x y)
    (fd/< y z)
    (fd/eq
      (= (+ x y z) 100) )))

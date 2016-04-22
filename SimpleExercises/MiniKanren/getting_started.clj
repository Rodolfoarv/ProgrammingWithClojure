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

(defn insideo [e l]
  (conde
    [(fresh [h t]
      (conso h t l)
      (== h e))]
    [(fresh [h t]
      (conso h t l)
      (insideo e t))]))

(run* [q] (insideo q [:a :b :c]))
(run 3 [q] (insideo :a q))
(run* [q] (insideo :d [:a :b :c q]))

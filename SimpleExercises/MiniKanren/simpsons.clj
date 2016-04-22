(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)

;Relationship
(db-rel fathero f ch)
(db-rel mothero m ch)

(def simpsons
  (db
    [fathero :homer :bart]
    [fathero :homer :lisa]
    [fathero :homer :maggie]
    [mothero :marge :bart]
    [mothero :marge :lisa]
    [mothero :marge :maggie]
    [mothero :jackeline :marge]
    [fathero :clancy :marge]
    [fathero :abraham :homer]
    [mothero :mona :homer]
    [mothero :jackeline :patty]
    [mothero :jackeline :selma]
    [fathero :clancy :patty]
    [fathero :clancy :selma]
    [mothero :selma :ling]))

(with-db simpsons (run* [q] (fathero :homer q)))
(with-db simpsons (run* [q] (fathero q :lisa)))

;Combined with-db and run*
(run-db* simpsons [q1 q2] (fathero q1 q2))

;To determine grandparents
;Suffix "o" is a logic fuction

;To use ands we an use all, fresh or run, this means conjunctions
;Disjunction conde, we need to use or because the grandmother can be of a father or a mother

(defn grand-mothero
  [gm gch]
  (fresh [m]
    (conde
      [(mothero gm m) (mothero m gch)]
      [(mothero gm m) (fathero m gch)])))

(defn grand-fathero
  [gf gch]
  (fresh [f]
    (conde
      [(fathero gf f) (mothero f gch)]
      [(fathero gf f) (fathero f gch)])))

(run-db* simpsons [q] (grand-fathero q :bart))
(run-db* simpsons [q] (grand-fathero :clancy q))

(defn marriedo
  [h w]
  (fresh [ch]
    (mothero w ch) (fathero h ch)))

(run-db* simpsons [q] (marriedo :homer :marge))

(defn siblingo
  [p1 p2]
  (fresh [x]
    (!= p1 p2)
    (conda
      [(mothero x p1) (mothero x p2)]
      [(fathero x p1) (mothero x p2)])))

(run-db* simpsons [q1 q2] (siblingo q1 q2))

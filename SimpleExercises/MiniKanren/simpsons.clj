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

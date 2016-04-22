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
    [mothero :homer :bart]
    [mothero :homer :lisa]
    [mothero :homer :maggie]


    ))

(with-db simpsons (run* [q] (fathero :homer q)))
(with-db simpsons (run* [q] (fathero q :lisa)))

;Combined with-db and run*
(run-db* simpsons [q1 q2] (fathero q1 q2))

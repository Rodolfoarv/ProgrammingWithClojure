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

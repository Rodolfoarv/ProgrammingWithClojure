(defn pow
  "Raises b to the power of e"
  [b e]
  (if (zero? e)
    1N ;gives a Big Integer
    (* b (pow b(dec e)))))

; lein repl
; (load-file recursive.clj)
;

; Find out whether a list is palindrome
(use 'clojure.test)

;Split a list into two parts;  the length of the first part is given
(defn split
  "Splits a list into two parts, given by the length of the split"
  [lst split]
  (lazy-seq (split-at split (into [] lst))))

(deftest test-split
  (is (= '((0 1 2) (3 4 5)) (split '(0 1 2 3 4 5) 3))))


(run-tests)

;;;  Função que recebe uma lista e uma posição e retorna  ;;;
; ;;;  outra lista contendo os elementos ate e inclusive    ;;;
; ;;;  posicao indicada.                                    ;;;
; 
; (defun split (pos org-list &optional (ini 0))
;     (if (> pos ini)
;         (cons (car org-list) (split pos (cdr org-list) (+ ini 1)))))
;
;
; ;;;  Função que recebe uma lista e uma posição e retorna  ;;;
; ;;;  outra lista contendo os elementos apos a posicao     ;;;
; ;;;  indicada.                                            ;;;
;
; (defun split-after (pos org-list &optional (ini 0))
;     (if (> pos ini)
;         (split-after pos (cdr org-list) (+ ini 1))
;         org-list))

#lang sicp

; Generates all subsets of a set.
;
; Parameters:
; s - set
;
; Returns:
; list of all subsets
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (l) (append l (list (car s))))
                     rest)))))

(equal? (subsets nil) (list nil))
(equal? (subsets (list 1)) (list nil (list 1)))
(equal? (subsets (list 1 2)) (list nil (list 2) (list 1) (list 2 1)))

#lang sicp

; Returns union of sets.
;
; Parameters:
; s1, s2 - sets to union
;
; Returns:
; union of specified sets
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (r1 (cdr s1))
               (x2 (car s2))
               (r2 (cdr s2)))
           (cond ((< x1 x2) (cons x1 (union-set r1 s2)))
                 ((> x1 x2) (cons x2 (union-set s1 r2)))
                 ((= x1 x2) (cons x1 (union-set r1 r2))))))))

(equal? (union-set (list) (list)) (list))
(equal? (union-set (list) (list 2)) (list 2))
(equal? (union-set (list 2) (list)) (list 2))
(equal? (union-set (list 1) (list 2)) (list 1 2))
(equal? (union-set (list 1) (list 1)) (list 1))
(equal? (union-set (list 2) (list 1)) (list 1 2))
(equal? (union-set (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(equal? (union-set (list 4 5 6) (list 1 2 3)) (list 1 2 3 4 5 6 ))
(equal? (union-set (list 1 2 3) (list 1 2 3 4 5 6)) (list 1 2 3 4 5 6))
(equal? (union-set (list 1 2 3) (list 0 1 2 3 4)) (list 0 1 2 3 4))
(equal? (union-set (list 1 2 3) (list 0 1 2 3)) (list 0 1 2 3))
(equal? (union-set (list 0 1 2 3 4) (list 1 2 3)) (list 0 1 2 3 4))
(equal? (union-set (list 0 1 2 3 4) (list 2 3 4 5 6)) (list 0 1 2 3 4 5 6))
(equal? (union-set (list 0 2 4 6 8) (list 3 5 7 9)) (list 0 2 3 4 5 6 7 8 9))
#lang sicp

; Adds element to set.
;
; Parameters:
; x - element to add
; set - set to add element
;
; Returns:
; set with added element
(define (adjoin-set x set)
  (cond ((null? set) (cons x nil))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

(equal? (adjoin-set 5 (list)) (list 5))
(equal? (adjoin-set 5 (list 6)) (list 5 6))
(equal? (adjoin-set 5 (list 5)) (list 5))
(equal? (adjoin-set 5 (list 4)) (list 4 5))
(equal? (adjoin-set 5 (list 4 6)) (list 4 5 6))
(equal? (adjoin-set 5 (list 2 3 4)) (list 2 3 4 5))
(equal? (adjoin-set 5 (list 5 8 9)) (list 5 8 9))
(equal? (adjoin-set 5 (list 3 4 5 9)) (list 3 4 5 9))
(equal? (adjoin-set 5 (list 2 3 4 5)) (list 2 3 4 5))


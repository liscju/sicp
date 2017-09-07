#lang sicp

; Includes elements from list that satisfy specified predicate.
;
; Parameters:
; f - predicate
; l - list
;
; Returns:
; list of elements from l that satisfies predicate f
(define (filter f l)
  (if (null? l)
      nil
      (let ((head (car l))
            (tail (cdr l)))
        (if (f head)
            (cons head (filter f tail))
            (filter f tail)))))

; Returns arguments that have same parity as first argument.
;
; Parameters:
; x - first argument
; l - rest of arguments
;
; Returns:
; arguments that have same parity as first argument
(define (same-parity x . l)
  (cons x (filter (lambda (y) (= (remainder y 2) (remainder x 2))) l)))
#lang sicp

; Accumulates elements of sequence using specified operation.
;
; Accumulate operation is defined as follows:
; (accumulate op initial (list e1 e2)) == (op e1 (op e2 initial))
;
; Parameters:
; op - operation that accumulates sequence elements
; initial - initial accumulate value
; sequence - list of elements
;
; Returns:
; accumulated sequence
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Another name for accumulate.
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

; Accumulates elements of sequence from left side.
;
; Accumulate left is defined as follows:
; (fold-left op initial (list e1 e2 e3 ...)) =
;         (op (op (op initial e1) e2) e3 ...)
;
; Parameters:
; op - accumulate operator
; initial - initial value of accumulator
; sequence - sequence to accumulate
;
; Returns:
; accumulated value
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(fold-right / 1 (list 1 2 3))
; 3/2

(fold-left / 1 (list 1 2 3))
; 1/6

(fold-right list nil (list 1 2 3))
; (list 1 (list 2 (list 3)))

(fold-left list nil (list 1 2 3))
; (list (list (list nil 1) 2) 3)

; fold-left = fold-right when operator is associative


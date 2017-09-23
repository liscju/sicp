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

; Count leaves in the tree.
;
; Parameters:
; t - tree
;
; Returns:
; number of leaves in the tree
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                             1
                             (count-leaves x)))
                       t)))

(= 0 (count-leaves (list)))
(= 1 (count-leaves (list 1)))
(= 2 (count-leaves (list (list 1 2))))
(= 3 (count-leaves (list (list 1 2) (list 3))))

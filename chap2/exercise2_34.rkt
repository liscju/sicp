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

; Evaluates polynomial value at specified point using horner method.
;
; Horner method for evaluating polynomial:
; p = a_{n}*x^n + a_{n-1}*x^(n-1) + ... + a_{1}*x + a_{0}
; at the point x is the following:
; (...(a_{n}*x+a_{n-1})*x + ...a_{1})*x + a_{0}
; 
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(= 0 (horner-eval 1 (list)))
(= 5 (horner-eval 1 (list 5)))
(= 7 (horner-eval 3 (list 1 2)))
(= 31 (horner-eval 2 (list 3 4 5)))

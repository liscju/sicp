#lang sicp

; Creates function that invokes specified unary function two times on
; given argument.
;
; Created function g(x) = f(f(x))
;
; Parameters:
; f - unary function to invoke
;
; Returns:
; unary function that invokes f two times on given argument
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)
; = 21
#lang sicp

; Creates composite function from specified functions.
;
; Composite function of f and g is f(g(x)).
;
; Parameters:
; f, g - functions to compose
;
; Returns:
; composite function
(define (compose f g)
  (lambda (x) (f (g x))))

; Computes square of argument.
;
; Parameters:
; x - argument to square
;
; Returns:
; squared argument
(define (square x)
  (* x x))

((compose square inc) 6)
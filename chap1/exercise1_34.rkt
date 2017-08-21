#lang sicp

(define (square x)
  (* x x))

(define (f g)
  (g 2))


; (f f)
; error -> expected a procedure that can be applied to arguments
;
; f takes as argument 'g' unary function applicable to '2'. Trying
; to apply f as argument raises error that f cant take as a function
; scalar value
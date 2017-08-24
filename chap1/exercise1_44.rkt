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

; Identity function.
;
; Identity function returns its argument.
;
; Parameters:
; x - argument
;
; Returns:
; argument
(define (id x)
  x)

; Computes functional power of a function f to power n.
;
; f^n(x) = f(f(f(....(f(x)))))
;          ^^^^^^^^^^^^
;          ||||||||||||
;             n times
;
; Parameters:
; f - function
; n - power
;
; Returns:
; functional power of a function f to power n
(define (repeated f n)
  (cond ((= n 0) id)
        ((= n 1) f)
        (else (lambda (x) ((compose f (repeated f (- n 1))) x)))))

; Calculates average value from specified numbers.
;
; It is specialised version of average for three arguments.
;
; Parameters:
; a, b, c - numbers to calculate average value
;
; Returns:
; average of a, b, c
(define (average3 a b c)
  (/ (+ a b c) 3))

; Infinite small smoothness paramater.
;
; Values is much smaller than expected argument to smoothed function.
(define dx 2)

; Transform specified function to more smooth version.
;
; Parameters:
; f - function to transform
;
; Returns:
; smooth version of original function
(define (smooth f)
  (lambda (x)
    (average3 (f (- x dx)) (f x) (f (+ x dx)))))

; (repeat (smooth f) n)

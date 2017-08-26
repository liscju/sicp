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

; Fixed point calculation precision.
;
; Value was chosen to be much smaller than needed
; precision.
(define tolerance 0.00001)

; Finds fixed point of a function f with specified first guess.
;
; Fixed point of a function f is a point x such as f(x) = x.
;
; Parameters:
; f - function to find fixed-point
; first-guess - first guess of a fixed point
;
; Returns:
; found fix point
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Calculates average value of x and y.
;
; Parameters:
; x, y - calculate average for these values
;
; Returns:
; average value of x and y
(define (average x y)
  (/ (+ x y) 2))

; Transforms function f to smoother version.
;
; Parameters:
; f - function
;
; Returns:
; transformed function
(define (average-dump f)
  (lambda (x) (average x (f x))))

; Returns value of x raised to the power of n.
;
; Current implementation works only for integer non-negative
; value of base and exponent.
;
; Parameters:
; x - base, integer
; n - exponent, integer
;
; Returns:
; value x^n
(define (pow x n)
  (cond ((= n 0) 1)
        (else (* x (pow x (dec n))))))

(define (log2 x)
  (/ (log x) (log 2)))

; Calculates n-th root of a number x.
;
; 
(define (root x n)
  (fixed-point
   ((repeated average-dump n)
    (lambda (y) (/ x (pow y (- n 1)))))
   1.0))
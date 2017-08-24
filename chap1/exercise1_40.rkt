#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Infinitesimal used to differentiate functions.
;
; Values was chosen as infinitesimal to expected
; point values to calculate differentiate - like
; 5, 10, 1000, ...
(define dx 0.00001)

; Differentiates specified function.
;
; Parameters:
; g - function to be differentiated
;
; Return:
; differentiated function
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

; Transforms specified function to the function that has fixed-point
; in the same point as original function has value 0.
;
; It is used in newton-method to find zero of a function.
;
; Parameters:
; g - function to transform
;
; Returns:
; transformed function
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

; Calculates zero of a specified function using newton method.
;
; Parameters:
; g - function to calculate zero of a function
; guess - first guess of a zero of a function
;
; Returns:
; zero of a function
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

; Creates cubic polynomial.
;
; Cubic polynomial has the following form:
;     f(x) = x^3 + a*x^2 + b*x + c
;
; Parameters:
; a, b, c - coefficients of polynomial
;
; Returns:
; cubic polynomial
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(newton-method (cubic 1 1 1) 1)

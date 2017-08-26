#lang sicp

; Creates procedure that solves numeric problem using iterative improvement.
;
; Iterative improvement is a popular technique in numerical analysis
; that consists of the following steps: checking if current guess is
; good enough and if it is not, improving a guess and repeating this
; cycle.
;
; Parameters:
; good-enough? - unary function checking if current guess is good enough
; improve-solution - unary function that calculate new guess from current
;                    guess minimizing error
;
; Returns:
; unary function of first guess of the solution
(define (iterative-improve good-enough? improve-solution)
  (define (iter-impr-rec x)
    (if (good-enough? x)
        x
        (iter-impr-rec (improve-solution x))))
  (lambda (first-guess) (iter-impr-rec first-guess)))

; Calculates average value of x and y.
;
; Parameters:
; x, y - calculate average for these values
;
; Returns:
; average value of x and y
(define (average x y)
  (/ (+ x y) 2))

; Computes square of argument.
;
; Parameters:
; x - argument to square
;
; Returns:
; squared argument
(define (square x)
  (* x x))

; Calculate square root of a number.
;
; Parameters:
; x - number to calculate square root
;
; Returns:
; square root of x
(define (sqrt x)
  (define (is-good-enough? guess)
    (< (/ (abs (- (square guess) x)) guess) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve is-good-enough? improve) 1.0))

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
  (define (close-enough? guess)
    (< (abs (- guess (improve-solution guess))) tolerance))
  (define (improve-solution guess)
    (f guess))
  ((iterative-improve close-enough? improve-solution) first-guess))

(define (sqrt-fixed-point x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))



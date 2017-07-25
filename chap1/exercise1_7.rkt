#lang sicp

(define (sqrt-iter guess x)
  (if (is-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (is-good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; original version result
; (sqrt 0.000001) = 0.031260655525445276 -> proper result = 0.001
; (sqrt 9999999999999) = 3162277.660168221 -> proper result = 3162277.66017

(define (sqrt-iter-impr guess x)
  (if (is-good-enough-impr? guess x)
      guess
      (sqrt-iter-impr (improve guess x) x)))

(define (is-good-enough-impr? guess x)
  (< (/ (abs (- (square guess) x)) guess) 0.001))

(define (sqrt-impr x)
  (sqrt-iter-impr 1.0 x))

; improved version result
; (sqrt 0.000001) = 0.0012961915927068783 -> proper result = 0.001
; (sqrt 9999999999999) = 3162277.660168221 -> proper result = 3162277.66017

(sqrt-impr 9999999999999)


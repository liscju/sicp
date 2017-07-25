#lang sicp

(define (sqrt3-iter guess x)
  (if (is-good-enough? guess x)
      guess
      (sqrt3-iter (improve guess x) x)))

(define (improve guess x)
  (average (/ x (square guess)) (* 2 guess))) 

(define (average x y)
  (/ (+ x y) 3))

(define (is-good-enough? guess x)
  (< (abs (- (cube guess) x)) (* guess 0.001))) 

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (sqrt3 x)
  (sqrt3-iter 1.0 x))



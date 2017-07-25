#lang sicp

(define (new-if predicate next alternative)
  (cond (predicate next)
        (else alternative)))

(define (sqrt-iter guess x)
  (new-if (is-good-enough? guess x)
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

(sqrt 9)

; program hangs
; new-if demands that predicate, next and alternative
; is calculated before performing "new-if". In sqrt-iter
; in alternative there is sqrt-iter invocation that makes
; program run in unlimited loop



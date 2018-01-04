#lang sicp
(#%require (only racket/base random))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (integral-experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (* (monte-carlo trials integral-experiment) (- x2 x1) (- y2 y1)))

(define (is-inside-unit-circle? x y)
  (<= (+ (* x x) (* y y)) 1))

(estimate-integral is-inside-unit-circle? (- 1) 1 (- 1) 1 10000000)
; 3.1403748
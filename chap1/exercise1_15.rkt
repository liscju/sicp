#lang sicp

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3)))))

; (sine 12.15) executes p 5 times
; time is O(log3(10*N))
; space is O(log3(10*N)

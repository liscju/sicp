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

; Golden ratio satisfy:
; f^2 = f + 1 // f
; f = 1 + 1/f
; so we are looking for fixed-point for: 1 + 1/f

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)

; expected f = 1.6180
; actual f = 1.61803
#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "current guess: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "Basic version")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
(newline)
(display "Averaged damping")
(newline)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)

; Basic version count of steps = 33
; Averaged damping count of steps = 9

#lang sicp

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

; New expmod proposal:
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))
; For big number it can overflow a numeric range



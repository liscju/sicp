#lang sicp

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even?  exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

; f(1) = 1
; f(n) = 2*f(n/2)

; n  |  f(n) |
;------------|
; 1  |  1    |
; 2  |  2    |
; 4  |  4    |
; 8  |  8    |

; Suppose f(n) = n
; Lets proof it:

; Induction base:
; f(n) = n <-> f(1) = 1
; OK!

; Induction step:
; f(n) = 2 * f(n/2) = 2 * n/2 = n
; f(n) = n
; n = n
; OK!

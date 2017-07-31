#lang sicp

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (is-prime? n)
  (fermat-test-iter 2 n))

(define (fermat-test-iter i n)
  (cond ((= i n) true)
        (else
         (if (not (= (expmod i n n) i))
             false
             (fermat-test-iter (inc i) n)))))

; Carmichael test

; 561 = 3 * 11 * 17
; (is-prime? 561) -> #t

; 1105 = 5 * 13 * 17
; (is-prime? 1105) -> #t

; 1729 = 7 * 13 * 19
; (is-prime? 1729) -> #t
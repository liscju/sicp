#lang sicp

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-if-special (remainder (square (expmod base (/ exp 2) m)) m) base (/ exp 2)))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (check-if-special base2expsq base exp)
  (cond ((= base 1) base2expsq)
        ((= base exp) base2expsq)
        ((= base2expsq 1) 0)
        (else base2expsq)))

(define (add-special i)
  (if (= i 0)
      1
      0))

(define (miler-rabin i n)
  (cond ((= i n) 0)
        (+ (add-special (expmod i (dec n) n)) (miler-rabin (inc i) n))))

(define (is-prime? n)
  (cond ((= (remainder n 2) 2) false)
        (else (>= (miler-rabin 2 n) (/ (dec n) 2)))))
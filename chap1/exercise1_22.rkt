#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square n)
  (* n n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes begin end)
  (if (even? begin)
      (search-for-primes-iter (inc begin) end)
      (search-for-primes-iter begin end)))

(define (search-for-primes-iter begin end)
  (if (< begin end)
      (search-for-primes-iter-impl begin end)))

(define (search-for-primes-iter-impl begin end)
  (timed-prime-test begin)
  (search-for-primes-iter (+ 2 begin) end))

(search-for-primes 1000000000 1000000021)       ; 1e9
; time ~ 3000
(search-for-primes 10000000000 10000000061)     ; 1e10
; time ~ 30020
(search-for-primes 100000000000 100000000057)   ; 1e11
; time ~ 
(search-for-primes 1000000000000 1000000000063) ; 1e12
; time ~ 80000
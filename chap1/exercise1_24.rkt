#lang sicp

(#%require (only racket/base random))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (remainder (- n 1) 4294967086)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square n)
  (* n n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor divisor)
  (if (= divisor 2)
      3
      (+ 2 divisor)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (fast-prime? n 100))

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
; old time ~ 2000
; new time ~ 1001
(search-for-primes 10000000000 10000000061)     ; 1e10
; old time ~ 19000
; new time ~ 1001
(search-for-primes 100000000000 100000000057)   ; 1e11
; time ~ 2001
(search-for-primes 1000000000000 1000000000063) ; 1e12
; old time ~ 55000
; new time ~ 15010

; Old
; 1000000007 *** 2002
; 1000000009 *** 2002
; 10000000019 *** 5003
; 10000000033 *** 7023
; 100000000003 *** 19014
; 100000000019 *** 19011
; 1000000000039 *** 59042
; 1000000000061 *** 55039

; New
; 1000000007 *** 1001
; 1000000009 *** 0
; 10000000019 *** 2004
; 10000000033 *** 1002
; 100000000003 *** 3002
; 100000000019 *** 2001
; 1000000000039 *** 2001
; 1000000000061 *** 15010

; Time remains nearly constant because of the nature of fast-prime
; test with constant number of times
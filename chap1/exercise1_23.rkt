#lang sicp

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
; old time ~ 3000
; new time ~ 2000
(search-for-primes 10000000000 10000000061)     ; 1e10
; old time ~ 30020
; new time ~ 19000
(search-for-primes 100000000000 100000000057)   ; 1e11
; time ~ 
(search-for-primes 1000000000000 1000000000063) ; 1e12
; old time ~ 80000
; new time ~ 55000

; Old
; 1000000007 *** 2001
; 1000000009 *** 3002
; 10000000019 *** 9007
; 10000000033 *** 9006
; 100000000003 *** 30020
; 100000000019 *** 30525
; 1000000000039 *** 88133
; 1000000000061 *** 81058

; New
; 1000000007 *** 2002
; 1000000009 *** 2002
; 10000000019 *** 5003
; 10000000033 *** 7023
; 100000000003 *** 19014
; 100000000019 *** 19011
; 1000000000039 *** 59042
; 1000000000061 *** 55039
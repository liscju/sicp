#lang sicp

; ------------------------------Accumulate-----------------------------


; Combines results of computations over elements in range of values only
; for elements that satisfy specified predicate in recursive process.
;
; Range of values is defined by start 'a' and end 'b'. Calculating
; consecutive element is done by specified 'next' function. Calculating
; result for element in range is done by 'term' function. Results
; of computations are joined using 'combiner' function. Predicate is
; passed in 'predicate' argument. Result of computation over empty range
; equals 'null-value'.
;
; Parameters:
; combiner - binary function joining result of calculations
; null-value - result of computation over empty range
; term - unary function calculating value in specified element
; a - start element
; next - unary function calculates consecutive element for specified element
; b - end element
;
; Returns:
; Combined result of computation
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((not (filter a))
         (filtered-accumulate filter
                              combiner
                              null-value
                              term
                              (next a)
                              next
                              b))
        (else (combiner (term a)
                        (filtered-accumulate filter
                                             combiner
                                             null-value
                                             term
                                             (next a)
                                             next
                                             b)))))


; --------------------------------Utils-------------------------------


; Identity function.
;
; Returns specified argument.
;
; Parameters:
; x - argument to return
;
; Returns:
; specified argument
(define (id x)
  x)

; Returns true for any argument.
;
; Parameters:
; x - argument
;
; Returns:
; true for any argument
(define (any-true x)
  #t)

; Returns false for any argument.
;
; Parameters:
; x - argument
;
; Returns:
; false for any argunet
(define (any-false x)
  #f)

; Calculates greatest common divisor of given numbers.
;
; The greatest common divisor (gcd) of two integers a and b is defined
; to be the largest integer that divides both a and b with no remainder.
;
; Parameters:
; a - first number
; b - second number
;
; Returns:
; greatest common divisor of 'a' and 'b'
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Checks if two numbers are coprime integers.
;
; Parameters:
; a - first number
; b - second number
;
; Returns:
; true if 'a' and 'b' is coprime integers, false otherwise
(define (are-coprimes? a b)
  (= (gcd a b) 1))


; --------------------------------Primes-------------------------------


; Finds smallest divisor for specified number.
;
; Parameters:
; n - positive number to find smallest divisor
;
; Returns:
; smallest divisor of the number
(define (smallest-divisor n)
  (find-divisor n 2))

; Compute square of a given number
;
; Square is defined as multiplication of a number by itselve.
;
; Parameters:
; n - number to compute square
;
; Returns:
; square of a number
(define (square n)
  (* n n))

; Finds smallest divisor for a number larger or equals specified argument.
;
; Parameters:
; n - number to calculate smallest divisor
; test-divisor - divisor found must be greater than this number
;
; Returns:
; smallest divisor for a number greater or equal specified number
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

; Checks if numer is divided by given argument.
;
; Function is defined only for positive numbers, for negative return
; value is unspecified.
;
; Parameters:
; a - number to check if it is divided by argument 'b'
; b - number to divides 'a'
;
; Returns:
; true when 'a' divides 'b', false otherwise
(define (divides? a b)
  (= (remainder b a) 0))

; Checks if number is prime.
;
; Parameters:
; n - number to check primality
;
; Returns:
; true when number is prime, false otherwise
(define (prime? n)
  (= n (smallest-divisor n)))


; --------------------------------Exercises-------------------------------


; Calculates sum of prime squared in range <2, n>.
;
; Parameters:
; n - last element in range
;
; Returns:
; sum of primes squared in range
(define (sum-of-prime-squared n)
  (filtered-accumulate prime? + 0 square 2 inc n))

; Computes product of coprime integers to specified positive integer.
;
; Product consists of positive integer less than 'n' that are coprimes
; integers to 'n'.
;
; Parameters:
; n - positive number to calculate product
(define (product-of-coprime-ints n)
  (define (is-coprime-to-n? x)
    (are-coprimes? x n))
  (filtered-accumulate is-coprime-to-n? * 1 id 2 inc (dec n)))
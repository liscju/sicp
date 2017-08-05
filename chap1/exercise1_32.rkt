#lang sicp

; Combines results of computations over elements in range of values in
; recursive process.
;
; Range of values is defined by start 'a' and end 'b'. Calculating
; consecutive element is done by specified 'next' function. Calculating
; result for element in range is done by 'term' function. Results
; of computations are joined using 'combiner' function. Result of computation
; over empty range equals 'null-value'.
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
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; Combines results of computations over elements in range of values in
; iterative process.
;
; Range of values is defined by start 'a' and end 'b'. Calculating
; consecutive element is done by specified 'next' function. Calculating
; result for element in range is done by 'term' function. Results
; of computations are joined using 'combiner' function. Result of computation
; over empty range equals 'null-value'.
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
(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner
                       (combiner (term a) null-value)
                       term
                       (next a)
                       next
                       b)))

; Identity function.
;
; Returns element provided in argument.
;
; Parameters:
; x - argument to return
; Returns:
; provided argument
(define (id x)
  x)

; Sums results of computation for given range in recursive process.
;
;
; Parameters:
; term - unary function calculating value for given element in range
; a - start of range
; next - unary function calculates consecutive element for given
; b - end of range
;
; Returns:
; Sum of results of computation of values over given range
(define (sum term a next b)
  (accumulate + 0 term a next b))

; Sums results of computation for given range in iterative process.
;
;
; Parameters:
; term - unary function calculating value for given element in range
; a - start of range
; next - unary function calculates consecutive element for given
; b - end of range
;
; Returns:
; Sum of results of computation of values over given range
(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

; Multiplies results of computation for given range in recursive process.
;
;
; Parameters:
; term - unary function calculating value for given element in range
; a - start of range
; next - unary function calculates consecutive element for given
; b - end of range
;
; Returns:
; Multiply of results of computation of values over given range
(define (product term a next b)
  (accumulate * 1 term a next b))

; Multiplies results of computation for given range in recursive process.
;
;
; Parameters:
; term - unary function calculating value for given element in range
; a - start of range
; next - unary function calculates consecutive element for given
; b - end of range
;
; Returns:
; Multiply of results of computation of values over given range
(define (product-iter term a next b)
  (accumulate * 1 term a next b))
#lang sicp

(define (id x)
  x)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (product-iter-helper a result)
    (if (> a b)
        result
        (product-iter-helper (next a) (* result (term a)))))
  (product-iter-helper a 1))

(define (inc2 x) (inc (inc x)))

(define (pi-term x) (/ (* x (inc2 x)) (* (inc x) (inc x))))

(define (pi)
  (* 4 (product pi-term 2 inc2 100)))

(define (pi-iter)
  (* 4 (product-iter pi-term 2 inc2 100)))

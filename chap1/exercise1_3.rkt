#lang sicp

(define
  (square x)
  (* x x))

(define
  (sum-of-bigger-squares a b c)
  (max (+ (square a) (square b))
       (+ (square b) (square c))
       (+ (square a) (square c))))

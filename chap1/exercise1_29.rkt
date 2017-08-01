#lang sicp

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (h a b n)
  (/ (- b a) n))

(define (coeff a b n)
  (/ (h a b n) 3))

(define (simpson f a b n)
  (define (f-iter k)
    (if (= (remainder k 2) 1)
        (* (coeff a b n) (* 4 (f (+ a (* k (h a b n))))))
        (* (coeff a b n) (* 2 (f (+ a (* k (h a b n))))))))
  (+ (* (coeff a b n) (f a))
     (sum f-iter 1 inc (- n 1))
     (* (coeff a b n) (f (+ a (* n (h a b n)))))))

; (integral cube 0 1 0.01) -> 0.24998750000000042
; (simpson cube 0 1 100) -> 1/4

; (integral cube 0 1 0.001) -> 0.249999875000001
; (simpson cube 0 1 1000) -> 1/4
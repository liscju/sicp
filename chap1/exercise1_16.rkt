#lang sicp

(define (square x)
  (* x x))

;(define (fast-expt b n)
;  (cond ((= n 0) 1)
;        ((even? n) (square (fast-expt b (/ n 2))))
;        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt b n)
  (fast-expt-iter b 1 n))

(define (fast-expt-iter b a n)
  (cond ((= n 0) a)
        ((odd? n) (fast-expt-iter b (* b a) (dec n)))
        (else (fast-expt-iter (square b) a (/ n 2)))))

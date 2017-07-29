#lang sicp

(define (double x)
  (* 2 x))

(define (halve x)
  (floor (/ x 2)))

;(define (* a b)
;  (if (= b 0)
;      a
;      (+ a (* a (- b 1)))))

(define (fast-mul a b)
  (fast-mul-iter a b 0))

; keeps equation: a*b + c = const
(define (fast-mul-iter a b c)
  (cond ((= b 0) c)
        ((odd? b) (fast-mul-iter a (- b 1) (+ c a)))
        ((even? b) (fast-mul-iter (double a) (halve b) c))))
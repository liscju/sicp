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
  (cond ((= b 1) a)
        ((odd? b) (+ a (fast-mul a (- b 1))))
        ((even? b) (fast-mul (double a) (halve b)))))
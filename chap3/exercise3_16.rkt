#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (cons 'a (cons 'b (cons 'c nil)))) ; =3

(define c (cons 'c nil))
(count-pairs (cons c (cons 'b c))) ; =4

(define b (cons c c))
(count-pairs (cons b b)) ; =7

(define infinite (cons 'a (cons 'b (cons 'c nil))))
(set-cdr! (cddr infinite) infinite)
(count-pairs infinite) ; =infinite time

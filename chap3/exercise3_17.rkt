#lang sicp

(define (contains? elem list)
  (cond
    ((null? list) #f)
    ((eq? elem (car list)) #t)
    (else (contains? elem (cdr list)))))

(define (count-pairs x)
  (define walked-pairs '())
  (define (count-pairs-walk x)
     (cond ((not (pair? x)) 0)
           ((contains? x walked-pairs) 0)
           (else
            (begin
              (set! walked-pairs (cons x walked-pairs))
              (+ (count-pairs-walk (car x))
                    (count-pairs-walk (cdr x))
                    1)))))
  (count-pairs-walk x))

(define infinite (cons 'a (cons 'b (cons 'c nil))))
(set-cdr! (cddr infinite) infinite)
(count-pairs infinite)
; 3

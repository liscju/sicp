#lang sicp

(define (contains? elem list)
  (cond
    ((null? list) #f)
    ((eq? elem (car list)) #t)
    (else (contains? elem (cdr list)))))

(define (has-cycle? l)
  (define walked-pairs '())
  (define (search-for-cycle l)
    (cond ((null? l) #f)
          ((contains? l walked-pairs) #t)
          (else
           (begin (set! walked-pairs (cons l walked-pairs))
                  (search-for-cycle (cdr l))))))
  (search-for-cycle l))


(define list-with-cycle (cons 'a (cons 'b (cons 'c nil))))
(set-cdr! (cddr list-with-cycle) list-with-cycle)
(has-cycle? list-with-cycle) ; #t

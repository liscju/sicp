#lang sicp
(#%require (only racket/stream
                 stream-cons
                 stream-first
                 stream-rest
                 empty-stream
                 stream-empty?
                 stream
                 for/stream
                 stream-map
                 stream-ref
                 stream-filter
                 stream-for-each))

(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)
(define (stream-enumerate-interval start end)
  (if (> start end)
      empty-stream
      (stream-cons start (stream-enumerate-interval (inc start) end))))
(define (display-stream s) (stream-for-each display s))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define ones (stream-cons 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (stream-cons 1 (add-streams ones integers)))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (partial-sums s)
  (define partial (stream-cons (stream-car s) (add-streams (stream-cdr s) partial)))
  partial)
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
         ((stream-null? s2) s1)
         (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (stream-cons s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (stream-cons s2car (merge s1 (stream-cdr s2))))
                  (else
                   (stream-cons s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))

; exercise

(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

; O(fib(n)) = n
; Without memoizing each fib would require each term to be recomputed,
; so calculating fib(n) would require at least fib(n) additions which
; is exponential
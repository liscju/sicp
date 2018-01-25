#lang sicp
(#%require (only racket/promise delay force))
(#%require (only racket/stream
                 stream-cons
                 stream-first
                 stream-rest
                 empty-stream
                 stream-empty?
                 stream))

(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)

; (A1 B1 C1 D1) (A-STREAM B-STREAM C-STREAM D-STREAM)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define s1 (stream 1 2 3 4 5 empty-stream))
(define s2 (stream 4 5 6 6 7 empty-stream))

(define s (stream-map (lambda (x y) (+ x y)) s1 s2))


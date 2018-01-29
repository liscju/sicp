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
(define (add-streams s1 s2)
  (stream-map + s1 s2))

; exercise

(define s (stream-cons 1 (add-streams s s)))
; 1 2 4 8 16 32 64 128 ..









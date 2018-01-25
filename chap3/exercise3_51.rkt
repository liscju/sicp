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
                 stream-ref))

(define the-empty-stream empty-stream)
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)
(define (stream-enumerate-interval start end)
  (if (> start end)
      empty-stream
      (stream-cons start (stream-enumerate-interval (inc start) end))))

(define (show x)
  (display x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
; 5

(stream-ref x 7)
; 7


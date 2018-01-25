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

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display sum)
(newline)
; 0

(define y (stream-filter even? seq))
(display sum)
(newline)
; 0

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(display sum)
(newline)
; 0

(stream-ref y 7)
(newline)
(display sum)
(newline)
; 136 becacuse to get eithh even value it calculates first 16 (1+2+..+16)

;(display-stream z)
;(newline)
;(display sum)
;(newline)
; 210 because calculates all values in stream (1+2+3..+20)
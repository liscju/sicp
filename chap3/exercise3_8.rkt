#lang sicp

(define result 1)

(define (f x)
  (begin (set! result (* result x))
         result))

; (+ (f 0) (f 1))
; gives 0 when arguments calculated from left to right
; gives 1 when arguments calculated from right to left
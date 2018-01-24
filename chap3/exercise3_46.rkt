#lang sicp

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             (false))))

; Suppose P1 and P2 do (test-and-set! (list false))
; P1 and P2 checks simaltenously (if (car cell))
; they both get false, both set cell to true
; and acquire mutex

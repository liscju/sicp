#lang sicp

(define x 10)

(define s (make-serializer))

(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

; P1 assigns x 100, P2 increments x to 101
; P2 increments x to 11, P1 assigns x 121
; P1 gets x value, P2 assign x 11, P1 sets x 100
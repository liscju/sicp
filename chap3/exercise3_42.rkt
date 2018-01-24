#lang sicp
(#%require (only racket/base error))
(#%require (only racket/base thread-wait thread))


(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))


(define x 10)
;(parallel-execute
; (lambda () (set! x (* x x)))
; (lambda () (set! x (+ x 1))))

(parallel-execute (lambda () (display "A")) (lambda() (display "B")))
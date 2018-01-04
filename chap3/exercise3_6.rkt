#lang sicp

(define random-init 1)
(define (rand-update x)
  (remainder (* 7 x) 101))

(define rand
  (let ((x random-init))
    (begin
      (define (generate)
        (set! x (rand-update x))
        x)
      (define (reset new-value)
        (set! x new-value))
      (define (dispatch cmd)
        (cond ((eq? cmd 'generate)
               (generate))
              ((eq? cmd 'reset)
               (lambda (new-value) (reset new-value)))
              (else (error "Unrecognized command"))))
      dispatch)))
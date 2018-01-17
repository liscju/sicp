#lang sicp

(define (logical-and x y)
  (and x y))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; If multiple events trigger an output event then:
; if a1 change from 0 -> 1 and a2 change from 1 -> 0,
; to the same time segment are put two procedures:
; (lambda () (set-signal! output 1) and
; (lambda () (set-signal! output 0))
; If we proceed to take them in LIFO order from queue
; then the final output will be 1 which is not correct
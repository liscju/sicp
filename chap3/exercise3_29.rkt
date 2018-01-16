#lang sicp

(define inverter-delay 200)
(define or-gate-delay 500)
(define and-gate-delay 1000)

(define (logical-or x y)
  (or x y))

(define (logical-and x y)
  (and x y))

(define (logical-not x)
  (if (= x 0) 1 0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay))))

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

(define (or-gate a1 a2 output)
  (let ((inner1 (make-wire))
        (inner2 (make-wire))
        (inner3 (make-wire)))
    (inverter a1 inner1)
    (inverter a2 inner2)
    (and-gate inner1 inner2 inner3)
    (inverter inner3 output)
    'ok))

; delay(or-gate) = delay(inverter) + delay(and-gate) + delay(inverter)
